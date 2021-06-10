module XmlUnscrambler
  ( -- * Execution
    parseElement,
    Error (..),
    Location (..),
    Reason (..),
    NodeType (..),

    -- * Parsers by context

    -- ** Element
    Element,
    elementName,
    elementNameIs,
    children,
    childrenByName,
    attributesByName,

    -- ** Nodes
    Nodes,
    elementNode,
    textNode,

    -- ** ByName
    ByName,
    byName,
  )
where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.HashMap.Strict as HashMap
import qualified Text.XML as Xml
import qualified XmlUnscrambler.NameMap as NameMap
import XmlUnscrambler.Prelude

-- |
-- Parse an \"xml-conduit\" element AST.
parseElement :: Element a -> Xml.Element -> Either Error a
parseElement (Element run) = run

-- |
-- Parsing error.
data Error = Error [Location] Reason

data Location
  = ByNameLocation (Maybe Text) Text
  | AtOffsetLocation Int
  | ChildrenLocation
  | AttributesLocation

-- |
-- Reason of an error.
data Reason
  = AttoparsecFailedReason Text
  | UnexpectedNodeTypeReason
      NodeType
      -- ^ Actual.
      NodeType
      -- ^ Expected.
  | NoNodesLeftReason
  | NoneOfChildrenFoundByNameReason [(Maybe Text, Text)]

data NodeType
  = ElementNodeType
  | InstructionNodeType
  | ContentNodeType
  | CommentNodeType

-- |
-- Parse in the context of an element node.
newtype Element a
  = Element (Xml.Element -> Either Error a)
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT Xml.Element (Except Error))

-- |
-- Parse namespace and name with the given function.
elementName :: (Maybe Text -> Text -> Either Text a) -> Element a
elementName =
  error "TODO"

-- |
-- Fail if the namespace and name don't match the provided.
elementNameIs :: Maybe Text -> Text -> Element ()
elementNameIs =
  error "TODO"

-- |
-- Look up the first element by name and parse it.
childrenByName :: ByName Element a -> Element a
childrenByName =
  \(ByName runByName) -> Element $ \(Xml.Element _ _ nodes) ->
    case runByName (NameMap.fromNodes nodes) parse of
      OkByNameResult _ res -> Right res
      NotFoundByNameResult unfoundNames ->
        Left (Error [] (NoneOfChildrenFoundByNameReason unfoundNames))
      FailedDeeperByNameResult ns name err ->
        Left (consLocationToError (ByNameLocation ns name) err)
  where
    parse element (Element run) = run element

-- |
-- Look up the first attribute by name and parse it.
attributesByName :: ByName Atto.Parser a -> Element a
attributesByName =
  error "TODO"

-- |
-- Children sequence by order.
children :: Nodes a -> Element a
children (Nodes runNodes) =
  Element $ \(Xml.Element _ _ nodes) ->
    runNodes (nodes, 0) & fst & first (consLocationToError ChildrenLocation)

-- |
-- Parser in the context of a sequence of nodes.
newtype Nodes a
  = Nodes (([Xml.Node], Int) -> (Either Error a, ([Xml.Node], Int)))
  deriving
    (Functor, Applicative, Monad)
    via (ExceptT Error (State ([Xml.Node], Int)))

-- |
-- Consume the next node expecting it to be element.
elementNode :: Element a -> Nodes a
elementNode (Element runElement) =
  Nodes $ \(nodes, offset) ->
    case nodes of
      node : nodes -> case node of
        Xml.NodeElement element ->
          ( first
              (consLocationToError (AtOffsetLocation offset))
              (runElement element),
            (nodes, succ offset)
          )
        Xml.NodeInstruction _ -> failWithUnexpectedNodeType InstructionNodeType
        Xml.NodeContent _ -> failWithUnexpectedNodeType ContentNodeType
        Xml.NodeComment _ -> failWithUnexpectedNodeType CommentNodeType
        where
          failWithUnexpectedNodeType actualType =
            ( Left (Error [AtOffsetLocation offset] (UnexpectedNodeTypeReason ElementNodeType actualType)),
              (nodes, succ offset)
            )
      _ ->
        ( Left (Error [AtOffsetLocation offset] NoNodesLeftReason),
          ([], succ offset)
        )

-- |
-- Consume the next node expecting it to be textual and parse its contents with \"attoparsec\".
textNode :: Atto.Parser a -> Nodes a
textNode =
  error "TODO"

-- * ByName

data ByNameResult content a
  = NotFoundByNameResult [(Maybe Text, Text)]
  | FailedDeeperByNameResult (Maybe Text) Text Error
  | OkByNameResult (NameMap.NameMap content) a
  deriving (Functor)

-- |
-- Composable extension to a parser, which looks up its input by name.
--
-- Useful for searching elements and attributes by name.
--
-- Alternative and MonadPlus alternate only on lookup errors.
-- When lookup is successful, but the deeper parser fails,
-- the error propagates.
--
-- Monad and Applicative sequentially fetch contents by matching names.
newtype ByName parser a
  = ByName
      ( forall content.
        NameMap.NameMap content ->
        (content -> forall x. parser x -> Either Error x) ->
        ByNameResult content a
      )

instance Functor (ByName parser) where
  fmap fn (ByName run) =
    ByName $ \map parse -> fmap fn (run map parse)

instance Applicative (ByName parser) where
  pure x =
    ByName $ \map _ -> OkByNameResult map x
  ByName runL <*> ByName runR =
    ByName $ \map parse -> case runL map parse of
      OkByNameResult map lRes -> runR map parse & fmap lRes
      NotFoundByNameResult unfoundNames -> NotFoundByNameResult unfoundNames
      FailedDeeperByNameResult ns name err -> FailedDeeperByNameResult ns name err

instance Monad (ByName parser) where
  return = pure
  ByName runL >>= k =
    ByName $ \map parse -> case runL map parse of
      OkByNameResult map lRes -> case k lRes of ByName runR -> runR map parse
      NotFoundByNameResult unfoundNames -> NotFoundByNameResult unfoundNames
      FailedDeeperByNameResult ns name err -> FailedDeeperByNameResult ns name err

instance Alternative (ByName parser) where
  empty =
    ByName $ const $ const $ NotFoundByNameResult []
  ByName runL <|> ByName runR =
    ByName $ \map parse -> case runL map parse of
      OkByNameResult map lRes -> OkByNameResult map lRes
      NotFoundByNameResult unfoundNamesL -> case runR map parse of
        NotFoundByNameResult unfoundNamesR -> NotFoundByNameResult (unfoundNamesL <> unfoundNamesR)
        resR -> resR
      FailedDeeperByNameResult ns name err -> FailedDeeperByNameResult ns name err

instance MonadPlus (ByName parser) where
  mzero = empty
  mplus = (<|>)

-- |
-- Execute a parser on the result of looking up a content by namespace and name.
byName :: Maybe Text -> Text -> parser a -> ByName parser a
byName ns name parser =
  ByName $ \map parse ->
    case NameMap.fetch ns name map of
      Just (content, map) -> case parse content parser of
        Right a -> OkByNameResult map a
        Left err -> FailedDeeperByNameResult ns name err
      Nothing -> NotFoundByNameResult [(ns, name)]

-- * Utils

consLocationToError :: Location -> Error -> Error
consLocationToError loc (Error path reason) = Error (loc : path) reason
