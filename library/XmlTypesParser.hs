module XmlTypesParser
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
import qualified Data.XML.Types as Xml
import XmlTypesParser.Prelude

parseElement :: Element a -> Xml.Element -> Either Error a
parseElement (Element run) = run

data Error = Error [Location] Reason

data Location
  = ByNameLocation (Maybe Text) Text
  | AtOffsetLocation Int
  | ChildrenLocation
  | AttributesLocation

data Reason
  = NameNotFoundReason (Maybe Text) Text
  | AttoparsecFailedReason Text
  | NoReason
  | UnexpectedNodeTypeReason
      NodeType
      -- ^ Actual.
      NodeType
      -- ^ Expected.
  | NoNodesLeftReason

data NodeType
  = ElementNodeType
  | InstructionNodeType
  | ContentNodeType
  | CommentNodeType

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
-- Look up the first element by name and parse it.
childrenByName :: ByName Element a -> Element a
childrenByName =
  \(ByName runByName) -> Element $ \element ->
    let lookup = buildByNameLookup (nodesToElementsByName (Xml.elementNodes element))
     in runByName lookup exec
  where
    exec element (Element run) = run element

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

textNode :: Atto.Parser a -> Nodes a
textNode =
  error "TODO"

newtype ByName parser a
  = ByName
      ( forall content.
        (Maybe Text -> Text -> Maybe content) ->
        (forall a. content -> parser a -> Either Error a) ->
        Either Error a
      )

instance Functor (ByName parser) where
  fmap fn (ByName run) =
    ByName $ \lookup exec -> fmap fn $ run lookup exec

instance Applicative (ByName parser) where
  pure x =
    ByName $ const $ const $ Right x
  ByName runL <*> ByName runR =
    ByName $ \lookup exec -> case runL lookup exec of
      Left error -> Left error
      Right lRes -> runR lookup exec & fmap lRes

instance Monad (ByName parser) where
  return = pure
  ByName runL >>= k =
    ByName $ \lookup exec -> case runL lookup exec of
      Left error -> Left error
      Right lRes -> case k lRes of ByName runR -> runR lookup exec

instance Alternative (ByName parser) where
  empty =
    ByName $ const $ const $ Left $ Error [] NoReason
  ByName runL <|> ByName runR =
    ByName $ \lookup exec -> case runL lookup exec of
      Left error -> runR lookup exec
      Right lRes -> Right lRes

instance MonadPlus (ByName parser) where
  mzero = empty
  mplus = (<|>)

byName :: Maybe Text -> Text -> parser a -> ByName parser a
byName ns name parser =
  ByName $ \lookup exec -> case lookup ns name of
    Just content -> case exec content parser of
      Right a -> Right a
      Left (Error path reason) -> Left (Error (ByNameLocation ns name : path) reason)
    Nothing -> Left (Error [] (NameNotFoundReason ns name))

{-# INLINE buildByNameLookup #-}
buildByNameLookup :: Foldable f => f (Xml.Name, a) -> Maybe Text -> Text -> Maybe a
buildByNameLookup list =
  foldr step lookup list HashMap.empty HashMap.empty
  where
    step (Xml.Name name ns _, contents) next !map1 !map2 =
      case ns of
        Just ns ->
          next
            ( HashMap.alter
                ( maybe
                    (Just (HashMap.singleton name contents))
                    (Just . HashMap.insert name contents)
                )
                ns
                map1
            )
            map2
        Nothing ->
          next map1 (HashMap.insert name contents map2)
    lookup map1 map2 ns name =
      case ns of
        Just ns -> HashMap.lookup ns map1 >>= HashMap.lookup name
        Nothing -> HashMap.lookup name map2

{-# INLINE nodesToElementsByName #-}
nodesToElementsByName :: [Xml.Node] -> [(Xml.Name, Xml.Element)]
nodesToElementsByName =
  mapMaybe $ \case
    Xml.NodeElement element -> Just (Xml.elementName element, element)
    _ -> Nothing

consLocationToError :: Location -> Error -> Error
consLocationToError loc (Error path reason) = Error (loc : path) reason
