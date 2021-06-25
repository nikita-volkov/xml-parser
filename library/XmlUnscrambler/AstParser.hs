module XmlUnscrambler.AstParser
  ( -- * Execution
    parseElement,
    renderError,
    Error (..),
    Location (..),
    Reason (..),
    ContentError (..),
    NodeType (..),

    -- * Parsers by context

    -- ** Element
    Element,
    elementName,
    elementNameIs,
    children,
    childrenByName,
    attributesByName,

    -- ** Content
    Content,
    textContent,
    attoparsedContent,
    qNameContent,

    -- ** Nodes
    Nodes,
    elementNode,
    textNode,

    -- ** ByName
    ByName,
    byName,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Text.Builder as Tb
import qualified Text.XML as Xml
import qualified XmlUnscrambler.NameMap as NameMap
import qualified XmlUnscrambler.NamespaceRegistry as NamespaceRegistry
import qualified XmlUnscrambler.NodeConsumerState as NodeConsumerState
import XmlUnscrambler.Prelude
import qualified XmlUnscrambler.XmlSchemaAttoparsec as XmlSchemaAttoparsec

-- |
-- Parse an \"xml-conduit\" element AST.
parseElement :: Element a -> Xml.Element -> Either Error a
parseElement (Element run) element =
  run
    (NamespaceRegistry.interpretAttributes (Xml.elementAttributes element) NamespaceRegistry.new)
    element

renderError :: Error -> Text
renderError =
  Tb.run . errorAtPath
  where
    errorAtPath (Error a b) = "Error at path " <> path a <> ". " <> reason b
    path a = "/" <> Tb.intercalate "/" (fmap location a)
    reason = \case
      AttoparsecFailedReason a -> Tb.text a
      UnexpectedNodeTypeReason a b -> "Unexpected node type. Got " <> nodeType a <> ", but expected " <> nodeType b
      NoNodesLeftReason -> "No nodes left"
      NoneOfChildrenFoundByNameReason a b -> "None of following names found: " <> list name a <> ". Names available: " <> list name b
      ContentErrorReason a -> contentError a
    location = \case
      ByNameLocation a b -> name (a, b)
      AtOffsetLocation a -> Tb.decimal a
      ChildrenLocation -> "children"
      AttributesLocation -> "attributes"
    list renderer = mappend "[" . flip mappend "]" . Tb.intercalate ", " . fmap renderer . sort
    nodeType = \case
      ElementNodeType -> "element"
      InstructionNodeType -> "instruction"
      ContentNodeType -> "content"
      CommentNodeType -> "comment"
    contentError = \case
      ParsingContentError a -> Tb.text a
      NamespaceNotFoundContentError a -> "Namespace not found: " <> Tb.text a
    name (a, b) =
      case a of
        Just a -> Tb.text a <> ":" <> Tb.text b
        Nothing -> Tb.text b

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
  | NoneOfChildrenFoundByNameReason [(Maybe Text, Text)] [(Maybe Text, Text)]
  | ContentErrorReason ContentError

data NodeType
  = ElementNodeType
  | InstructionNodeType
  | ContentNodeType
  | CommentNodeType

-- |
-- Parse in the context of an element node.
newtype Element a
  = Element (NamespaceRegistry.NamespaceRegistry -> Xml.Element -> Either Error a)
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT (NamespaceRegistry.NamespaceRegistry) (ReaderT Xml.Element (Except Error)))

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
-- Look up elements by name and parse them.
childrenByName :: ByName Element a -> Element a
childrenByName =
  \(ByName runByName) -> Element $ \nsReg (Xml.Element _ attributes nodes) ->
    let nameMap = NameMap.fromNodes nodes
        newNsReg = NamespaceRegistry.interpretAttributes attributes nsReg
     in case runByName nameMap (\element (Element run) -> run newNsReg element) of
          OkByNameResult _ res -> Right res
          NotFoundByNameResult unfoundNames ->
            let availNames = NameMap.extractNames nameMap
             in Left (Error [] (NoneOfChildrenFoundByNameReason unfoundNames availNames))
          FailedDeeperByNameResult ns name err ->
            Left (consLocationToError (ByNameLocation ns name) err)

-- |
-- Look up the first attribute by name and parse it.
attributesByName :: ByName Content a -> Element a
attributesByName =
  error "TODO"

-- |
-- Children sequence by order.
children :: Nodes a -> Element a
children (Nodes runNodes) =
  Element $ \nsReg (Xml.Element _ _ nodes) ->
    runNodes (NodeConsumerState.new nodes nsReg) & fst & first (consLocationToError ChildrenLocation)

-- |
-- Parser in the context of a sequence of nodes.
newtype Nodes a
  = Nodes (NodeConsumerState.NodeConsumerState -> (Either Error a, NodeConsumerState.NodeConsumerState))
  deriving
    (Functor, Applicative, Monad)
    via (ExceptT Error (State NodeConsumerState.NodeConsumerState))

-- |
-- Consume the next node expecting it to be element.
elementNode :: Element a -> Nodes a
elementNode (Element runElement) =
  Nodes $ \x ->
    case NodeConsumerState.fetchNode x of
      Just (node, x) -> case node of
        Xml.NodeElement element ->
          ( first
              (consLocationToError (AtOffsetLocation (NodeConsumerState.getOffset x)))
              (runElement (NodeConsumerState.getNamespaceRegistry x) element),
            (NodeConsumerState.bumpOffset x)
          )
        Xml.NodeInstruction _ -> failWithUnexpectedNodeType InstructionNodeType
        Xml.NodeContent _ -> failWithUnexpectedNodeType ContentNodeType
        Xml.NodeComment _ -> failWithUnexpectedNodeType CommentNodeType
        where
          failWithUnexpectedNodeType actualType =
            ( Left (Error [AtOffsetLocation (NodeConsumerState.getOffset x)] (UnexpectedNodeTypeReason ElementNodeType actualType)),
              (NodeConsumerState.bumpOffset x)
            )
      _ ->
        ( Left (Error [AtOffsetLocation (NodeConsumerState.getOffset x)] NoNodesLeftReason),
          (NodeConsumerState.bumpOffset x)
        )

-- |
-- Consume the next node expecting it to be textual and parse its contents with \"attoparsec\".
textNode :: Content content -> Nodes content
textNode (Content parseContent) =
  Nodes $ \x ->
    case NodeConsumerState.fetchNode x of
      Just (node, x) -> case node of
        Xml.NodeContent content ->
          case parseContent (\ns -> NodeConsumerState.lookupNamespace ns x) content of
            Right parsedContent -> (Right parsedContent, NodeConsumerState.bumpOffset x)
            Left contentError ->
              ( Left
                  ( Error
                      [AtOffsetLocation (NodeConsumerState.getOffset x)]
                      (ContentErrorReason contentError)
                  ),
                x
              )
        Xml.NodeElement _ -> failWithUnexpectedNodeType ElementNodeType
        Xml.NodeInstruction _ -> failWithUnexpectedNodeType InstructionNodeType
        Xml.NodeComment _ -> failWithUnexpectedNodeType CommentNodeType
        where
          failWithUnexpectedNodeType actualType =
            ( Left
                ( Error
                    [AtOffsetLocation (NodeConsumerState.getOffset x)]
                    (UnexpectedNodeTypeReason ElementNodeType actualType)
                ),
              (NodeConsumerState.bumpOffset x)
            )
      _ ->
        ( Left
            ( Error
                [AtOffsetLocation (NodeConsumerState.getOffset x)]
                NoNodesLeftReason
            ),
          (NodeConsumerState.bumpOffset x)
        )

-- * Content

-- |
-- Parser in the context of decoded textual content,
-- which can be the value of an attribute or a textual node.
newtype Content content
  = -- | Parser in the context of an xml namespace URI by alias lookup function.
    Content ((Text -> Maybe Text) -> Text -> Either ContentError content)
  deriving (Functor)

data ContentError
  = ParsingContentError Text
  | NamespaceNotFoundContentError Text

-- |
-- Return the content as it is.
textContent :: Content Text
textContent =
  Content (const pure)

-- |
-- Parse the content using the \"attoparsec\" parser.
attoparsedContent :: Attoparsec.Parser a -> Content a
attoparsedContent parser =
  Content (const (first (ParsingContentError . fromString) . Attoparsec.parseOnly parser))

-- |
-- Parse the content as XML Schema QName,
-- automatically resolving the namespace as URI and failing,
-- if none is associated.
--
-- Produces a URI associated with the namespace and name.
-- If the content does not contain colon, produces an unnamespaced name.
--
-- Refs:
--
-- - https://www.w3.org/2001/tag/doc/qnameids.html#sec-qnames-xml
-- - https://en.wikipedia.org/wiki/QName
qNameContent :: Content (Maybe Text, Text)
qNameContent =
  Content $ \lookup content -> case Attoparsec.parseOnly XmlSchemaAttoparsec.qName content of
    Right (ns, name) -> case ns of
      Just ns -> case lookup ns of
        Just uri -> Right (Just uri, name)
        Nothing -> Left (NamespaceNotFoundContentError ns)
      Nothing -> Right (Nothing, name)
    Left err -> Left (ParsingContentError (fromString err))

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
