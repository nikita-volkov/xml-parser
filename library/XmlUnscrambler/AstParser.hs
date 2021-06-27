module XmlUnscrambler.AstParser
  ( -- * Execution
    parseElement,
    renderElementError,
    ElementError (..),
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
    contentNode,

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
parseElement :: Element a -> Xml.Element -> Either ElementError a
parseElement (Element run) element =
  run
    (NamespaceRegistry.interpretAttributes (Xml.elementAttributes element) NamespaceRegistry.new)
    element

renderElementError :: ElementError -> Text
renderElementError =
  Tb.run . (\(a, b) -> "/" <> Tb.intercalate "/" (reverse a) <> ": " <> b) . simplifyElementError

simplifyElementError :: ElementError -> ([Tb.Builder], Tb.Builder)
simplifyElementError =
  elementError []
  where
    sortedList renderer =
      mappend "[" . flip mappend "]" . Tb.intercalate ", " . fmap renderer . sort
    name a b =
      case a of
        Just a -> Tb.text a <> ":" <> Tb.text b
        Nothing -> Tb.text b
    elementError collectedPath = \case
      NoneOfChildrenFoundByNameElementError a b ->
        ( collectedPath,
          "None of following child element names found: "
            <> sortedList (uncurry name) a
            <> ". Names available: "
            <> sortedList (uncurry name) b
        )
      ChildByNameElementError a b c ->
        elementError (name a b : collectedPath) c
      ChildAtOffsetElementError a b ->
        nodeError (Tb.decimal a : collectedPath) b
    nodeError collectedPath = \case
      UnexpectedNodeTypeNodeError a b ->
        ( collectedPath,
          "Unexpected node type. Got " <> nodeType b <> ", but expected " <> nodeType a
        )
      NotAvailableNodeError ->
        (collectedPath, "No nodes left")
      ElementNodeError a ->
        elementError collectedPath a
      TextNodeError a ->
        (collectedPath, contentError a)
    nodeType = \case
      ElementNodeType -> "element"
      InstructionNodeType -> "instruction"
      ContentNodeType -> "content"
      CommentNodeType -> "comment"
    contentError = \case
      ParsingContentError a ->
        Tb.text a
      NamespaceNotFoundContentError a ->
        "Namespace not found: " <> Tb.text a

-- |
-- Error in the context of an element.
--
-- It has a tree structure specifying the context of containing operations.
data ElementError
  = AttributeByNameElementError
      (Maybe Text)
      Text
      ContentError
  | NoneOfAttributesFoundByNameElementError
      [(Maybe Text, Text)]
      -- ^ Not found.
      [(Maybe Text, Text)]
      -- ^ Out of.
  | NoneOfChildrenFoundByNameElementError
      [(Maybe Text, Text)]
      -- ^ Not found.
      [(Maybe Text, Text)]
      -- ^ Out of.
  | ChildByNameElementError
      (Maybe Text)
      -- ^ Namespace.
      Text
      -- ^ Name.
      ElementError
      -- ^ Reason. Not 'NodeError' because only element nodes can be looked up by name.
  | ChildAtOffsetElementError
      Int
      -- ^ Offset.
      NodeError
      -- ^ Reason.

data NodeError
  = UnexpectedNodeTypeNodeError
      NodeType
      -- ^ Expected.
      NodeType
      -- ^ Actual.
  | NotAvailableNodeError
  | ElementNodeError ElementError
  | TextNodeError ContentError

data ContentError
  = ParsingContentError Text
  | NamespaceNotFoundContentError Text

data NodeType
  = ElementNodeType
  | InstructionNodeType
  | ContentNodeType
  | CommentNodeType

-- |
-- Parse in the context of an element node.
newtype Element a
  = Element (NamespaceRegistry.NamespaceRegistry -> Xml.Element -> Either ElementError a)
  deriving
    (Functor, Applicative, Monad)
    via (ReaderT (NamespaceRegistry.NamespaceRegistry) (ReaderT Xml.Element (Except ElementError)))

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
  \(ByName runByName) -> Element $ \nreg (Xml.Element _ attributes nodes) ->
    let nameMap = NameMap.fromNodes nreg nodes
        newNreg = NamespaceRegistry.interpretAttributes attributes nreg
     in case runByName nameMap (\element (Element run) -> run newNreg element) of
          OkByNameResult _ res -> Right res
          NotFoundByNameResult unfoundNames ->
            let availNames = NameMap.extractNames nameMap
             in Left (NoneOfChildrenFoundByNameElementError unfoundNames availNames)
          FailedDeeperByNameResult ns name err ->
            Left (ChildByNameElementError ns name err)

-- |
-- Look up the first attribute by name and parse it.
attributesByName :: ByName Content a -> Element a
attributesByName =
  error "TODO"

-- |
-- Children sequence by order.
children :: Nodes a -> Element a
children (Nodes runNodes) =
  Element $ \nreg (Xml.Element _ _ nodes) ->
    runNodes (NodeConsumerState.new nodes nreg)
      & second fst

-- |
-- Parser in the context of a sequence of nodes.
newtype Nodes a
  = Nodes (NodeConsumerState.NodeConsumerState -> Either ElementError (a, NodeConsumerState.NodeConsumerState))
  deriving
    (Functor, Applicative, Monad)
    via (StateT NodeConsumerState.NodeConsumerState (Either ElementError))

-- |
-- Consume the next node expecting it to be element and parse its contents.
elementNode :: Element a -> Nodes a
elementNode (Element runElement) =
  Nodes $ \x ->
    case NodeConsumerState.fetchNode x of
      Just (node, x) -> case node of
        Xml.NodeElement element ->
          bimap
            (ChildAtOffsetElementError (NodeConsumerState.getOffset x) . ElementNodeError)
            (,NodeConsumerState.bumpOffset x)
            (runElement (NodeConsumerState.getNamespaceRegistry x) element)
        Xml.NodeInstruction _ -> failWithUnexpectedNodeType InstructionNodeType
        Xml.NodeContent _ -> failWithUnexpectedNodeType ContentNodeType
        Xml.NodeComment _ -> failWithUnexpectedNodeType CommentNodeType
        where
          failWithUnexpectedNodeType actualType =
            Left
              ( ChildAtOffsetElementError
                  (NodeConsumerState.getOffset x)
                  (UnexpectedNodeTypeNodeError ElementNodeType actualType)
              )
      _ -> Left (ChildAtOffsetElementError (NodeConsumerState.getOffset x) NotAvailableNodeError)

-- |
-- Consume the next node expecting it to be textual and parse its contents.
contentNode :: Content content -> Nodes content
contentNode (Content parseContent) =
  Nodes $ \x ->
    case NodeConsumerState.fetchNode x of
      Just (node, x) -> case node of
        Xml.NodeContent content ->
          case parseContent (\ns -> NodeConsumerState.lookupNamespace ns x) content of
            Right parsedContent ->
              Right (parsedContent, NodeConsumerState.bumpOffset x)
            Left contentError ->
              Left
                ( ChildAtOffsetElementError
                    (NodeConsumerState.getOffset x)
                    (TextNodeError contentError)
                )
        Xml.NodeElement _ -> failWithUnexpectedNodeType ElementNodeType
        Xml.NodeInstruction _ -> failWithUnexpectedNodeType InstructionNodeType
        Xml.NodeComment _ -> failWithUnexpectedNodeType CommentNodeType
        where
          failWithUnexpectedNodeType actualType =
            Left
              ( ChildAtOffsetElementError
                  (NodeConsumerState.getOffset x)
                  (UnexpectedNodeTypeNodeError ContentNodeType actualType)
              )
      _ -> Left (ChildAtOffsetElementError (NodeConsumerState.getOffset x) NotAvailableNodeError)

-- * Content

-- |
-- Parser in the context of decoded textual content,
-- which can be the value of an attribute or a textual node.
newtype Content content
  = -- | Parser in the context of an xml namespace URI by alias lookup function.
    Content ((Text -> Maybe Text) -> Text -> Either ContentError content)
  deriving (Functor)

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

data ByNameResult deeperError content a
  = NotFoundByNameResult [(Maybe Text, Text)]
  | FailedDeeperByNameResult (Maybe Text) Text deeperError
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
      ( forall content deeperError.
        NameMap.NameMap content ->
        (content -> forall x. parser x -> Either deeperError x) ->
        ByNameResult deeperError content a
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
    ByName $ \_ _ -> NotFoundByNameResult []
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
