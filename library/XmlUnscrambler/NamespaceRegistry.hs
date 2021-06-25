module XmlUnscrambler.NamespaceRegistry
  ( NamespaceRegistry,
    new,
    lookup,
    resolveElementName,
    resolveAttributeName,
    interpretAttribute,
    interpretAttributes,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Text.XML as Xml
import XmlUnscrambler.Prelude hiding (extend, insert, lookup)
import qualified XmlUnscrambler.XmlSchemaAttoparsec as XmlSchemaAttoparsec

data NamespaceRegistry
  = NamespaceRegistry
      (HashMap Text Text)
      (Maybe Text)

new :: NamespaceRegistry
new = NamespaceRegistry HashMap.empty Nothing

lookup :: Text -> NamespaceRegistry -> Maybe Text
lookup ns (NamespaceRegistry nsMap _) =
  HashMap.lookup ns nsMap

resolveElementName :: Xml.Name -> NamespaceRegistry -> Maybe (Maybe Text, Text)
resolveElementName = resolveName True

resolveAttributeName :: Xml.Name -> NamespaceRegistry -> Maybe (Maybe Text, Text)
resolveAttributeName = resolveName False

resolveName :: Bool -> Xml.Name -> NamespaceRegistry -> Maybe (Maybe Text, Text)
resolveName useDef name (NamespaceRegistry map def) =
  case parseQName name of
    Just (ns, name) -> case ns of
      Just ns -> case HashMap.lookup ns map of
        Just uri -> Just (Just uri, name)
        Nothing -> Nothing
      Nothing -> Just (if useDef then def else Nothing, name)
    Nothing -> Nothing

insert :: Text -> Text -> NamespaceRegistry -> NamespaceRegistry
insert alias uri (NamespaceRegistry map def) =
  NamespaceRegistry (HashMap.insert alias uri map) def

setDefault :: Text -> NamespaceRegistry -> NamespaceRegistry
setDefault =
  error "TODO"

-- |
-- Extend the registry by reading in the value if this is an \"xmlns\" attribute.
interpretAttribute :: Xml.Name -> Text -> NamespaceRegistry -> NamespaceRegistry
interpretAttribute name uri =
  case parseQName name of
    Just (Just "xmlns", name) -> insert name uri
    Just (Nothing, "xmlns") -> setDefault uri
    _ -> id

-- |
-- Extend the registry by reading in the \"xmlns\" attributes of an element.
--
-- Useful when diving into an element
interpretAttributes :: Map Xml.Name Text -> NamespaceRegistry -> NamespaceRegistry
interpretAttributes attributes x =
  Map.foldlWithKey' (\x name value -> interpretAttribute name value x) x attributes

-- * Utils

parseQName :: Xml.Name -> Maybe (Maybe Text, Text)
parseQName (Xml.Name localName namespace prefix) =
  case namespace of
    Nothing -> case prefix of
      Nothing -> case Attoparsec.parseOnly XmlSchemaAttoparsec.qName localName of
        Right qName -> Just qName
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
