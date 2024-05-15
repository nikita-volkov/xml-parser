module XmlParser.NamespaceRegistry
  ( NamespaceRegistry,
    new,
    lookup,
    resolveElementName,
    resolveAttributeName,
    interpretAttribute,
    interpretAttributes,
  )
where

import Data.Attoparsec.Text qualified as Attoparsec
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Text.XML qualified as Xml
import XmlParser.Attoparsec qualified as Attoparsec
import XmlParser.Prelude hiding (insert, lookup)

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
resolveName useDef (Xml.Name localName uri ns) (NamespaceRegistry map def) =
  case uri of
    Just uri -> Just (Just uri, localName)
    Nothing -> case ns of
      Just ns -> case HashMap.lookup ns map of
        Just uri -> Just (Just uri, localName)
        Nothing -> Nothing
      Nothing ->
        case Attoparsec.parseOnly Attoparsec.qName localName of
          Right (ns, localName) -> case ns of
            Just ns -> case HashMap.lookup ns map of
              Just uri -> Just (Just uri, localName)
              Nothing -> Nothing
            Nothing ->
              Just (if useDef then def else Nothing, localName)
          _ -> Nothing

insert :: Text -> Text -> NamespaceRegistry -> NamespaceRegistry
insert alias uri (NamespaceRegistry map def) =
  NamespaceRegistry (HashMap.insert alias uri map) def

setDefault :: Text -> NamespaceRegistry -> NamespaceRegistry
setDefault uri (NamespaceRegistry map _) =
  NamespaceRegistry map (Just uri)

-- |
-- Extend the registry by reading in the value if this is an \"xmlns\" attribute.
interpretAttribute :: Xml.Name -> Text -> NamespaceRegistry -> NamespaceRegistry
interpretAttribute (Xml.Name localName namespace prefix) uri =
  case namespace of
    Nothing -> case prefix of
      Nothing -> case Attoparsec.parseOnly Attoparsec.qName localName of
        Right (Just "xmlns", name) -> insert name uri
        Right (Nothing, "xmlns") -> setDefault uri
        _ -> id
      _ -> id
    _ -> id

-- |
-- Extend the registry by reading in the \"xmlns\" attributes of an element.
--
-- Useful when diving into an element
interpretAttributes :: Map Xml.Name Text -> NamespaceRegistry -> NamespaceRegistry
interpretAttributes attributes x =
  Map.foldlWithKey' (\x name value -> interpretAttribute name value x) x attributes
