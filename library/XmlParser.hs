module XmlParser
  ( -- * Execution
    parseByteString,
    parseFile,

    -- * Parsers by context

    -- ** Element
    AstParser.Element,
    AstParser.elementName,
    AstParser.elementNameIs,
    AstParser.children,
    AstParser.childrenByName,
    AstParser.attributesByName,
    AstParser.astElement,

    -- ** Nodes
    AstParser.Nodes,
    AstParser.elementNode,
    AstParser.contentNode,

    -- ** ByName
    AstParser.ByName,
    AstParser.byName,

    -- ** Content
    AstParser.Content,
    AstParser.textContent,
    AstParser.narrowedContent,
    AstParser.enumContent,
    AstParser.attoparsedContent,
    AstParser.qNameContent,
  )
where

import qualified Text.XML as XmlConduit
import qualified XmlParser.AstParser as AstParser
import XmlParser.Prelude
import qualified XmlParser.XmlConduitWrapper as XmlConduitWrapper

-- |
-- Parse XML bytestring.
parseByteString :: AstParser.Element a -> ByteString -> Either Text a
parseByteString astParser input =
  XmlConduitWrapper.parseByteString input >>= parseXmlConduitDocument astParser

-- |
-- Parse XML file.
parseFile :: AstParser.Element a -> FilePath -> IO (Either Text a)
parseFile astParser path =
  fmap (>>= parseXmlConduitDocument astParser) $
    XmlConduitWrapper.parseFile path

parseXmlConduitDocument :: AstParser.Element a -> XmlConduit.Document -> Either Text a
parseXmlConduitDocument astParser =
  first AstParser.renderElementError . AstParser.parseElement astParser . XmlConduit.documentRoot