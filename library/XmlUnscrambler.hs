module XmlUnscrambler
  ( -- * Execution
    parseByteString,

    -- * Parsers by context

    -- ** Element
    AstParser.Element,
    AstParser.elementName,
    AstParser.elementNameIs,
    AstParser.children,
    AstParser.childrenByName,
    AstParser.attributesByName,

    -- ** Nodes
    AstParser.Nodes,
    AstParser.elementNode,
    AstParser.textNode,

    -- ** ByName
    AstParser.ByName,
    AstParser.byName,
  )
where

import qualified Text.XML as Xml
import qualified XmlUnscrambler.AstParser as AstParser
import XmlUnscrambler.Prelude

parseByteString :: AstParser.Element a -> ByteString -> Either Text a
parseByteString astParser input =
  error "TODO"

renderFirstStageError :: SomeException -> Text
renderFirstStageError =
  -- FIXME: Do something more user-friendly
  fromString . show

renderSecondStageError :: AstParser.Error -> Text
renderSecondStageError =
  error "TODO"
