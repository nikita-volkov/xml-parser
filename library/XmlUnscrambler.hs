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

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Text.XML as XmlConduit
import qualified Text.XML.Unresolved as XmlConduit (InvalidEventStream (..))
import qualified XmlUnscrambler.AstParser as AstParser
import XmlUnscrambler.Prelude

{-|
Parse the XML document bytestring.
-}
parseByteString :: AstParser.Element a -> ByteString -> Either Text a
parseByteString astParser =
  parseLazyByteString astParser . LazyByteString.fromStrict

parseLazyByteString :: AstParser.Element a -> LazyByteString.ByteString -> Either Text a
parseLazyByteString astParser input =
  first renderFirstStageError (XmlConduit.parseLBS xmlConduitSettings input)
    >>= first renderSecondStageError . AstParser.parseElement astParser . XmlConduit.documentRoot
  where
    xmlConduitSettings =
      def
        { XmlConduit.psRetainNamespaces = True
        }

renderFirstStageError :: SomeException -> Text
renderFirstStageError e
  | Just e <- fromException @XmlConduit.XMLException e =
    fromString (show e)
  | Just e <- fromException @XmlConduit.InvalidEventStream e =
    fromString (show e)
  | Just (XmlConduit.UnresolvedEntityException e) <- fromException @XmlConduit.UnresolvedEntityException e =
    "Unresolved entities: " <> Text.intercalate "," (toList e)
  | otherwise =
    -- FIXME: Find other cases and do something more user-friendly about them
    fromString (show e)

renderSecondStageError :: AstParser.Error -> Text
renderSecondStageError =
  error "TODO"
