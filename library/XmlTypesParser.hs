module XmlTypesParser
(
)
where

import XmlTypesParser.Prelude
import qualified Data.XML.Types as Xml


data Error = Error [Location] Text

data Location =
  ChildByNameLocation Text |
  AttrByNameLocation Text |
  NodeAtOffsetLocation Int


newtype Element a =
  Element (Xml.Element -> Either Error a)

{-|
Look up the first element by name and parse it.
-}
childByName :: Maybe Text -> Text -> Element a -> Element a
childByName =
  error "TODO"

attrByName :: Maybe Text -> Text -> (Text -> Either Text a) -> Element a
attrByName =
  error "TODO"

{-| Children sequence by order. -}
children :: Nodes a -> Element a
children =
  error "TODO"



{-|
Parser in the context of a sequence of nodes.
-}
data Nodes a

elementNode :: Element a -> Nodes a
elementNode =
  error "TODO"

textNode :: (Text -> Either Text a) -> Nodes a
textNode =
  error "TODO"
