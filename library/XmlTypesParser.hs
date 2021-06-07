module XmlTypesParser
  ( -- * Execution
    parseElement,
    Error (..),
    Location (..),

    -- * Parsers by context

    -- ** Element
    Element,
    childrenByName,
    attributesByName,

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

data Reason
  = NameNotFoundReason (Maybe Text) Text
  | AttoparsecFailedReason Text
  | NoReason

newtype Element a
  = Element (Xml.Element -> Either Error a)

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
children =
  error "TODO"

-- |
-- Parser in the context of a sequence of nodes.
data Nodes a

elementNode :: Element a -> Nodes a
elementNode =
  error "TODO"

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
