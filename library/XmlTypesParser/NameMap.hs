module XmlTypesParser.NameMap
  ( NameMap,
    fromList,
    fromReverseList,
    empty,
    insert,
    fetch,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.XML.Types as Xml
import XmlTypesParser.Prelude hiding (empty, fromList, insert)
import qualified XmlTypesParser.TupleHashMap as TupleHashMap

data NameMap a
  = NameMap
      (TupleHashMap.TupleHashMap Text Text [a])
      -- ^ Namespaced
      (HashMap Text [a])
      -- ^ Unnamespaced

fromList :: [(Xml.Name, a)] -> NameMap a
fromList =
  fromReverseList . reverse

fromReverseList :: [(Xml.Name, a)] -> NameMap a
fromReverseList list =
  foldr step NameMap list TupleHashMap.empty HashMap.empty
  where
    step (Xml.Name name ns _, contents) next !map1 !map2 =
      case ns of
        Just ns ->
          next (TupleHashMap.insertSemigroup ns name [contents] map1) map2
        Nothing ->
          next map1 (HashMap.insertWith (++) name [contents] map2)

empty :: NameMap a
empty =
  NameMap TupleHashMap.empty HashMap.empty

insert :: Maybe Text -> Text -> a -> NameMap a -> NameMap a
insert ns name contents (NameMap map1 map2) =
  case ns of
    Just ns ->
      NameMap (TupleHashMap.insertSemigroup ns name [contents] map1) map2
    Nothing ->
      NameMap map1 (HashMap.insertWith (++) name [contents] map2)

fetch :: Maybe Text -> Text -> NameMap a -> Maybe (a, NameMap a)
fetch ns name (NameMap map1 map2) =
  case ns of
    Just ns ->
      TupleHashMap.alterF
        ( \case
            Just list ->
              case list of
                head : tail -> case tail of
                  [] -> Compose (Just (head, Nothing))
                  _ -> Compose (Just (head, Just tail))
                _ -> Compose Nothing
            Nothing ->
              Compose Nothing
        )
        ns
        name
        map1
        & getCompose
        & fmap (fmap (flip NameMap map2))
    Nothing ->
      HashMap.alterF
        ( \case
            Just list ->
              case list of
                head : tail -> case tail of
                  [] -> Compose (Just (head, Nothing))
                  _ -> Compose (Just (head, Just tail))
                _ -> Compose Nothing
            Nothing ->
              Compose Nothing
        )
        name
        map2
        & getCompose
        & fmap (fmap (NameMap map1))
