module XmlTypesParser.NameMap
  ( NameMap,
    fromList,
    fetch,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.XML.Types as Xml
import XmlTypesParser.Prelude

data NameMap a
  = NameMap
      (HashMap Text (HashMap Text [a]))
      -- ^ Namespaced
      (HashMap Text [a])
      -- ^ Unnamespaced

{-# INLINE fromList #-}
fromList :: [(Xml.Name, a)] -> NameMap a
fromList list =
  foldr step NameMap (reverse list) HashMap.empty HashMap.empty
  where
    step (Xml.Name name ns _, contents) next !map1 !map2 =
      case ns of
        Just ns ->
          next
            ( HashMap.alter
                ( maybe
                    (Just (HashMap.singleton name [contents]))
                    (Just . HashMap.insertWith (++) name [contents])
                )
                ns
                map1
            )
            map2
        Nothing ->
          next map1 (HashMap.insertWith (++) name [contents] map2)

fetch :: Maybe Text -> Text -> NameMap a -> Maybe (a, NameMap a)
fetch ns name (NameMap map1 map2) =
  case ns of
    Just ns ->
      HashMap.alterF
        ( \case
            Just byNameMap ->
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
                byNameMap
                & fmap Just
            Nothing ->
              Compose Nothing
        )
        ns
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
