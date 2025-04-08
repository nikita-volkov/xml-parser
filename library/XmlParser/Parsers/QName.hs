module XmlParser.Parsers.QName where

import qualified Data.Map.Strict as Map
import XmlParser.Preludes.Parser

type Params = NamespaceMap

type Result = QName

parse :: Params -> Parser Result
parse params = do
  namespace <- do
    prefix <- error "TODO"
    case prefix of
      Just prefix -> case Map.lookup prefix params of
        Nothing -> fail "Undeclared prefix"
        Just namespace -> pure (Just namespace)
      Nothing -> pure Nothing
  name <- error "TODO"
  pure QName {namespace, name}
