module Encoding (hashTerm) where

import Data.ByteString 
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Lang
import Crypto.Hash.SHA256 (hash)

packStr :: String -> ByteString
packStr = encodeUtf8 . T.pack



hashTerm :: Term -> ByteString
hashTerm t = hash $ packStr $ show t