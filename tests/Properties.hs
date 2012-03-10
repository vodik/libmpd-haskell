{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -Wwarn -fno-warn-orphans -fno-warn-missing-methods #-}
module Properties (main) where

import           Arbitrary
import           Defaults
import           Unparse

import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Types
import           Network.MPD.Util hiding (read)

import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Map as M
import           Data.Time
import           System.Environment
import           Text.Printf
import           Test.QuickCheck

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

main :: IO ()
main = do
    n <- (maybe 100 read . listToMaybe) `liftM` getArgs
    mapM_ (\(s, f) -> printf "%-25s : " s >> f n) tests
    where tests = [("splitGroups / reversible" :: String,
                        mytest prop_splitGroups_rev)
                  ,("splitGroups / integrity",
                        mytest prop_splitGroups_integrity)
                  ,("parseBool", mytest prop_parseBool)
                  ,("parseBool / reversible",
                        mytest prop_parseBool_rev)
                  ,("showBool", mytest prop_showBool)
                  ,("toAssoc / reversible",
                        mytest prop_toAssoc_rev)
                  ,("parseNum", mytest prop_parseNum)
                  ,("parseDate / simple",
                        mytest prop_parseDate_simple)
                  ,("parseDate / complex",
                        mytest prop_parseDate_complex)
                  ,("parseIso8601", mytest prop_parseIso8601)
                  ,("parseCount", mytest prop_parseCount)
                  ,("parseOutputs", mytest prop_parseOutputs)
                  ,("parseSong", mytest prop_parseSong)
                  ,("parseStats", mytest prop_parseStats)]

mytest :: Testable a => a -> Int -> IO ()
mytest a n = quickCheckWith stdArgs { maxSize = n } a

prop_parseDate_simple :: YearString -> Bool
prop_parseDate_simple (YS x) = isJust $ parseDate x

prop_parseDate_complex :: DateString -> Bool
prop_parseDate_complex (DS x) = isJust $ parseDate x

-- Conversion to an association list.
prop_toAssoc_rev :: AssocString -> Bool
prop_toAssoc_rev x = k == k' && v == v'
    where
        AS str k v = x
        (k',v') = toAssoc str

prop_parseBool_rev :: BoolString -> Bool
prop_parseBool_rev (BS x) = showBool (fromJust $ parseBool x) == x

prop_parseBool :: BoolString -> Bool
prop_parseBool (BS xs) =
    case parseBool xs of
        Nothing    -> False
        Just True  -> xs == "1"
        Just False -> xs == "0"

prop_showBool :: Bool -> Bool
prop_showBool True = showBool True == "1"
prop_showBool x    = showBool x == "0"

prop_splitGroups_rev :: [(ByteString, ByteString)] -> Property
prop_splitGroups_rev xs = not (null xs) ==>
    let wrappers = [fst $ head xs]
        r = splitGroups wrappers xs
    in r == splitGroups wrappers (concat r)

prop_splitGroups_integrity :: [(ByteString, ByteString)] -> Property
prop_splitGroups_integrity xs = not (null xs) ==>
    sort (concat $ splitGroups [fst $ head xs] xs) == sort xs

prop_parseNum :: Integer -> Bool
prop_parseNum x =
    case xs of
        '-':_ -> ((<= 0) `fmap` parseNum bs) == Just True
        _     -> ((>= 0) `fmap` parseNum bs) == Just True
    where
      xs = show x
      bs = UTF8.fromString xs


--------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------

-- This property also ensures, that (instance Arbitrary UTCTime) is sound.
-- Indeed, a bug in the instance declaration was the primary motivation to add
-- this property.
prop_parseIso8601 :: UTCTime -> Bool
prop_parseIso8601 t = Just t == (parseIso8601 . UTF8.fromString . formatIso8601) t

prop_parseCount :: Count -> Bool
prop_parseCount c = Right c == (parseCount . map UTF8.fromString . lines . unparse) c

prop_parseOutputs :: [Device] -> Bool
prop_parseOutputs ds =
    Right ds == (parseOutputs . map UTF8.fromString . lines . concatMap unparse) ds

deriving instance Ord Value

prop_parseSong :: Song -> Bool
prop_parseSong s = Right (sortTags s) == sortTags `fmap` (parseSong . toAssocList . map UTF8.fromString . lines . unparse) s
  where
    -- We consider lists of tag values equal if they contain the same elements.
    -- To ensure that two lists with the same elements are equal, we bring the
    -- elements in a deterministic order.
    sortTags song = song {sgTags = M.map sort $ sgTags song}

prop_parseStats :: Stats -> Bool
prop_parseStats s = Right s == (parseStats . map UTF8.fromString . lines . unparse) s
