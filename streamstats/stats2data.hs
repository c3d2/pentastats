module Main where

import Control.Applicative
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Data.Maybe

newtype Timestamp = Timestamp Integer
    deriving (Eq, Show, Ord)

parseLines :: [String] -> [(Timestamp, HM.HashMap String Int)]
parseLines [] = []
parseLines (t:lines) =
    let ts = Timestamp $ read t
        fields = takeWhile ((== 2) . length) $
                 map words lines
        categories = HM.fromList
                     [ (category, read count)
                       | category:count:[] <- fields
                     ]
        lines' = drop (length fields) lines
    in (ts, categories) : parseLines lines'

formatOutput series =
    let keys = Set.toList $
               Set.unions $
               map (Set.fromList . HM.keys . snd)
               series
        header = "#" : (map (\(i, key) -> show i ++ ":" ++ key) $ zip [1..] keys)
        lines = do (Timestamp t, values) <- series
                   let values' = 
                           map (show .
                                fromMaybe 0 . 
                                (`HM.lookup` values)
                               ) keys
                   case HM.null values of
                     True ->
                         return []
                     False ->
                         return $ show t : values'
    in header : lines


main = unlines <$> map unwords <$> filter (not . null) <$> formatOutput <$> 
       parseLines <$> lines <$> 
       getContents >>=
       putStrLn
       