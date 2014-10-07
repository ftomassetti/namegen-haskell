{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map.Strict as M
import qualified Data.List as L
import System.Random
import Data.Maybe

mypath = "../namegen-data/personnames/Italian_male.txt"

data Prosecution = NextChar Char | NameEnd
                   deriving (Show, Ord, Eq)

data Language = Language { firstLetter :: M.Map Char Float, transitions :: TransitionMap }
                deriving Show

type TransitionMap = M.Map Char (M.Map Prosecution Float)
type TransitionMapInt = M.Map Char (M.Map Prosecution Int)

countTransitionsLetter :: TransitionMapInt -> (Char, Prosecution) -> TransitionMapInt
countTransitionsLetter tm (orig, dst) = M.insertWith (\_ subMap ->M.insertWith (+) dst 1 subMap) orig (M.singleton dst 1) tm

transition :: String -> Int -> (Char, Prosecution)
transition name index = let orig = name !! (pred index)
                            dst = if index<(length name) then NextChar (name !! index) else NameEnd
                        in (orig, dst)

countTransitionsName :: TransitionMapInt -> String -> TransitionMapInt 
countTransitionsName tm name = let transitions = L.map (\i -> transition name i) [1..(length name)]
                                   tm' = foldl countTransitionsLetter tm transitions
                               in tm'

convertSubTm :: M.Map Prosecution Int -> M.Map Prosecution Float
convertSubTm subTm = let total :: Int = M.foldl (+) 0 subTm
                         total' :: Float = fromIntegral total
                     in M.map (\i -> (fromIntegral i) / total') subTm

convertTm :: TransitionMapInt -> TransitionMap
convertTm iTm = M.map convertSubTm iTm

fromSamples :: [String] -> Language
fromSamples samples = let initials = map (!! 0) samples
                          freqInitials = map (\g -> (head g, length g)) $ L.group . L.sort $ initials
                          l = length samples
                          probInitials = L.map (\(letter, freq) -> (letter, (fromIntegral freq) / (fromIntegral l))) freqInitials
                          probInitials' = M.fromList probInitials
                          -- first we count the frequency of each letter
                          transitionMap = foldl countTransitionsName M.empty samples
                          transitionMap' = convertTm transitionMap
                       in Language probInitials' transitionMap'

loadSamples :: FilePath -> IO [String]
loadSamples path = do content <- readFile path 
                      let ls = lines content
                      return ls

getInitial :: Float -> Language -> Char
getInitial f l = helper f (M.toAscList (firstLetter l))
                 where helper f [] = error "?"
                       helper f ((k,v):as) = if v>f then k else helper (f-v) as

getProsecution :: Float -> Char -> Language -> Prosecution
getProsecution f currChar l = helper f (M.toAscList transitionMap)
                              where transitionMap = fromJust (M.lookup currChar (transitions l))
                                    helper f [] = error "?"
                                    helper f ((k,v):as) = if v>=f then k else helper (f-v) as

generateName' :: [Float] -> Char -> Language -> String
generateName' rseq currChar l = case prosecution of
                                  NameEnd -> []
                                  NextChar c -> c:(generateName' (tail rseq) c l)
                                where prosecution = getProsecution (head rseq) currChar l

generateName :: Language -> Int -> String
generateName l seed = initial:(generateName' (tail rseq) initial l)
                      where rg = mkStdGen seed
                            rseq = randomRs (0.0,1.0) rg
                            initial = getInitial (head rseq) l