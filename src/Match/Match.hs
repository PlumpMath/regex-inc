{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses,DataKinds,KindSignatures #-}

module Match where


import Debug.Trace (trace)
import Data.Char (chr)
import Data.List (elemIndex, foldl') 
import Data.Array (Array, Ix, array, bounds, indices, range, (!))
import Data.Monoid (Monoid, mappend, mempty)
import Data.IntSet (IntSet, fromList, member)
import Data.FingerTree hiding (fromList)

import qualified Language.HaLex.Dfa as HDA
import qualified Language.HaLex.Ndfa as HNA
import qualified Language.HaLex.RegExp as HRE
import qualified Language.HaLex.RegExp2Fa as HRE2HA
import qualified Language.HaLex.FaOperations as HFAO

import Regex
import ParseRegex


-- Function type


newtype Function = Function {function :: Maybe (Array Int Int)}


tabulate :: Int -> (Int -> Int) -> Function
tabulate n f = Function $ Just $ array (0, n) [(x, f x) | x <- range(0, n)]


compose :: Function -> Function -> Function
compose f (Function Nothing) = f
compose (Function Nothing) g = g
compose (Function (Just f)) (Function (Just g)) = 
    if bounds f == bounds g
    then Function $ Just $ array (bounds f) [(x, (g ! (f ! x))) | x <- (indices f)]
    else error "array sizes differ"


evaluate :: Function -> Int -> Int
evaluate (Function Nothing)  = id
evaluate (Function (Just f)) = (f !)


instance Monoid Function where
    mempty = Function Nothing
    mappend f g = compose f g



-- Deterministic Automaton


data DFA at = DFA { nStates :: Int , s0 :: Int , accepting :: IntSet , delta :: at -> Function }

buildDFA :: HRE.RegExp Char -> DFA Char
buildDFA = buildFromHDA . HDA.beautifyDfa . HFAO.ndfa2dfa . (forceVocabulary (map chr [0..256])) . HRE2HA.regExp2Ndfa

forceVocabulary :: String -> HNA.Ndfa at Char -> HNA.Ndfa at Char
forceVocabulary v (HNA.Ndfa _ ss s t d) = HNA.Ndfa v ss s t d

buildFromHDA :: (Ix at) => HDA.Dfa Int at -> DFA at
buildFromHDA (HDA.Dfa v s s0 sa d) = DFA n s0 sa' delta
    where 
        n          = length s
        delta c    = tabulate n ((flip d) c)
        sa'        = fromList $ sa

-- Regular Expressions


matchesDFA :: DFA Char -> String -> Bool
matchesDFA dfa = (accepts dfa) . (initialize dfa)

compile :: String -> DFA Char
compile = (buildDFA . regexToHRE . stringToRegex)

matches :: String -> String -> Bool
matches regex input = matchesDFA (compile regex) input


-- Finger Tree


type FingerString = FingerTree (Size, Function) (Elem Char)

data Elem at = Elem { dfa :: DFA at , getElem :: at } 
data Size = Size { getSize :: Int } deriving (Eq, Ord, Show)


instance Show (Elem Char) where
    show = show . getElem

instance Monoid Size where
    mempty = Size 0
    Size m `mappend` Size n = Size (m+n)

instance (Ix at) => Measured (Size, Function) (Elem at) where
    measure (Elem dfa a) = (Size 1, (delta dfa) a)


initialize :: DFA Char -> String -> FingerString
initialize dfa str = foldl' (|>) empty $ map (Elem dfa) $ str

insert :: DFA Char -> Int -> Char -> FingerString -> FingerString
insert dfa i c z = l >< (Elem dfa c <| r) 
    where (l, r) = split (\(Size n, _) -> n > i) z

modify :: DFA Char -> Int -> Char -> FingerString -> FingerString
modify dfa i c z = l >< (Elem dfa c <| r) 
    where (l', r) = split (\(Size n, _) -> n > i) z
          l :> _ = viewr l'

erase :: Int -> FingerString -> FingerString
erase i z = l >< r
    where (l', r) = split (\(Size n, _) -> n > i) z
          l :> _ = viewr l'

accepts :: DFA Char -> FingerString -> Bool
accepts dfa = (flip member) (accepting dfa) . (flip evaluate) (s0 dfa) . snd . measure

