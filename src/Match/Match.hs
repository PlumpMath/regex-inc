{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses,DataKinds,KindSignatures #-}

module Match where



import Data.List (elemIndex, foldl') 
import Data.Array (Array, Ix, array, bounds, indices, range, (!))
import Data.Monoid (Monoid, mappend, mempty)
import Data.IntSet (IntSet, fromList, member)
import Data.FingerTree hiding (fromList)

import qualified Language.HaLex.Dfa as HDA
import qualified Language.HaLex.Ndfa as HNA
import qualified Language.HaLex.RegExp as HRE
import qualified Language.HaLex.RegExp2Fa as HRE2HA

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

buildDFA :: (Ix at) => HRE.RegExp at -> DFA at
buildDFA re = buildFromHDA (HRE2HA.regExp2Dfa re)

buildFromHDA :: (Ix at) => HDA.Dfa [Int] at -> DFA at
buildFromHDA (HDA.Dfa v s s0 sa d) = DFA n s0' sa' delta
    where 
        n          = length s
        s0'        = getIndex s0
        delta      = tabulate n . d'
        sa'        = fromList $ map getIndex sa
        d' a state = getIndex $ d (s!!state) a
        getIndex e = case elemIndex e s of 
                          Just i  -> i
                          Nothing -> undefined

-- Regular Expressions

stringFromList = foldl' (|>) empty
buildString dfa str = stringFromList (map (Elem dfa) $ str)

matchesDFA :: DFA Char -> String -> Bool
matchesDFA dfa str = member (evaluate (snd (measure (buildString dfa str))) (s0 dfa)) (accepting dfa)

compile :: String -> DFA Char
compile = (buildDFA . regexToHRE . stringToRegex)

matches :: String -> String -> Bool
matches regex input = matchesDFA (compile regex) input


-- Finger Tree


type FingerString = FingerTree (Size, Function) (Elem Char)

data Elem at = Elem { dfa :: DFA at , getElem :: at } 
data Size = Size { getSize :: Int } deriving (Eq, Ord, Show)


instance Monoid Size where
    mempty = Size 0
    Size m `mappend` Size n = Size (m+n)

instance (Ix at) => Measured (Size, Function) (Elem at) where
    measure (Elem dfa a) = (Size 1, (delta dfa) a)


insert :: DFA Char -> Int -> Char -> FingerString -> FingerString
insert dfa i c z = l >< (Elem dfa c <| r) where (l,r) = split (\(Size n,_) -> n>i) z

