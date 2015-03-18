module Regex where

import Data.Char (chr)
import qualified Data.Set as DSet
import qualified Language.HaLex.RegExp as HRE


type Alphabet = String

data Regex = Epsilon | Literal Char | Union Regex Regex | Star Regex | Seq Regex Regex |
             AnyLiteral | OneOf (DSet.Set Char) | NoneOf (DSet.Set Char) | Plus Regex 
             deriving Show


normalizeRegex :: Alphabet -> Regex -> Regex
normalizeRegex alphabet AnyLiteral = foldl1 (Seq) $ map (Literal) alphabet
normalizeRegex _ (OneOf s) = foldl1 (Union) $ map (Literal) $ DSet.toList s
normalizeRegex alphabet (NoneOf s) = foldl1 (Union) $ map (Literal) $ DSet.toList $ DSet.difference (DSet.fromList alphabet) s
normalizeRegex alphabet (Plus a) = Seq a' (Star a') where a' = normalizeRegex alphabet a
normalizeRegex alphabet (Seq a b) = Seq (normalizeRegex alphabet a) (normalizeRegex alphabet b) 
normalizeRegex alphabet (Star a) = Star (normalizeRegex alphabet a)
normalizeRegex alphabet (Union a b) = Union (normalizeRegex alphabet a) (normalizeRegex alphabet b) 
normalizeRegex _ r = r

regexToHRE :: Regex -> HRE.RegExp Char
regexToHRE = regexToHRE' . normalizeRegex (map chr [0..256])

regexToHRE' Epsilon = HRE.Epsilon
regexToHRE' (Literal c) = HRE.Literal c
regexToHRE' (Union a b) = HRE.Or (regexToHRE a) (regexToHRE b)
regexToHRE' (Star a) = HRE.Star (regexToHRE a)
regexToHRE' (Plus a) = HRE.OneOrMore (regexToHRE a)
regexToHRE' (Seq a b) = HRE.Then (regexToHRE a) (regexToHRE b)
regexToHRE' regex = error $ "not implemented: " ++ show regex
