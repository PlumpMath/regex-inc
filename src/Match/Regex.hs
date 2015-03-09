module Regex where

import Data.Set
import qualified Language.HaLex.RegExp as HRE

data Regex = Empty | AnyLiteral | Literal Char | AnyOf (Set Char) | AllBut String | Union Regex Regex | Star Regex | Plus Regex | Seq Regex Regex 
             deriving Show


regexToHRE :: Regex -> HRE.RegExp Char
regexToHRE Empty = HRE.Empty
regexToHRE (Literal c) = HRE.Literal c
regexToHRE (Union a b) = HRE.Or (regexToHRE a) (regexToHRE b)
regexToHRE (Star a) = HRE.Star (regexToHRE a)
regexToHRE (Plus a) = HRE.OneOrMore (regexToHRE a)
regexToHRE (Seq a b) = HRE.Then (regexToHRE a) (regexToHRE b)
regexToHRE regex = error $ "not implemented: " ++ show regex
