# regex-inc
Incremental Regular Expressions with Haskell

## Example interaction

```
Prelude> :load Match
[1 of 3] Compiling Regex            ( Regex.hs, interpreted )
[2 of 3] Compiling ParseRegex       ( ParseRegex.hs, interpreted )
[3 of 3] Compiling Match            ( Match.hs, interpreted )
Ok, modules loaded: Match, Regex, ParseRegex.
*Match> regex <- getLine
a[A-Z]b*
*Match> let dfa = compile regex
*Match> let ftree = initialize dfa "aAAbbbbb"
*Match> ftree
fromList ['a','A','A','b','b','b','b','b']
*Match> dfa `accepts` ftree
False
*Match> let ftree' = erase 3 ftree
*Match> ftree'
fromList ['a','A','b','b','b','b','b']
*Match> dfa `accepts` ftree'
True
*Match> let ftree'' = modify dfa 6 'c' ftree'
*Match> ftree''
fromList ['a','A','b','b','b','c','b']
*Match> dfa `accepts` ftree''
False
```
