import Match
import ParseRegex

initDFA :: IO (DFA Char)
initDFA = do putStrLn "Type a regular expression:"
             regex <- getLine
             putStrLn $ show $ stringToRegex regex
             return $ compile regex

interactDFA :: (DFA Char) -> IO ()
interactDFA dfa = do putStrLn "Type an string to test or \"quit\" to quit" 
                     str <- getLine
                     if (str == "quit")
                     then return ()
                     else (putStrLn $ show $ matchesDFA dfa str) >> interactDFA dfa

main :: IO ()
main = do dfa <- initDFA
          interactDFA dfa
