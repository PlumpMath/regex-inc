import Match

warning :: String
warning = "\
\::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: \n\
\::                                WARNING                                          :: \n\
\::                                                                                 :: \n\
\:: Due to restrictions in HaLeX, the characters you use in the regular expression  :: \n\
\:: are the only ones you can test.                                                 :: \n\
\:: That means that if your expression is \"(a|b)\", you can test if \"a\", \"abb\"       :: \n\
\:: or \"b\" matches, but you can't test if \"c\" matches.                              :: \n\
\:: This will be fixed soon.                                                        :: \n\
\::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: \n"


initDFA :: IO (DFA Char)
initDFA = do putStrLn warning
             putStrLn "Type a regular expression:"
             regex <- getLine
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
