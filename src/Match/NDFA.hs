module NDFA where


data NDFA a = NDFA {s0 :: Int, states :: [Int], accepting :: Int, delta :: (Int -> (Maybe a) -> [Int])}

type CountedNDFA a = (NDFA a, Int)

ndfaFromRegex :: Int -> Regex -> (CountedNDFA Char)

ndfaFromRegex n Epsilon = (NDFA (n [n] n (const [])), n + 1)

ndfaFromRegex n (Literal c) = (NDFA (n [n, n + 1] (n + 1) (delta)), n + 2)
    where delta n c = n + 1
          delta _ _ = []

ndfaFromRegex n (Union a b) = (NDFA (n newStates n' newDelta), n' + 1) 
    where (NDFA aS0 aStates aAccepting aDelta, m) = ndfaFromRegex (n + 1) a
          (NDFA bS0 bStates bAccepting bDelta, n') = ndfaFromRegex m b
          newStates = [n, n'] ++ aStates ++ bStates
          newDelta n Nothing = [a, b]
          newDelta n _ = []
          newDelta s Nothing = if s `elem` aStates 
                               then if s == aAccepting  
                                    then [n'] ++ aDelta
                                    else aDelta
                               else if s == bAccepting
                                    then [n'] ++ bDelta
                                    else bDelta
          newDelta s c = if s `elem` aStates
                         then aDelta s c
                         else bDelta s c

ndfaFromRegex n (Star a) = (NDFA (n newStates n' newDelta), n' + 1)
    where (NDFA aS0 aStates aAccepting aDelta, newN) = ndfaFromRegex (n + 1) a
          newStates = [n, n'] ++ aStates
          newDelta n Nothing = [aS0, n']
          newDelta n _ = []
          newDelta s Nothing = if s == aAccepting 
                               then [aS0, n'] ++ (aDelta s Nothing)
                               else aDelta s Nothing
          newDelta s c = aDelta s c

ndfaFromRegex n (Seq a b) = (NDFA (n newStates n' newDelta), n' + 1)
    where (NDFA aS0 aStates aAccepting aDelta, m) = ndfaFromRegex n a
          (NDFA bS0 bStates n' bDelta, newN) = ndfaFromRegex m b
          newStates = [n'] ++ aStates ++ bStates
          newDelta s c = if s `elem` aStates
                         then aDelta s c
                         else bDelta s c

