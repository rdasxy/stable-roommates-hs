-- roommates2.hs
-- solve the stable roomates problem in Haskell, using Irving's algorithm
-- this version checks stability of the matching, for testing

-- The preferences are represented as a function of a person.  Given a person,
-- the function returns that person's preference list.  The algorithm is 
-- implemented by "modifying" this function.  When the algorithm terminates, if
-- a stable matching exists, each person's preference list has been reduced to
-- a singleton.

-- We modify Irving's algorithm slightly to keep the lists symmetric.  Whenever
-- A crosses B off his list, B crosses A off his.  Thus, all proposals are accepted.

import System.IO

noSolution = \z -> []
second = head.tail
type Person = String
type PrefFn = (Person->[Person])

main :: IO ()
main = do
       putStr "Please enter filename > "
       inf    <- getLine
       inh    <- openFile inf ReadMode
       outh   <- openFile ((takeWhile (/= '.') inf) ++ ".out.txt") WriteMode
       inpStr <- hGetContents inh
       let (stable, persons, prefs) = solve inpStr
       output outh stable persons prefs
       hClose inh
       hClose outh

solve :: Person -> (Bool, [Person], PrefFn )
solve str =
   let prefs    = map words (lines str)
       univ     = map head prefs
       pref     = find prefs
       result   = propose univ univ pref

       -- "bachelors" are people whose proposal is held by no one
       -- "spinsters" are those who hold no proposal
       -- if there are no bachelors, and all preference lists have length 1,
       -- the problem is solved
       -- otherwise, perform a rotation and continue

       propose :: [Person] -> [Person] -> PrefFn -> PrefFn
      
       propose [] _ pref
        | all (\z -> (length (pref z) == 1)) univ  = pref
        | otherwise                                = rotate pref
      
       -- if the first bachelor's list is empty, there is no solution
       -- when s accepts b's proposal s crosses off everyone on his list lower than b
       -- these people simulataneously cross s off their preference lists
      
       propose (b:bs) sp pref
        | null (pref b)   = noSolution
        | otherwise       = propose bs' sp' pref'
              where s     = head (pref b)
                    pref' = foldl crossOff pref pairs
                    pairs = [(s, p) | p <- tail (dropWhile (/= b) (pref s))]
                    sp'   = delete s sp
                    bs'   = if s `elem` sp
                            then bs
                            else (last (pref s)):bs
                            
       rotate::PrefFn -> PrefFn
       rotate pref = propose ps qs pref'
          where pairs = cycle [p] [q] pref
                p     = head [u | u <- univ, length (pref u) > 1]
                q     = second (pref p)
                pref' = foldl crossOff pref pairs
                ps    = map fst pairs
                qs    = map snd pairs
                
       -- Get an "all-or-nothing" cycle, as Irving calls it

       cycle::[Person] -> [Person] -> PrefFn -> [(Person, Person)]
       cycle ps (q:qs) pref
          | p `elem` ps = zip (p:(takeWhile (/= p) ps)) (q:qs)
          | otherwise   = cycle (p:ps) (q':q:qs) pref
              where  p  = last (pref q)
                     q' = second (pref p)
       
       wish::PrefFn
       wish p = takeWhile (/=q) (pref p)
                  where q = head (result p)

       stable = null (result (head univ)) ||
                null [p | p <-univ, any (p `elem`) (map wish (wish p))]


   in (stable, univ, result)

-- find takes a list of lists, and given the head of one of the
-- lists, returns the tail.  (If more than one list has the same head, it
-- returns the tail of the first such, but that has no application in this
-- problem.)  By partial application, we will specialize it, so that given a
-- person, it return his preference list.

find :: [[Person]] -> PrefFn
find l x
  | x == head (head l) = tail (head l)
  | otherwise          = find (tail l) x
  
-- crossOff: strike p1 off p2's preference list, and vice versa
  
crossOff ::  PrefFn -> (Person, Person) -> PrefFn
crossOff pref (p1, p2) z
  | z == p1    = delete p2 (pref p1)
  | z == p2    = delete p1 (pref p2)
  | otherwise  = pref z

delete :: Person -> [Person] -> [Person]
delete w ws = [z | z <-ws, z /= w]

output:: Handle -> Bool -> [Person] -> PrefFn -> IO()
output h stable [] pref = do return ()
output h stable (p:ps) pref
   | null (pref p) = do putStrLn "No solution"
                        return ()
   | otherwise     = do hPutStrLn h (p ++ " " ++ last (pref p))
                        output h stable ps pref
                        putStr (if stable then "" else "ERROR: Matching is unstable.\n")



