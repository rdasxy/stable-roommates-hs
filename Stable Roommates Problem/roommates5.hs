--Inputs: text file with n lines each consisting of n names, where n is even
--outputs: output.txt, containing either the stable matching (if it exists), or "No stable matching exists."
--Purpose: We implement a version of Robert W. Irving's algorithm to solve the "stable roommates problem,"
	--modified for a functional-programming paradigm.

import System.IO
import Data.List   --this module is needed for the list-difference operator '\\'
type Person = String
type Roommates = [Person]
type Cycle = [Person]
type PrefFn = Person -> [Person]

--defining 'Answer' allows us to deal with the possibility of an unstable matching while
--keeping our code similar to that of the stable marriage problem
data Answer =
    Stable PrefFn |
    Unstable String

--main is essentially the same as in the Stable marriage code
main :: IO ()
main = do
	putStr "Please enter file name > "
	fileName <- getLine
	inh <- openFile fileName ReadMode
	outh <- openFile "output.txt" WriteMode
	inpStr <- hGetContents inh
	let (roommates, answer) = solve inpStr
	output outh roommates answer
	hClose inh
	hClose outh	

solve :: String -> (Roommates, Answer)
solve str =
    let prefs = map words (lines str)
	roommates = map head prefs
	pref = retrieve prefs
	result = propose roommates pref
    in (roommates, result)

--since we imported the List module, which has a built in 'find' function, we had to rename 'find' to
--'retrieve'. The code is otherwise the same.
retrieve :: [[Person]] -> PrefFn	
retrieve l x
	|x == head (head l) = tail (head l)
	|otherwise = retrieve (tail l) x

accept :: Person -> Person -> PrefFn -> PrefFn
accept x y pref = \z -> if z /= y
            		then acceptReturn rejects y z pref
            		else takeWhile (/=x) (pref y) ++ [x]
	where rejects = tail (dropWhile (/= x) (pref y)) -- all those people lower than x on y's pref list

acceptReturn :: Roommates -> Person -> Person -> PrefFn -> Roommates
acceptReturn rs y z pref 
	|z `elem` rs = takeWhile (/=y) (pref z) ++ tail (dropWhile (/=y) (pref z)) --deletes y from z's pref list
	|otherwise = pref z


crossOff :: PrefFn -> (Person, Person) -> PrefFn
crossOff pref (a, b) = \z -> if z /= b
			     then pref z
			     else takeWhile (/= a) (pref b) ++ tail(dropWhile (/=a) (pref b))
--as with 'find', since we imported the List module, we had to rename 'delete' as 'remove'.
remove :: (Eq a) => a -> [a] -> [a]	
remove r rs = [z | z <-rs, z /= r]

--reject is a pref function which takes a person and returns their pref list minus the first element
reject :: Person -> PrefFn -> PrefFn
reject r pref = \z -> if z /= r
		then pref z
		else tail (pref r)

--phase1 performs a function similar to that of 'propose' in the 'Stable Marriage' code
phase1 :: Roommates -> Roommates -> PrefFn -> PrefFn
phase1 [] _ pref = pref
phase1 (r1: rs) fr pref
	| r2 == "Nobody" = phase1 [] fr pref --if a person's pref list is empty, we jump to the base-case of phase1	
	| r2 `elem` fr && r1 `elem` (pref r2) = phase1 rs (remove r2 fr) (accept r1 r2 pref)
	| r1 `elem` (pref r2) = phase1 (r3 : rs) fr (accept r1 r2 pref)
	| otherwise = phase1 (r1 : rs) fr (reject r1 pref)
	where r2 = if null (pref r1) --this assumes that no person on the input file has the name 'Nobody'.
                then "Nobody"        --this check must be done to account for a roommate having an empty
                else head (pref r1)  --pref list, which was not possible in the 'Stable Marriage' code.
	      r3 = if r2 == "Nobody"			
                then "Nobody"			
                else last (pref r2)

--phase2 functions start here--------
doubleCross :: PrefFn -> (Person, Person) -> PrefFn
doubleCross pref (x,y) = crossOff (crossOff pref (x,y)) (y,x) --performs a two-way crossOff

--since each q may be defined in relation to it's corresponding p (qk is the first person on pk's pref list),
--we only build the p-list for use in phase2.
pBuilder :: Person -> PrefFn -> Cycle -> Cycle
pBuilder r pref (p:pList)
	|p `elem` pList = p:(takeWhile (/=p) pList) --base-case: a cycle has been found in the pList
	|otherwise = pBuilder nextP pref (nextP:(p:pList)) 
	where nextP = last (pref (last(take 2 (pref r)))) --nextP is the last person on the pref list of
                                                          --the second person on r's pref list 
--with a pre-generated pList, killcyc will cross every p off of it's corresponding q's list, and vice versa
killcyc :: Cycle -> PrefFn -> PrefFn
killcyc [] pref = pref
killcyc pList pref = killcyc (tail pList) (doubleCross pref ((head pList), (head(pref(head pList)))))

--phase2 calls propose but passes it a modified pref-function which reflects the necessary
--second phase changes to the preference lists
phase2 :: Roommates -> PrefFn -> Answer 
phase2 rs pref = propose rs mPref
	where mPref = killcyc (pBuilder (head rs) pref [(head rs)]) pref
--returns the list of all those roommates whose pref lists contain more than one person
noOnes :: Roommates -> PrefFn -> Roommates
noOnes rs pref = [x|x <- rs, length (pref x) > 1]

--phase2 functions end here----------

--propose is the 'main' function of the program. The three guards correspond to:
--i) somebody's pref list is of length 0, hence no stable matching is possible;
--ii) every pref list is of length 1, hence we return the corresponding stable matching;
--iii) some pref lists contain more than 1 person, hence we must execute phase2, which in turn re-calls propose
--Observe that phase1 is necessarily executed every time propose is called, but phase2 is not necessarily called.
propose :: Roommates -> PrefFn -> Answer
propose rs pref
	| 0 `elem` [length (modifiedPrefs r)| r<-rs] = (Unstable "No stable matching exists.")
	| null ([length (modifiedPrefs r)| r<-rs] \\ [ 1 | _<- [1..(length (rs))]]) = (Stable modifiedPrefs) -- \\ is the "list-difference" operator
	| otherwise = phase2 (noOnes rs modifiedPrefs) modifiedPrefs 
	where modifiedPrefs = phase1 rs rs pref
--'output' pattern matches depending on whether Answer is of type Stable or Unstable		
output:: Handle -> Roommates -> Answer-> IO()
output h [] (Stable pref)= do return()
output h (r:rs) (Stable pref)= do hPutStrLn h (r++" "++head(pref r))
      				  output h rs (Stable pref)
output h rm (Unstable msg)=hPutStr h (msg)
