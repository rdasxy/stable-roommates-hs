import System.IO
import Debug.Trace
import Data.List

--
type Person = String
type Persons = [Person]
type PrefFn = Person -> [Person]
type Pairings = [(Person, Person)]

-- This is the main() function and entry-point (hence the name).
-- This function reads a given input file, builds the lists, solves the problem, and outputs the results to a file (or screen if no matching exists)
main = do 
        inh      <- getAndOpenFile "Enter the name of the input file: " ReadMode
        outh     <- getAndOpenFile "Enter the name of the output file: " WriteMode
        contents <- hGetContents inh
        let (persons, orig_prefs, new_prefs)  = solve contents
        if isValid persons orig_prefs new_prefs  -- checks if the resulting pairings are stable or not
        then output outh persons new_prefs
        else putStrLn "There is no stable matching." 
        hClose inh
        hClose outh

-- Prompt user for file name and open it. If the file does not exist or
-- cannot be opened with the requested mode, repeate until successful.
getAndOpenFile :: String -> IOMode -> IO Handle
getAndOpenFile prompt mode = 
    do putStrLn prompt
       filename <- getLine
       catch (openFile filename mode)
             (\_ -> do putStrLn ("Cannot open " ++ filename)
                       getAndOpenFile prompt mode)
                       
-- Returns true if the result represents a valid match
isValid :: Persons -> PrefFn -> PrefFn -> Bool
isValid [] origpref newpref = True
isValid (w:ws) orig_pref new_pref =
    (length (new_pref w) > 0) && (isValid ws orig_pref new_pref)
         
-- Formats the output and prints it to the output file
output:: Handle -> Persons -> PrefFn -> IO()
output h [] pref = do return ()
output h (w:ws) pref =
    do hPutStrLn h (w ++ " " ++ last (pref w))
       output h ws pref
         
-- Primary functiont hat takes a string read from input file, parses it, and 
-- delegates to the decycle and propose process for solution
solve :: String -> (Persons, PrefFn, PrefFn)
solve contents = 
    let prefs = map words $ lines contents
        pref = find_init prefs
        persons = map head prefs
        preffn = decycle persons $ propose persons [] pref
    in (persons, pref, preffn)

-- Builds the initial preference function
find_init :: [Persons] -> PrefFn
find_init l x | x == head (head l) = tail (head l)          -- call find against remainder of list
find_init l x                      = find_init (tail l) x   -- return the preference list for the current person

-- This function removes cycles, when necessary, from the preference list given the list of people to deal with,
-- and produces the preference list when either a stable matching is found or someone has no one remaining in their list
decycle :: Persons -> PrefFn -> PrefFn
decycle people preffn
  | dropWhile (\z -> [] /= (preffn z) ) people /= []     = preffn -- produce the current preference function, because at least one person's list has no one left
  | dropWhile (\z -> 2 > length (preffn z)) people /= [] = decycle people (propose remaining uninvited newPrefFn) -- remove a cycle from the preference lists, at least 1 person has more than 1 person in the preference list
  | otherwise                                            = preffn -- produce the current preference function, representing success, because everyone's list has 1 person
  where cycle     = removeCycle people [] [] preffn       -- idenfities a cycle in the given preference function
        newPrefFn = fst cycle   -- extracts the resulting new preference function from removeCycle 
        remaining = snd cycle   -- extracts the resulting remaining list of people to match up from removeCycle
        uninvited = people \\ remaining -- determines the list of people who have not received an invitation

-- Finds a cycle in the current preference function
removeCycle :: Persons -> Persons -> Persons -> PrefFn -> (PrefFn, Persons)
removeCycle people ps qs preffn
  | ps == []    = removeCycle people [first] [second (preffn first)] preffn     -- the first call of removeCycle kicks off with current p being the first person with a preference list > 1
  | p `elem` ps = (foldr (\(p,q) acc -> breakup acc p q) preffn pairs, invites) -- if a cycle is found, break up all the identified pairs and return the new preference list and list of people who need to invite a roommate
  | otherwise   = removeCycle people (p:ps) (q:qs) preffn                       -- no cycle has been found -> keep looking in next step
  where cycle_ps    = (p:takeWhile (/= p) ps)       -- cycle ps is the list of ps since last time current p occured, with current p added on the front
        cycle_qs    = (takeWhile (/= q) qs) ++ [q]  -- cycle qs is the list of qs since last time current q occurred, with current q added on the end
        p           = last (preffn (head qs))       -- current p = the person from whom the last q holds an invitation
        q           = second (preffn p)             -- current q = the second person in current p's preference list
        pairs       = zip cycle_ps cycle_qs         -- merges ps and qs into a list of pairings to break up
        first       = head (filter (\z -> (length (preffn z)) > 1) people) -- finds the first person with a preference list > 1
        invites     = map (\(x,y) -> x ) pairs      -- identifies the people who will need to make invites after removing the cycle
        second      = \l -> head $ tail l           -- simple function to get the 2nd item in a list (because I'm lazy)

-- this function applies a breakup case from handling a all-or-nothing cycle by removing each person
-- in a pair from the other's list
breakup :: PrefFn -> Person -> Person -> Person -> [Person]
breakup preffn proposer target lookup
  | lookup == target   = remove proposer (preffn lookup)   -- remove the proposer from the target's preference list
  | lookup == proposer = remove target (preffn lookup)     -- remove the target from the proposer's preference list
  | otherwise          = preffn lookup                     -- don't catch anyone else in the crossfire, return the normal preference function

-- this function manages the logic of calling
propose :: Persons -> Persons -> PrefFn -> PrefFn
propose [] invited preffn = preffn
propose (proposer:others) invited preffn
  | [] == (preffn proposer)         = preffn    -- if the current person has an empty preference list, we're dead, no matching is found
  | target `notElem` invited        = propose others (nub (target:invited)) accepted
  | proposer == target_invite       = propose others invited preffn -- If the current person is already accepted by their target, they're removed from the list of people who need to invite someone
  | otherwise                       = propose (target_invite:others) (target:(remove target invited)) accepted -- Jilt-case, the target's currently accepted is jilted, and the target accepts the current proposer
  where target        = head (preffn proposer)          -- who the current person will propose to
        target_invite = last (preffn target)            -- who the current target has accepted a proposal from
        accepted      = accept target proposer preffn   -- defines the preference function for the case of the target accepting

-- removes a person from th provided list
remove :: Person -> Persons -> Persons
remove _ []                 = []    -- Nothing to remove..!
remove l (x:xs) | x == l    = xs    -- If the current item needs to be removed, return the remaining list
                | otherwise = x : remove l xs -- Keep looking for the item to remove

-- once the proposed target person accepts the proposers proposal her preferences change.  
-- The target deletes everyone lower on her list than than the proposer
accept :: Person -> Person -> PrefFn -> PrefFn
accept target proposer preffn = let droppedList = tail (dropWhile (/= proposer) (preffn target))
                                in \lookup -> if lookup /= target 
                                              then filterRejector target droppedList preffn lookup       -- handle symmetric removal of people who target dropped
                                              else takeWhile (/= proposer) (preffn target) ++ [proposer] -- gets the preference list without everyone after the proposer

-- Handles symmetric removal of rejectors from preference lists of people they've knocked off their own list
filterRejector :: Person -> Persons -> PrefFn -> PrefFn
filterRejector rejector dropped preffn lookup = if lookup `elem` dropped -- remove the rejector if they removed me!
                                                then remove rejector $ preffn lookup
                                                else preffn lookup 
