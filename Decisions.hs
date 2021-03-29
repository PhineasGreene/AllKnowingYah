module Decisions where

type Roll = [Int]
type Decision = [Bool]
keep = True
rerl = False

-- Merge rolls that occur multiple times with the number of times they occur.
mergeDupes :: [Roll] -> [(Roll, Int)]
mergeDupes rs = worker (zipDupes rs)
      where isnt     :: (Roll, Int)   -> (Roll, Int)   -> Bool
            f        :: [(Roll, Int)] -> [(Roll, Int)]
            zipDupes :: [Roll]        -> [(Roll, Int)]
            worker   :: [(Roll, Int)] -> [(Roll, Int)]
            isnt r1 r2    = (dupes (fst r1)) /= (dupes (fst r2))
            f (r:rs)      = filter (\e -> isnt r e) rs
            zipDupes   rs = zip rs (map (\e -> countDupes rs e) rs)
            worker []     = []
            worker (r:[]) = [r]
            worker (r:rs) = r:(worker (f (r:rs)))

-- The rollspace (or all possible rolls) for a given number of dice. Duplicates are merged and counted ie. (Roll, NumDupes).
rollspace :: Int -> [(Roll, Int)]
rollspace depth = mergeDupes $ worker [[]] depth
      where m       :: [Roll] -> Int    -> [Roll]
            addRoll :: [Roll] -> [Roll]
            worker  :: [Roll] -> Int    -> [Roll]
            m xs n      = map (\e -> n:e) xs
            addRoll xs  = m xs 1 ++ m xs 2 ++ m xs 3 ++ m xs 4 ++ m xs 5 ++ m xs 6
            worker xs 0 = xs
            worker xs i = worker (addRoll xs) (i-1)

-- Lookup without re-evaluating.
rollspace1 = rollspace 1
rollspace2 = rollspace 2
rollspace3 = rollspace 3
rollspace4 = rollspace 4
rollspace5 = rollspace 5

rollspaceN :: Int -> [(Roll, Int)]
rollspaceN 0 = []
rollspaceN 1 = rollspace1
rollspaceN 2 = rollspace2
rollspaceN 3 = rollspace3
rollspaceN 4 = rollspace4
rollspaceN 5 = rollspace5
rollspaceN n = rollspace n

-- All posible decisions with depth number of dice.
decisionspace :: Int -> [Decision]
decisionspace depth = worker [[]] depth
      where addDie :: [Decision] -> [Decision] 
            worker :: [Decision] -> Int        -> [Decision]
            addDie xs   = (map (\e -> rerl:e) xs) ++ (map (\e -> keep:e) xs)
            worker xs 0 = xs
            worker xs i = worker (addDie xs) (i-1)

decisionspace5 = decisionspace 5

-- Average the numbers in a list.
--average :: [Int] -> Float
--average xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))
--averageF :: [Float] -> Float
--averageF xs = (sum xs) / (fromIntegral (length xs))

-- How many dice in a roll are of number n.
countN :: Eq a => [a] -> a -> Int
countN ds n = worker ds 0 
      where worker []     i = i
            worker (d:ds) i = if d == n then worker ds (i+1) else worker ds i

-- Count rolls with the same number of dice of a given number of pips.
countDupes :: [Roll] -> Roll -> Int
countDupes ds n = worker ds 0 
      where worker []     i = i
            worker (d:ds) i = if dupes d == dupes n then worker ds (i+1) else worker ds i

-- How many times is a duplicated number duplicated.
dupes :: Roll -> [Int]
dupes ds = [c 1, c 2, c 3, c 4, c 5, c 6]
      where c n = countN ds n
maxDupes :: Roll -> Int
maxDupes ds = maximum $ map (\e -> countN ds e) ds
minDupes :: Roll -> Int
minDupes ds = minimum $ map (\e -> countN ds e) ds

-- Return the kept dice after a decision is made.
kept :: Roll -> Decision -> Roll
kept r d = map fst (filter snd (zip r d))

-- Possible rolls after a decision.
rollsAfter :: Roll -> Decision -> [(Roll, Int)]
rollsAfter strt d = map merge space
     where space :: [(Roll, Int)]
           merge :: (Roll, Int)   -> (Roll, Int)
           k     :: Roll
           k            = kept strt d
           space        = rollspaceN (countN d rerl)
           merge (r, n) = (k ++ r, n) 

-- Average the scores in a list of the the form [(Score, Occurances)]
averageScore  :: [(Float, Float)]  -> Float
averageScore []     = 0
averageScore (r:[]) = fst r
averageScore rs     = (sum rs) / (count rs)
      where count :: [(Float, Float)] -> Float
            sum   :: [(Float, Float)] -> Float
            count (r:[]) = snd r
            count (r:rs) = (snd r) + (count rs)
            sum (r:[])   = (fst r) * (snd r)
            sum (r:rs)   = ((fst r) * (snd r)) + (sum rs)

-- Average a list of numbers.
average :: Fractional a => [a] -> a
average []     = 0
average (x:[]) = x
average xs     = (sum xs) / (fromIntegral (length xs))

-- Score a roll. Output takes the form (Score, Occurances)
scoreRollWith :: (Roll, Int) -> (Roll -> Float) -> (Float, Float)
scoreRollWith (r,n) scrf = (scrf r, fromIntegral n)

-- Score a set of rolls and return the average score.
scoreRollSet  :: [(Roll, Int)] -> (Roll -> Float) -> Float
scoreRollSet set scrf = averageScore $ map (\e -> scoreRollWith e scrf) set

-- Return the best decision's score in a list of decisions in form (Decision, Score).
bestDecision :: [(Decision, Float)] -> Float
bestDecision ds = worker ds ([], 0)
     where worker :: [(Decision, Float)] -> (Decision, Float) -> Float
           worker [] m           = snd m
           worker (d:[]) m = if snd d > snd m then snd d else snd m
           worker (d:ds) m = if snd d > snd m then worker ds d else worker ds m

-- Average score after a decision.
scoreDecision      :: Decision -> Roll -> (Roll -> Float) -> Float
scoreDecision [True,True,True,True,True] strt scrf = fst $ scoreRollWith (strt, 1) scrf
scoreDecision d strt scrf = scoreRollSet (rollsAfter strt d) scrf

-- The maximum decision score in a decisionspace.
--scoreDecisionSpace :: [Decision] -> Roll -> (Roll -> Float) -> Float
--scoreDecisionSpace ds strt scrf = maximum $ map scrD ds
--      where scrD d = scoreDecision d strt scrf

-- Best decision for a given starting roll, remaining rolls and scoring function.
scoreDecisions :: Roll -> Int -> (Roll -> Float) -> [(Decision, Float)]
scoreDecisions strt remn scrf
      | remn == 1 = zip (decisionspace5) (map scrD (decisionspace5))
      | remn == 2 = map (\e -> (e, scrRlsAftr e)) decisionspace5 
      | otherwise = undefined
      where scrD       :: Decision   -> Float
            scrDs      :: Roll       -> Float
            scrRlsAftr :: Decision   -> Float
            -- Wrapper around scoreDecision that passes scoring function and starting roll.
            scrD d   = scoreDecision d strt scrf
            -- Wrapper around recursing once that can handle a roll space.
            scrDs s = (bestDecision (scoreDecisions s 1 scrf))
            -- Score all possible rolls after a given decision.
            scrRlsAftr d = average $ (thisRoll):(map scrDs (map fst (rollsAfter strt d)))
            thisRoll = fst (scoreRollWith (strt,0) scrf)

