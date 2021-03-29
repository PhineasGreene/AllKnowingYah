module Scoring where

import Decisions
import Data.List
import Data.Ord

data Card = Card { ones       :: Float,
                   twos       :: Float,
                   threes     :: Float,
                   fours      :: Float,
                   fives      :: Float,
                   sixes      :: Float,
                   threeOAC   :: Float,
                   fourOAC    :: Float,
                   fullHouse  :: Float,
                   smStraight :: Float,
                   lgStraight :: Float,
                   yahtzee    :: Float,
                   chance     :: Float
                 } deriving (Show)

-- turn a card into a list
listCard :: Card -> [Float]
listCard c = [ones c, twos c, threes c, fours c, fives c, sixes c, threeOAC c, fourOAC c, fullHouse c, smStraight c, lgStraight c, yahtzee c, chance c]

-- total score from a card
total :: Card -> Float
total c = (sum cardl) + bonus
      where bonus = if sum (take 6 cardl) >= 63 then 35 else 0
            cardl = listCard c

-- Scoring functions.
scoringFuncs :: [(Roll -> Float)]
scoringFuncs = [scoreTop1, scoreTop2, scoreTop3, scoreTop4, scoreTop5, scoreTop6, score3OfAKind, score4OfAKind, scoreFullHouse, scoreSmStraight, scoreLgStraight, scoreYahtzee, scoreChance]

scoreTop :: Roll -> Int -> Float
score3OfAKind, score4OfAKind, scoreFullHouse, scoreSmStraight, scoreLgStraight, scoreYahtzee, scoreChance :: Roll -> Float

scoreTop ds n = fromIntegral $ (countN ds n) * n
scoreTop1 ds = scoreTop ds 1
scoreTop2 ds = scoreTop ds 2
scoreTop3 ds = scoreTop ds 3
scoreTop4 ds = scoreTop ds 4
scoreTop5 ds = scoreTop ds 5
scoreTop6 ds = scoreTop ds 6
score3OfAKind ds = if maxDupes ds >= 3 then fromIntegral (sum ds) else 0
score4OfAKind ds = if maxDupes ds >= 4 then fromIntegral (sum ds) else 0
scoreFullHouse ds = if maxDupes ds == 3 && minDupes ds == 2 then 25 else 0 
scoreSmStraight ds = if has [1,2,3,4] || has [2,3,4,5] || has [3,4,5,6] then 30 else 0
      where has []     = True
            has (n:ns) = countN ds n >= 1 && has ns
scoreLgStraight ds = if has [1,2,3,4,5] || has [2,3,4,5,6] then 40 else 0
      where has []     = True
            has (n:ns) = countN ds n >= 1 && has ns
scoreYahtzee [1,1,1,1,1] = 50
scoreYahtzee [2,2,2,2,2] = 50
scoreYahtzee [3,3,3,3,3] = 50
scoreYahtzee [4,4,4,4,4] = 50
scoreYahtzee [5,5,5,5,5] = 50
scoreYahtzee [6,6,6,6,6] = 50
scoreYahtzee _ = 0
scoreChance ds = fromIntegral $ sum ds

-- Average score in a given card slot.
averageSlot :: (Roll -> Float) -> Float
averageSlot scrf = averageScore $ map wrapper rollspace5
      where wrapper (r,i) = (bestDecision $ scoreDecisions r 2 scrf, fromIntegral i)

-- Following averages are constant and are found using above function.
averageCard = Card average1 average2 average3 average4 average5 average6 average3OfAKind average4OfAKind averageFullHouse averageSmStraight averageLgStraight averageYahtzee averageChance
average1 = 2.096571
average2 = 4.193142
average3 = 6.289713
average4 = 8.386284
average5 = 10.482854
average6 = 12.579426
average3OfAKind = 15.925446
average4OfAKind = 6.482461
averageFullHouse = 10.030978
averageSmStraight = 17.193275
averageLgStraight = 18.808998
averageYahtzee = 3.4645507
averageChance = 23.243835

-- Fill in empty fields with average values.
predictCard :: Card -> Float
predictCard (Card a b c d e f g h i j k l m) = total $ Card (p a (average1)) (p b (average2)) (p c (average3)) (p d (average4)) (p e (average5)) (p f (average6)) (p g (average3OfAKind)) (p h (average4OfAKind)) (p i (averageFullHouse)) (p j (averageSmStraight)) (p k (averageLgStraight)) (p l (averageYahtzee)) (p m (averageChance))
      where p :: Float -> Float -> Float
            p a b = if a == (-1) then b else a

blank = Card (-1)(-1)(-1)(-1)(-1)(-1)(-1)(-1)(-1)(-1)(-1)(-1)(-1)

cardFields = ["Aces", "Twos", "Threes", "Fours", "Fives", "Sixes", "Three of a Kind", "Four of a Kind", "Full House", "Small Straight", "Large Straight", "Yahtzee", "Chance"]

rollGameScores :: Roll -> Card -> [Float]
rollGameScores roll (Card a b c d e f g h i j k l m)= [
    if a == (-1) then predictCard (Card (scoreTop1 roll) b c d e f g h i j k l m) else 0,
    if b == (-1) then predictCard (Card a (scoreTop2 roll) c d e f g h i j k l m) else 0,
    if c == (-1) then predictCard (Card a b (scoreTop3 roll) d e f g h i j k l m) else 0,
    if d == (-1) then predictCard (Card a b c (scoreTop4 roll) e f g h i j k l m) else 0,
    if e == (-1) then predictCard (Card a b c d (scoreTop5 roll) f g h i j k l m) else 0,
    if f == (-1) then predictCard (Card a b c d e (scoreTop6 roll) g h i j k l m) else 0,
    if g == (-1) then predictCard (Card a b c d e f (score3OfAKind roll) h i j k l m) else 0,
    if h == (-1) then predictCard (Card a b c d e f g (score4OfAKind roll) i j k l m) else 0,
    if i == (-1) then predictCard (Card a b c d e f g h (scoreFullHouse roll) j k l m) else 0,
    if j == (-1) then predictCard (Card a b c d e f g h i (scoreSmStraight roll) k l m) else 0,
    if k == (-1) then predictCard (Card a b c d e f g h i j (scoreLgStraight roll) l m) else 0,
    if l == (-1) then predictCard (Card a b c d e f g h i j k (scoreYahtzee roll) m) else 0,
    if m == (-1) then predictCard (Card a b c d e f g h i j k l (scoreChance roll)) else 0
    ]

-- Score a roll based on projected game score.
scoreRoll :: Roll -> Card -> Float
scoreRoll roll card = maximum $ rollGameScores roll card

-- Where is it best to take a roll.
takeField :: Roll -> Card -> String
takeField roll card = fst $ best (zip cardFields (rollGameScores roll card))
      where best x = maximumBy (comparing snd) x
