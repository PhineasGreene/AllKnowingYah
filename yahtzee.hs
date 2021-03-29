import Decisions
import Scoring
import Data.List
import Data.Ord

choice :: Bool -> String 
choice b = if b == keep then "Keep" else "Re-Roll"

addScore :: Card -> String -> Roll -> Card
addScore (Card a b c d e f g h i j k l m) fld roll = Card
    (if fld == "Aces" then scoreTop1 roll else a)
    (if fld == "Twos" then scoreTop2 roll else b)
    (if fld == "Threes" then scoreTop3 roll else c)
    (if fld == "Fours" then scoreTop4 roll else d)
    (if fld == "Fives" then scoreTop5 roll else e)
    (if fld == "Sixes" then scoreTop6 roll else f)
    (if fld == "Three of a Kind" then score3OfAKind roll else g)
    (if fld == "Four of a Kind" then score4OfAKind roll else h)
    (if fld == "Full House" then scoreFullHouse roll else i)
    (if fld == "Small Straight" then scoreSmStraight roll else j)
    (if fld == "Large Straight" then scoreLgStraight roll else k)
    (if fld == "Yahtzee" then scoreYahtzee roll else l)
    (if fld == "Chance" then scoreChance roll else m)

printField i = if i == -1 then "___" else show i

printCard (Card a b c d e f g h i j k l m) = do
     putStrLn $ "Aces            | " ++ (printField a)
     putStrLn $ "Twos            | " ++ (printField b)
     putStrLn $ "Threes          | " ++ (printField c)
     putStrLn $ "Fours           | " ++ (printField d)
     putStrLn $ "Fives           | " ++ (printField e)
     putStrLn $ "Sixes           | " ++ (printField f)
     putStrLn $ "Three of a Kind | " ++ (printField g)
     putStrLn $ "Four of a Kind  | " ++ (printField h)
     putStrLn $ "Full House      | " ++ (printField i)
     putStrLn $ "Small Straight  | " ++ (printField j)
     putStrLn $ "Large Straight  | " ++ (printField k)
     putStrLn $ "Yahtzee         | " ++ (printField l)
     putStrLn $ "Chance          | " ++ (printField m)

isRoll [a,b,c,d,e] = a>0 && b>0 && c>0 && d>0 && e>0 && a<7 && b<7 && c<7 && d<7 && e<7
isRoll _ = False

getRoll = do
     putStrLn "Enter the numbers on the dice (separated by spaces):"
     userDice <- getLine

     let roll = map read (words (userDice :: String))

     if isRoll roll then return (roll) else getRoll

getRollsLeft = do
     putStrLn "Rolls left: " 
     rlsLeft <- getLine
     let rollsLeft = read rlsLeft :: Int

     if rollsLeft >= 0 && rollsLeft < 3 then return (rollsLeft) else getRollsLeft


loop card rollsLeft = do
     dice <- getRoll
     let userRoll = dice :: Roll
     
     let userScoreFunc roll = scoreRoll roll card

     if rollsLeft > 0 then do
        putStrLn "Calculating best decision..."

        let userBestDecision = maximumBy (comparing snd) (scoreDecisions userRoll rollsLeft userScoreFunc)
        let result = map choice (fst userBestDecision)

        putStrLn $ "    " ++ result !! 0 ++ " Die 1"
        putStrLn $ "    " ++ result !! 1 ++ " Die 2"
        putStrLn $ "    " ++ result !! 2 ++ " Die 3"
        putStrLn $ "    " ++ result !! 3 ++ " Die 4"
        putStrLn $ "    " ++ result !! 4 ++ " Die 5"
        putStrLn $ "Average game score after this roll: " ++ (show (snd userBestDecision))

        loop card (rollsLeft - 1)
     else do
        let field = takeField userRoll card
        let score = scoreRoll userRoll card
        let newCard = addScore card field userRoll
        putStrLn $ "    Take as " ++ field
        printCard newCard
        putStrLn $ "Average game score after this roll: " ++ (show score)
     
        loop newCard 2

main = loop blank 2
