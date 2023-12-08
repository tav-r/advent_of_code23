import Data.Vect
import Data.Maybe
import Data.List
import Data.String
import Data.Either
import Debug.Trace
import System


%default total


-- helper functions

transposeList : List (Maybe a) -> Maybe (List a)
transposeList [] = Just []
transposeList (Nothing :: xs) = Nothing
transposeList ((Just x) :: xs) = (Prelude.Basics.(::) x) <$> (transposeList xs)


goodMinus : Nat -> Nat -> Nat
goodMinus n 0 = n
goodMinus 0 m = 0
goodMinus (S n) (S m) = goodMinus n m


--- string stuff

strip : List Char -> List Char
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace


splitAll : List Char -> Char -> List (List Char)
splitAll [] c = [[]]
splitAll (x :: xs) c = if x == c then [] :: (splitAll xs c) else push x (splitAll xs c)
  where
    insertAtHead : Char -> List (List Char) -> Maybe (List (List Char))
    insertAtHead c css = do
      hList <- head' css
      tLists <- tail' css

      pure ((c :: hList) :: tLists)

    push : Char -> List (List Char) -> List (List Char)
    push c css = fromMaybe [[c]] (insertAtHead c css)


readTo : List Char -> (Char -> Bool) -> Maybe (List Char, List Char)
readTo [] f = Nothing
readTo (x :: xs) f = if f x then Just ([], xs) else push x <$> (readTo xs f)
  where
    push : Char -> (List Char, List Char) -> (List Char, List Char)
    push c (x, y) = (c :: x, y)

-- logic


NTime : Type
NTime = Nat


NRecord : Type
NRecord = Nat


Game : Type
Game = (NTime, NRecord)


getDistance : Nat -> Nat -> Nat
getDistance timeTot timePush  = timePush * (goodMinus timeTot timePush)


getNWaysToBeatRecord : Game -> Nat
getNWaysToBeatRecord (time, recDist) = goodMinus (S time) $ 2 * (length $ takeWhile (\x => getDistance time x <= recDist) [0..time])


parseLine : List Char -> Maybe (List Nat)
parseLine cs = do
  (_, parts) <- readTo cs (':'==)
  transposeList $ parsePositive . pack <$> (filter ((0/=) . length) $ splitAll parts ' ')


parseGames : List Char -> List Char -> Maybe (List Game)
parseGames l1 l2 = do
  ts <- parseLine l1
  recs <- parseLine l2

  pure $ zip ts recs 


solvePartOne : List Char -> List Char -> Maybe Nat
solvePartOne l1 l2 = map (product . map getNWaysToBeatRecord) $ parseGames l1 l2


-- main...
main : IO ()
main = do
  line1 <- unpack <$> getLine
  line2 <- unpack <$> getLine

  let res1 = solvePartOne line1 line2

  case res1 of
    Nothing => printLn "Error processing input"
    Just n => printLn $ n
