import Data.Vect
import Data.Maybe
import Data.String
import Data.Either
import Debug.Trace
import System


%default total


-- helper functions

getNLines : (n : Nat) -> IO (Vect n String)
getNLines 0 = do
  pure []
getNLines (S k) = do
  l <- getLine
  ls <- getNLines k

  pure (l :: ls)


transposeList : List (Maybe a) -> Maybe (List a)
transposeList [] = Just []
transposeList (Nothing :: xs) = Nothing
transposeList ((Just x) :: xs) = (Prelude.Basics.(::) x) <$> (transposeList xs)


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


getWinningNumbers : List Char -> Maybe (List Nat)
getWinningNumbers cs = do
  (_, numbers) <- readTo cs (':' ==)
  (left, right) <- readTo numbers ('|' ==)

  leftNums <- transposeList $ map (parsePositive . pack) $ (filter ((0 <) . length) $ strip <$> splitAll left ' ')
  rightNums <- transposeList $ map (parsePositive. pack ) $ (filter ((0 <) . length)$ strip <$> splitAll right ' ')

  Just (filter (`elem` leftNums) rightNums)


solvePartOne : List (List Char) -> Maybe Nat
solvePartOne = map (sum . map (toPoints)) . transposeList . map getWinningNumbers
  where
    toPoints : List Nat -> Nat
    toPoints [] = 0
    toPoints (x :: Nil) = 1
    toPoints (x :: xs) = 2 * toPoints xs


solvePartTwo : List (List Char) -> Maybe Nat
solvePartTwo =  map calculatePts . transposeList . map getWinningNumbers
  where
    calculatePtsHelp : List (List Nat) -> List (List Nat) -> Nat
    calculatePtsHelp [] [] = 0
    calculatePtsHelp [] (x :: xs) = 0
    calculatePtsHelp (x :: xs) [] = 1 + calculatePtsHelp xs []
    calculatePtsHelp (x :: xs) (y :: ys) =
      1 + calculatePtsHelp (take (length x) ys) ys + (calculatePtsHelp xs ys)

    calculatePts : List (List Nat) -> Nat
    calculatePts kss = calculatePtsHelp kss kss


-- main...
main : IO ()
main = do
  args <- getArgs

  let nlines = fromMaybe 1000 $ head' (drop 1 args) >>= (parsePositive {a = Nat})

  inputLines <- getNLines nlines

  let res1 = (solvePartOne . map unpack . toList) inputLines

  case res1 of
    Nothing => printLn "Error processing input"
    Just n => printLn $ n

  let res2 = (solvePartTwo . map unpack . toList) inputLines

  case res2 of
    Nothing => printLn "Error processing input"
    Just n => printLn $ n
