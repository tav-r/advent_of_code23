import Data.Vect
import Data.Maybe
import Data.String
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


enumerate : List a -> Nat -> List (Nat, a)
enumerate xs n = zip (nums (length xs) n) xs
  where
    nums : Nat -> Nat -> List Nat
    nums n m  = take n [m..]


--- string stuff


strip : List Char -> List Char
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace


startsWith : List Char -> List Char -> Bool
startsWith [] ds = False
startsWith (x :: xs) [] = True
startsWith (x :: xs) (y :: ys) = if x == y then startsWith xs ys else False


endsWith : List Char -> List Char -> Bool
endsWith cs ds = startsWith (reverse cs) (reverse ds)


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


readTo : List Char -> Char -> Maybe (List Char, List Char)
readTo [] c = Nothing
readTo (x :: xs) c = if x == c then Just ([], xs) else push x <$> (readTo xs c)
  where
    push : Char -> (List Char, List Char) -> (List Char, List Char)
    push c (x, y) = (c :: x, y)


-- type definitions

record CubeSet where
  constructor MkCubeSet
  red : Nat
  green : Nat
  blue : Nat


Show CubeSet where
  show (MkCubeSet red green blue) =
    joinBy "" ["CubeSet(", (show red), ",", (show green), ",", (show blue), ")"]


-- logic

--- parsing

buildCubeSet : List Char -> Maybe CubeSet
buildCubeSet str = do
  (numStr, colorStr) <- readTo str ' '
  num <- (parsePositive . pack) numStr

  let cs = if colorStr == (unpack "red") then MkCubeSet num 0 0
                                         else if colorStr == (unpack "green") then MkCubeSet 0 num 0
                                         else MkCubeSet 0 0 num

  Just cs


buildMaxCubeSet : List CubeSet -> CubeSet
buildMaxCubeSet = foldr step (MkCubeSet 0 0 0)
  where
    step : CubeSet -> CubeSet -> CubeSet
    step (MkCubeSet red green blue) (MkCubeSet k j i) = MkCubeSet a b c
      where
        a : Nat
        a = max red k

        b : Nat
        b = max green j

        c : Nat
        c = max blue i


getCubeSet : List Char -> Maybe CubeSet
getCubeSet = map buildMaxCubeSet . transposeList . map buildCubeSet . map strip . (flip splitAll) ','


parseLine : List Char -> Maybe (List CubeSet)
parseLine str = do
  (_, str) <- readTo str ' '
  (gameNrStr, str) <- readTo str ':'
  gameNr <- (parsePositive . pack) gameNrStr

  let cubeSetStrs = strip <$> splitAll str ';'

  transposeList $ (getCubeSet <$> cubeSetStrs) 


--- calculations

availableCubes : CubeSet
availableCubes = MkCubeSet 12 13 14


testPossible : CubeSet -> Bool
testPossible (MkCubeSet red green blue) = case (red <= a, green <= b, blue <= c) of
  (True, (True, True)) => True
  (_, (_, _)) => False
    where
      a : Nat
      a = availableCubes.red

      b : Nat
      b = availableCubes.green

      c : Nat
      c = availableCubes.blue


testLine : String -> Maybe Bool
testLine = map (testPossible . buildMaxCubeSet) . parseLine . unpack


getSumPlausibleGames : List String -> Maybe Nat
getSumPlausibleGames = process . map testLine
  where
    sumUpIndices : List Bool -> Nat
    sumUpIndices = sum . (map fst) . (filter snd) . (flip enumerate 1) 

    process : List (Maybe Bool) -> Maybe Nat
    process = map sumUpIndices . transposeList


-- main...

main : IO ()
main = do
  args <- getArgs

  let nlines = fromMaybe 1000 $ head' (drop 1 args) >>= (parsePositive {a = Nat})

  inputLines <- getNLines nlines

  let res = getSumPlausibleGames $ toList inputLines

  case res of
    Nothing => printLn "Error reading input"
    Just n => printLn $ n
