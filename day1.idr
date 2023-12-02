import Data.Vect
import Data.Maybe
import Data.String
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


transposeList : List (Maybe Nat) -> Maybe (List Nat)
transposeList [] = Just []
transposeList (Nothing :: xs) = Nothing
transposeList ((Just x) :: xs) = (Prelude.Basics.(::) x) <$> (transposeList xs) 


transposeTuple : (Maybe a, Maybe b) -> Maybe (a, b)
transposeTuple ((Just x), (Just y)) = Just (x, y)
transposeTuple _ = Nothing


enumerate : List a -> List (Nat, a)
enumerate xs = zip (nums $ length xs) xs
  where
    nums : Nat -> List Nat
    nums n  = take n [0..]


-- logic
listW : List (List Char)
listW = unpack <$> [
  "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
]


listN : List (List Char)
listN = unpack <$> ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]


-- first starts with second
startsWith : Eq a => List a -> List a -> Bool
startsWith xs [] = True
startsWith [] (x :: xs) = False
startsWith (x :: xs) (y :: ys) = if x == y then startsWith xs ys else False


getFirstMatch : List (List Char) -> List Char -> Maybe (List Char)
getFirstMatch strs [] = Nothing
getFirstMatch strs str@(x :: xs) = if isJust res then res else recurse
  where
    res : Maybe (List Char)
    res = find (startsWith str) strs

    recurse : Maybe (List Char)
    recurse = getFirstMatch strs xs


getLastMatch : List (List Char) -> List Char -> Maybe (List Char)
getLastMatch strs str = reverse <$> getFirstMatch (reverse <$> strs) (reverse str)


parseString : List Char -> Maybe Nat
parseString str = case str of
  [] => Nothing
  (x :: xs) => if isDigit x then parsePositive (pack [x]) else parseWord str
    where
      parseWord : List Char -> Maybe Nat
      parseWord str = fst <$> find (\(_, s) => startsWith s str) (enumerate listW)


calibrationValueFromLine : String -> Maybe Nat
calibrationValueFromLine str = (uncurry (+)) <$> transposeTuple ((*10) <$> firstDigit, secondDigit)
  where
    strs : List (List Char)
    strs = listW ++ listN

    ustr : List Char
    ustr = unpack str

    firstDigit : Maybe Nat
    firstDigit = parseString =<< getFirstMatch strs ustr

    secondDigit : Maybe Nat
    secondDigit = parseString =<< getLastMatch strs ustr


getNumbers : Vect _ String -> Maybe Nat
getNumbers = (map sum) . (transposeList . (map calibrationValueFromLine) . toList)


main : IO ()
main = do
  args <- getArgs

  let nlines = fromMaybe 1000 $ head' (drop 1 args) >>= (parsePositive {a = Nat})

  inputLines <- getNLines nlines

  let res = getNumbers inputLines

  case res of
    Nothing => printLn "Error reading numbers"
    Just n => printLn $ n
