import Data.String
import Data.List
import Data.Nat
import Debug.Trace
import Data.Maybe
import System
import Data.Vect

%default total

testInput : String
testInput = """
  ...#......
  .......#..
  #.........
  ..........
  ......#...
  .#........
  .........#
  ..........
  .......#..
  #...#.....
  """

getNLines : (n : Nat) -> IO (Vect n String)
getNLines 0 = do
  pure []
getNLines (S k) = do
  l <- getLine
  ls <- getNLines k

  pure (l :: ls)

maxElt : List Nat -> Nat
maxElt = foldr maximum 0

elt : Eq a => a -> List a -> Bool
elt y [] = False
elt y (x :: xs) = if x == y then True else elt y xs

Coordinate : Type
Coordinate = (Nat, Nat)

GalaxyImage : Type
GalaxyImage = List Coordinate

-- we have to use Integer subtraction here, recursively
-- calculating the difference between Nats is too slow
diffNat : Nat -> Nat -> Integer
diffNat k j = abs $ (natToInteger k) - (natToInteger j)

dist2D : Coordinate -> Coordinate -> Integer
dist2D (x, z) (y, w) = diffNat x y + diffNat z w

parseGalaxyImage : List String -> Nat -> GalaxyImage
parseGalaxyImage [] _ = []
parseGalaxyImage (x :: xs) j =
  parseLine lineCharList j 0 ++ parseGalaxyImage xs (j+1)
  where
    lineCharList : List Char
    lineCharList = unpack x

    parseLine : List Char -> Nat -> Nat -> List Coordinate
    parseLine [] j k = []
    parseLine (y :: ys) j k = nextList ++ parseLine ys j (k+1)
      where
        nextList : List Coordinate
        nextList = if y == '#' then [(j, k)] else []

cosmicExpansion : GalaxyImage -> GalaxyImage
cosmicExpansion xs = (insertRows . insertCols) xs
  where
    rows : List Nat
    rows = map snd xs

    cols : List Nat
    cols = map fst xs

    lastRow : Nat
    lastRow = maxElt rows

    lastCol : Nat
    lastCol = maxElt cols

    emptyRows : List Nat
    emptyRows = filter (not . (flip elt) rows) [0..lastRow]

    emptyCols : List Nat
    emptyCols = filter (not . (flip elt) cols) [0..lastCol]

    condInc : Nat -> Nat -> Nat
    condInc x k = if x > k then x + 999999 else x  -- change number to 1 for part 1

    addRowIfLower : Nat -> List (Nat, Nat) -> List (Nat, Nat)
    addRowIfLower k = map (\(a, b) => (a, condInc b k))

    addColIfLower : Nat -> List (Nat, Nat) -> List (Nat, Nat)
    addColIfLower k = map (\(a, b) => (condInc a k, b))

    insertRows : GalaxyImage -> GalaxyImage
    insertRows = (flip $ foldr addRowIfLower) emptyRows

    insertCols : GalaxyImage -> GalaxyImage
    insertCols = (flip $ foldr addColIfLower) emptyCols

combinations : List a -> List (a, a)
combinations [] = []
combinations (x :: xs) = [(x, y) | y <- xs] ++ combinations xs

distances : GalaxyImage -> List Integer
distances xs = 
  map (uncurry dist2D) $ filter (\(a, b) => a /= b) (combinations xs)

solve : List String -> Integer
solve = sum . distances . cosmicExpansion . (flip parseGalaxyImage) 0

main : IO ()
main = do
  args <- getArgs

  let nlines = fromMaybe 140 $ head' (drop 1 args) >>= (parsePositive {a = Nat})

  inputLines <- getNLines nlines

  printLn $ solve $ toList inputLines
