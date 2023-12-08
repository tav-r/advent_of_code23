import Data.Char (isDigit)
import Debug.Trace (traceShowId)

parseLine :: String -> Int
parseLine = read . filter isDigit

getDistance :: Int -> Int -> Int
getDistance timeTot timePush  = timePush * (timeTot - timePush)

solvePart2 :: Int -> Int -> Int
solvePart2 time record = (time + 1) - 2 * length (takeWhile ((<= record) .getDistance time) [0..time])

main :: IO ()
main = do
    time <- parseLine <$> getLine
    record <- parseLine <$> getLine

    print $ solvePart2 time record