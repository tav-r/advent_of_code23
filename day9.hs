import Data.Char (isDigit)
import Debug.Trace

parseLine :: String -> [Int]
parseLine [] = []
parseLine (c:cs)
    | isDigit c || c == '-' = read (c:takeWhile isDigit cs) : parseLine (dropWhile isDigit cs)
    | otherwise = parseLine cs

predictNextSummand:: [Int] -> Int
predictNextSummand is
    | all (0==) is = 0
    | otherwise = last is + predictNextSummand summands
        where
            summands :: [Int]
            summands = uncurry (-) <$> zip (drop 1 is) is

solvePart1 :: [String] -> Int
solvePart1 = sum . map (predictNextSummand . parseLine)

solvePart2 :: [String] -> Int
solvePart2 = sum . map (predictNextSummand . reverse . parseLine)

main :: IO ()
main = do
    lines <- lines <$> getContents
    print $ solvePart1 lines 

    print $ solvePart2 lines