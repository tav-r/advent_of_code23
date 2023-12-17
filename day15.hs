import Data.Char (ord, isAlpha)
import Control.Monad
import Debug.Trace
import Data.Map (Map, insert, update, delete, empty, alter, member, (!))

data Instruction = Insert Int | Remove
    deriving (Show)

hash :: String -> Int
hash = foldl (\i -> (`mod` 256) . (*17) . (+i) . ord) 0

split :: String -> Char -> [String]
split [] _ = []
split xs c = takeWhile (/=c) xs : split (drop 1 $ dropWhile (/=c) xs) c

solve1 :: [String] -> Int
solve1 = sum . map hash

modifyBoxes :: Map Int [(String, Int)] -> (String, Instruction) -> Map Int [(String, Int)]
modifyBoxes boxes instr = case instr of
    (label, Insert i) -> alter (Just . addOrInsert label i) (hash label) boxes
    (label, Remove) -> update (Just . delete label) (hash label) boxes
    where
        delete :: String -> [(String, Int)] -> [(String, Int)]
        delete l ls = takeWhile ((/=l) . fst) ls ++ drop 1 (dropWhile ((/=l) . fst) ls)

        addOrInsert :: String -> Int -> Maybe [(String, Int)] -> [(String, Int)]
        addOrInsert s i (Just l)
            | s `elem` (fst <$> l) = map (\(t, j) -> if s == t then (s, i) else (t, j)) l
            | otherwise = l ++ [(s, i)]
        addOrInsert s i Nothing = [(s, i)]

parseInstruction :: String -> (String, Instruction)
parseInstruction s = (takeWhile isAlpha s, instr)
    where
        instrStr = dropWhile isAlpha s
        instr
            | instrStr == "-" = Remove
            | otherwise = Insert $ read $ drop 1 instrStr

calcBoxN :: Map Int [(String, Int)] -> Int -> Int
calcBoxN boxes i = sum $ (\(j, (_, k)) -> (i + 1) * j * k) <$> boxIndexed
    where
        boxIndexed = zip [1..length box] box
        box = boxes ! i

solve2 :: [String] -> Int
solve2 ss = sum $ calcBoxN boxes <$> filter (`member` boxes) [0..255]
    where
        boxes = (foldl modifyBoxes empty . map parseInstruction) ss

main :: IO ()
main = do
    input <- flip split ',' . join . lines <$> getContents

    print . solve1 $ input
    print . solve2 $ input