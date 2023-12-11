import Data.List
import Data.Bifunctor (Bifunctor(bimap))

getRank :: Char -> Maybe Int
getRank = flip elemIndex "23456789TJQKA"

handSignature :: [Int] -> [Int]
handSignature h = reverse $ map withLength [1..5]
    where
        groupedHand :: [[Int]]
        groupedHand = (group . sort) h

        withLength :: Int -> Int
        withLength i = length $ filter ((==i).length) groupedHand

compareHand :: [Int] -> [Int] -> Ordering
compareHand h1 h2
    | signatureCompare == EQ = compare h1 h2
    | otherwise = signatureCompare
    where
        signatureCompare :: Ordering
        signatureCompare = compare (handSignature h1) (handSignature h2)

solvePart1 :: ([Int], [[Int]]) -> Int
solvePart1 = sum . zipWith (*) [1..] . map fst . sortBy (\a b -> compareHand (snd a) (snd b)) . uncurry zip

main :: IO ()
main = do
    input <- lines <$> getContents
    let (maybeHands, bids) = unzip $ bimap (mapM getRank) (read . drop 1) . span (/=' ') <$> input

    let l = curry solvePart1 bids <$> sequence maybeHands

    {- TODO: part2! -}

    print l