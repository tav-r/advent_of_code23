import Data.Map hiding (take, drop, map, filter, foldr)
import Debug.Trace

type Node = (String, (String, String))
type Network = Map String (String, String)
data Choice = LeftC | RightC

navigate :: Network -> String  -> Choice -> Maybe String
navigate ns s LeftC = fst <$> Data.Map.lookup s ns
navigate ns s RightC = snd <$> Data.Map.lookup s ns

parseMapping :: String -> Node
parseMapping cs = (take 3 cs, (take 3 $ drop 7 cs, take 3 $ drop 12 cs))

countPath :: (String -> Bool) -> String -> [Choice] -> Network -> Int -> Maybe Int
countPath f s (c:cs) ns i
    | f s = Just i
    | otherwise = do
        nextNode <- navigate ns s c
        countPath f nextNode cs ns (i + 1)

parseChoice :: String -> Maybe [Choice]
parseChoice [] = pure []
parseChoice ('L':s) = (LeftC :) <$> parseChoice s
parseChoice ('R':s) = (RightC :) <$> parseChoice s
parseChoice _ = Nothing

{-
it took too long for me to realize that we can just calculate the path length
for each start-string ending wtih 'A' to a string ending with 'Z' and then
calculate the LCM of all these values...
-}
countPaths :: [String] -> [Choice] -> Network -> Int -> Maybe Int
countPaths ss cs ns i = do
    allRes <- mapM (\s -> countPath ((=='Z').last) s cs ns 0) ss

    pure $ foldr lcm 1 allRes

main :: IO ()
main = do
    lines <- lines <$> getContents

    let firstLine = head lines
    let restLines = drop 2 lines

    let choices = parseChoice firstLine

    let network = fromList $ parseMapping <$> restLines

    case (\cs -> countPath (=="ZZZ") "AAA" (cycle cs) network 0) =<< choices of
        Nothing -> print "could not solve"
        Just n -> print n

    let ss = filter ((=='A').last) $ map (take 3) restLines

    case (\cs -> countPaths ss (cycle cs) network 0) =<< choices of
        Nothing -> print "could not solve"
        Just n -> print n