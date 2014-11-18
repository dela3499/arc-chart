import String
import Dict

string = "ababab"
--main = asText (collectRepeatedSubstrings string)

-- Get the value associated with a key
get: comparable -> Dict.Dict comparable v -> v
get key dict = Dict.getOrFail key dict

-- Insert key-value pair into dictionary. Conses value when there is a collision
dictPush: (comparable,v) -> Dict.Dict comparable [v] -> Dict.Dict comparable [v]
dictPush kvPair dict = 
  let key = (fst kvPair)
      value = (snd kvPair)
  in case (Dict.member key dict) of
    False -> Dict.insert key [value] dict
    True -> Dict.insert key (value::(get key dict)) dict 

-- Insert key-value pairs into dictionary. Conses value when there is a collision
dictFromList: [(comparable,v)] -> Dict.Dict comparable [v]
dictFromList kvPairs = 
  let f kvPair dict = dictPush kvPair dict
  in foldl f Dict.empty kvPairs

getSubstrings: String -> [String]
getSubstrings s =
  let f (start, end) = String.slice start end s
  in map f (getSubstringIndices (String.length s))

getSubstringIndices: Int -> [(Int, Int)]
getSubstringIndices n = 
  let f start = map (\end -> (start, end)) [(start + 1)..n]
  in map f [0..(n - 1)] |> concat -- concat to flatten list of lists  

collectRepeatedSubstrings: String -> Dict.Dict String [(Int,Int)]
collectRepeatedSubstrings s = 
  let f indices = (String.slice (fst indices) (snd indices) s, indices)
  in dictFromList (map f (getSubstringIndices (String.length s)))

-- Return True if tuple ranges overlap
isOverlapping: (Int, Int) -> (Int, Int) -> Bool
isOverlapping x y = 
  let isBetween x low high = if ((x > low) && (x < high)) then True else False
      x1 = (fst x)
      x2 = (snd x)
      y1 = (fst y)
      y2 = (snd y)
  in if ((isBetween y1 x1 x2) || (isBetween y2 x1 x2)) then True else False

-- Return non-overlapping lists of tuples
removeOverlaps: [(Int, Int)] -> [(Int, Int)]
removeOverlaps list = 
  let sortedList = sortBy fst list
      f range newList = 
        let shouldAdd = ((length newList) == 0) || not (isOverlapping (last newList) range)
        in if shouldAdd then newList ++ [range] else newList
  in foldl f [] sortedList