import String
import Dict

x = 300
s = "10101"

arcs: [Arc]
arcs = getArcs s

config: Config
config = {w=x,h=x}

--main: Element
--main = display config (scaleArcs arcs (toFloat config.w))

main = testRepetitionRegions

testRepetitionRegions = 
  let nonOverlappingSubstrings = 
        s |> collectRepeatedSubstrings |> removeOverlappingSubstrings  
      repetitionRegions = getRepetitionRegions nonOverlappingSubstrings
  in asText repetitionRegions

getArcs s = 
  let nonOverlappingSubstrings = 
        s |> collectRepeatedSubstrings |> removeOverlappingSubstrings  
      maximalPairs = 
        nonOverlappingSubstrings |> collectConsecutivePairs |> 
        collectMaximalPairs |> takePairs 
      repetitionRegions = getRepetitionRegions nonOverlappingSubstrings
      essentialPairs = maximalPairs
  in map pairToArc essentialPairs

type Pair = ((Int,Int),(Int,Int))

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

collectSubstrings: String -> Dict.Dict String [(Int,Int)]
collectSubstrings s = 
  let f indices = (String.slice (fst indices) (snd indices) s, indices)
  in dictFromList (map f (getSubstringIndices (String.length s)))

collectRepeatedSubstrings: String -> Dict.Dict String [(Int,Int)]
collectRepeatedSubstrings s = Dict.filter (\key value -> (length value) > 1) (collectSubstrings s)

-- Return True if tuple ranges overlap
isOverlapping: (Int, Int) -> (Int, Int) -> Bool
isOverlapping x y = 
  let isBetween x low high = ((x > low) && (x < high))
      x1 = (fst x)
      x2 = (snd x)
      y1 = (fst y)
      y2 = (snd y)
  in ((isBetween y1 x1 x2) || (isBetween y2 x1 x2))

-- Return non-overlapping lists of tuples
removeOverlaps: [(Int, Int)] -> [(Int, Int)]
removeOverlaps list = 
  let sortedList = sortBy fst list
      f range newList = 
        let shouldAdd = ((length newList) == 0) || not (isOverlapping (last newList) range)
        in if shouldAdd then newList ++ [range] else newList
  in foldl f [] sortedList

removeOverlappingSubstrings: Dict.Dict String [(Int,Int)] -> Dict.Dict String [(Int,Int)]
removeOverlappingSubstrings x = Dict.map removeOverlaps x

-- Append a value to the last element in a list of lists
appendToLast: a -> [[a]] -> [[a]]
appendToLast x list = (take ((length list) - 1) list) ++ [(last list) ++ [x]]

-- Append a value to a list
append: a -> [a] -> [a]
append x list = list ++ [x]

-- Return true if substrings are adjacent
substringsAdjacent: (Int,Int) -> (Int,Int) -> Bool
substringsAdjacent a b = ((snd a) == (fst b)) || ((fst a) == (snd b))

groupAdjacentSubstrings: [(Int,Int)] -> [[(Int,Int)]]
groupAdjacentSubstrings x = 
  let f x list = 
    let newGroup = (list == []) || not (substringsAdjacent x (last (last list)))
    in if newGroup then append [x] list else appendToLast x list
  in foldl f [] x

-- Given a group of ordered, adjacent substrings, return the lowest and highest indices as a tuple
findGroupBounds: [[(Int,Int)]] -> [(Int,Int)]
findGroupBounds x = 
  let f group = (fst (head group), snd (last group))
  in map f x

findRepetitionRegions: [(Int,Int)] -> [(Int,Int)]
findRepetitionRegions x = 
  let regionGroups = groupAdjacentSubstrings x |> filter (\a -> (length a) > 1)
  in findGroupBounds regionGroups

-- Take a list of non-overlapping repeated substrings, and find the repetition regions in it (adjacent repeated substrings)
getRepetitionRegions: Dict.Dict String [(Int,Int)] -> [(String,(Int,Int))]
getRepetitionRegions x = 
  let regionsDictList = Dict.map findRepetitionRegions x |> Dict.toList
      f kvPair = 
        let key = fst kvPair
            vals = snd kvPair
        in map (\val -> (key,val)) vals
  in concatMap f regionsDictList

-- Pair up consecutive elements in a list
pair: [a] -> [(a,a)]
pair list = 
  let list1 = take ((length list) - 1) list
      list2 = tail list
  in zip list1 list2

collectConsecutivePairs: Dict.Dict String [(Int,Int)] -> [(String,Pair)]
collectConsecutivePairs x = 
  let f key indexList list = 
    let substrings = map (\p -> (key,p)) (pair indexList)
    in substrings ++ list
  in Dict.foldl f [] x

-- Returns true if first substring contains, or is identical to, second
substringAContainsB: (Int,Int) -> (Int,Int) -> Bool
substringAContainsB a b =
  let isBetween x low high = ((x >= low) && (x <= high))
      a1 = fst a
      a2 = snd a
      b1 = fst b
      b2 = snd b
  in (isBetween b1 a1 a2) && (isBetween b2 a1 a2)

-- Returns true if first pair contains, or is identical to, second
pairAContainsB: Pair -> Pair -> Bool
pairAContainsB a b = 
  let x1 = fst a
      x2 = snd a
      y1 = fst b
      y2 = snd b
  in (substringAContainsB x1 y1) && (substringAContainsB x2 y2)

-- Returns true if pair is not contained in any other pair in list
pairIsMaximal: (String,Pair) -> [(String,Pair)] -> Bool
pairIsMaximal x xs = 
  let pair = snd x
      f xi answer = answer && (not (pairAContainsB (snd xi) pair))
  in foldl f True xs

-- Apply f to each element xi of a list xs, but with an additional argument: xs without xi
pieceMap: (a -> [a] -> b) -> [a] -> [b]
pieceMap f list = 
  let g i xi = 
    let listWithoutXi = (take i list) ++ (drop (i + 1) list)
    in f xi listWithoutXi
  in indexedMap g list

-- Apply f to each element xi of a list xs, but with an additional argument: xs without xi
-- then filter out elements where f returns false
pieceFilter: (a -> [a] -> Bool) -> [a] -> [a]
pieceFilter f list = 
  let indexedList = zip [0..(length list) - 1] list
      g x filteredList = 
        let i = fst x
            xi = snd x
            listWithoutXi = (take i list) ++ (drop (i + 1) list)
        in if (f xi listWithoutXi) then filteredList ++ [xi] else filteredList
  in foldl g [] indexedList

collectMaximalPairs: [(String,Pair)] -> [(String,Pair)]
collectMaximalPairs x = pieceFilter pairIsMaximal x

-- Remove the string component of the tuples
takePairs: [(String,Pair)] -> [Pair]
takePairs x = map snd x

pairToArc: Pair -> Arc
pairToArc pair = 
  let x1 = fst (fst pair) |> toFloat
      x2 = snd (fst pair) |> toFloat
      y1 = fst (snd pair) |> toFloat
      y2 = snd (snd pair) |> toFloat
  in {a = x1, b = y1, n = (x2 - x1)}

--

type Arc = {a: Float, b: Float, n: Float}
type Config = {w: Int, h: Int}

display: Config -> [Arc] -> Element
display c arcs = 
  let arcforms = map (arc2form c) arcs
  in collage c.w c.h arcforms
 
arc2form: Config -> Arc -> Form
arc2form q arc = 
  let line = {width=arc.n, color=(rgba 78 154 255 0.5), cap=Flat, join=Smooth, dashing=[], dashOffset=0}
      r = (arc.b - arc.a) / 2
      cx = arc.a + (arc.n/2) + r
      cy = 0
      offset = (-(toFloat q.w)/2, -(toFloat q.h)/2)
  in traced line (path (arcPoints (cx, cy) r)) |> move offset

arcPoints: (number,number) -> number -> [(Float, Float)]
arcPoints center r = 
  let theta = linspace pi 0 1000
      x t = (fst center) + r * cos t
      y t = (snd center) + r * sin t
      f t = (x t, y t)
  in map f theta
        
linspace: number -> number -> Int -> [number]
linspace start end n = 
  let dt = (end - start) / ((toFloat n) - 1)
      f i = start + (i * dt)
  in map f [0..(toFloat n)-1]
  
  
scaleArcs: [Arc] -> Float -> [Arc]
scaleArcs arcs width = 
  let s = width / (maximum (map (\a -> a.b + a.n) arcs)) -- scalefactor
  in map (\x -> {a=x.a*s, b=x.b*s, n=x.n*s}) arcs

