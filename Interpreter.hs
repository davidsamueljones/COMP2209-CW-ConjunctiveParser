module Interpreter where
import Grammar

data Table = Table {titles :: Vars,
                    columns :: [[String]]}
                    deriving Show

-- TODO: Just ya know, write this thing?
--runInterpreter :: Show a => Prog -> [a]
--runInterpreter ast = []

interpretConjunction :: Table -> Table -> Table
interpretConjunction (Table titlesA columnsA) (Table titlesB columnsB)
--    | elementsOverlap titlesA titlesB   = (Table (titlesA ++ titlesB) (transpose ([rowA ++ [c] ++ rowB |     
    | elementsOverlap titlesA titlesB   = overlappedConj (Table titlesA columnsA) (Table titlesB columnsB)
    | otherwise                         = (Table (titlesA ++ titlesB) (transpose ([rowA ++ rowB | rowA <- transpose columnsA, rowB <- transpose columnsB]))) --TODO: Tidy up :)
    
overlappedConj :: Table -> Table -> Table
overlappedConj (Table titlesA columnsA) (Table titlesB columnsB) = (Table (otherATitles ++ keyTitles ++ otherBTitles) columns)
                             where aRows = transpose columnsA
                                   keyTitles = getOverlapping titlesA titlesB
                                   otherATitles = notOverlapping titlesA titlesB
                                   otherBTitles = notOverlapping titlesA titlesB
                                   columns = transpose ([a ++ b |a <- aRows, let aKey = removeOthersFromRow keyTitles titlesA a, let aOther = removeFromRow keyTitles titlesA a, b <- getMatchingRows aKey keyTitles (Table titlesB columnsB) ]) --TODO: Im sorry david ill shorten this
    
 
    
-- Our Rules:
-- * A variable that appears under the scope of an existential quantifier is 
--   said to be bound, otherwise it is free
-- * LHS of turnstile must have all free variables 
-- * Free variables must all be in the scope of at least one relation

-- Musings:
-- * Probably want either CEK or CESK interpreter, probably don't need store 
--   in CESK but who knows...
-- * Looks like breaking down into normal form a good idea?
-- * Use HashMap package for the environment?

-- Helper functions
removeFromRow :: [String] -> [String] -> [String] -> [String]
removeFromRow vars titles row = [v | n <- [0..(length titles -1)],let v = row !! n, let a = titles !! n, notElem a vars]

removeOthersFromRow :: [String] -> [String] -> [String] -> [String]
removeOthersFromRow vars titles row = [v | n <- [0..(length titles -1)],let v = row !! n, let a = titles !! n,elem a vars]

elementsOverlap :: Eq a => [a] -> [a] -> Bool
elementsOverlap [] ys = False
elementsOverlap (x:xs) ys = elem x ys || elementsOverlap xs ys

getOverlapping :: Eq a => [a] -> [a] -> [a]
getOverlapping [] _ = []
getOverlapping (x:xs) ys
    | elem x ys = [x] ++ getOverlapping xs ys
    | otherwise = getOverlapping xs ys
   
notOverlapping :: Eq a => [a] -> [a] -> [a]
notOverlapping [] _ = []
notOverlapping (x:xs) ys
    | notElem x ys = [x] ++ notOverlapping xs ys
    | otherwise = getOverlapping xs ys

getColumns :: Table -> [[String]]
getColumns (Table _ cs) = cs

getTitles :: Table -> [String]
getTitles (Table ts _) = ts

containsColumn :: Var -> Table -> Bool
containsColumn name (Table ts _) = elem name ts

findColumn :: Var -> Table -> [String]
findColumn var (Table [] []) = []
findColumn var (Table (t:ts) (c:cs))
    | (var == t) && (notElem var ts)    = c
    | (var == t)                        = [] -- TODO: decide how to handle this case of repeated element names within table e.g. T(x1,x2,x2)   
    | (notElem var ts)                  = []
    | otherwise                         = findColumn var (Table ts cs)
    
findColumns :: [Var] -> Table -> [[String]]
findColumns vars table = [ k | a <- vars, let k = findColumn a table]
    
findNotColumn :: Var -> Table -> Table
findNotColumn var (Table ts cs) = (Table [a | a <- ts, a /= var] (findNotColumn' var (Table ts cs)))

findNotColumn' :: Var -> Table -> [[String]]
findNotColumn' _ (Table [] []) = []
findNotColumn' var (Table (t:ts) (c:cs)) 
    | (var == t)    = findNotColumn' var (Table ts cs)
    | otherwise     = [c] ++ findNotColumn' var (Table ts cs)
    
findNotColumns :: [Var] -> Table -> Table
findNotColumns vars table = foldr findNotColumn table vars
    
splitOnColumn :: Var -> Table -> (Table, Table)
splitOnColumn var (Table ts cs) = ((Table leftTitles leftColumns),(Table rightTitles rightColumns))
    where splitIndex = getTitleIndex var (Table ts cs)
          splitTitles = splitAt splitIndex ts
          splitColumns = splitAt splitIndex cs
          leftTitles = fst splitTitles
          rightTitles = tail (snd splitTitles)
          leftColumns = fst splitColumns
          rightColumns = tail (snd splitColumns)
          
getTitleIndex :: Var -> Table -> Int
getTitleIndex _ (Table [] []) = 0 -- TODO: Handle properly?
getTitleIndex var (Table (t:ts) (c:cs))
    | var == t      = 0
    | otherwise     = 1 + getTitleIndex var (Table ts cs)
    
getMatchingRows :: [Var] -> [Var] -> Table -> [[String]]
getMatchingRows keyVars keyTitles table = getMatchingRows' keyVars keyRows otherRows
                where keyColumns = findColumns keyTitles table
                      otherColumns = getColumns(findNotColumns keyTitles table )
                      keyRows = transpose keyColumns
                      otherRows = transpose otherColumns
    
getMatchingRows' :: [Var] -> [[String]] -> [[String]] -> [[String]]
getMatchingRows' ks (kv:kvs) (r:rs)
    | ks == kv   = [kv ++ r] ++ getMatchingRows' ks kvs rs
    | otherwise = getMatchingRows' ks kvs rs
    
--getMatchingRows :: Var -> Var -> Table -> [[String]]
--getMatchingRows varName titleName (Table titles columns)
--    | notElem titleName titles = (Table titles [])
--    | otherwise                = (getMatchingRows' varName varColumn (transpose leftColumns) (transpose rightColumns))
--    where split = splitOnColumn varName (Table titles columns)
--          leftColumns = getColumns(fst split)
--          rightColumns = getColumns(snd split)
--          varColumn = findColumn varName (Table titles columns)         
          
--getMatchingRows' :: Var -> [String] -> [[String]] -> [[String]] -> [[String]]
--getMatchingRows' _  [] [] [] = []
--getMatchingRows' var (c:cs) (l:ls) (r:rs) --Left and right of column to match
--    | var == c      = [l ++ [c] ++ r] ++ getMatchingRows' var cs ls rs
--    | otherwise     = getMatchingRows' var cs ls rs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xs = [(map head xs)] ++ transpose (map tail xs)

-- http://matt.might.net/articles/cek-machines/
-- http://matt.might.net/articles/cesk-machines/
-- https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf
