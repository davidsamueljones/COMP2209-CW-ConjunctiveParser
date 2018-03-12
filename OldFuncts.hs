--Temp keeping incase I need any - Harry

interpretConjunction :: Table -> Table -> Table
interpretConjunction (Table titlesA columnsA) (Table titlesB columnsB)
--    | elementsOverlap titlesA titlesB   = (Table (titlesA ++ titlesB) (transpose ([rowA ++ [c] ++ rowB |     
    | elementsOverlap titlesA titlesB   = overlappedConj (Table titlesA columnsA) (Table titlesB columnsB)
    | otherwise                         = (Table (titlesA ++ titlesB) (transpose ([rowA ++ rowB | rowA <- transpose columnsA, rowB <- transpose columnsB]))) --TODO: Tidy up :)
    
overlappedConj :: Table -> Table -> Table
overlappedConj (Table titlesA columnsA) (Table titlesB columnsB) = (Table (otherATitles ++ keyTitles ++ otherBTitles) columns)
                             where aRows = transpose columnsA
                                   keyTitles = getOverlapping titlesA titlesB
                                   otherATitles = notOverlapping titlesA keyTitles
                                   otherBTitles = notOverlapping titlesB keyTitles
                                   columns = transpose ([aOther ++ b |a <- aRows, let aKey = removeOthersFromRow keyTitles titlesA a, let aOther = removeFromRow keyTitles titlesA a, b <- getMatchingRows aKey keyTitles (Table titlesB columnsB) ]) --TODO: Im sorry david ill shorten this

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
getMatchingRows' _ [] [] = []
getMatchingRows' ks (kv:kvs) (r:rs)
    | ks == kv   = [kv ++ r] ++ getMatchingRows' ks kvs rs
    | otherwise = getMatchingRows' ks kvs rs
getMatchingRows' _ _ _ = [] --If all variables are in key variables
    
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
transpose xs 
    | length (head xs) >1 = [(map head xs)] ++ transpose (map tail xs)
    | otherwise = [(map head xs)]
