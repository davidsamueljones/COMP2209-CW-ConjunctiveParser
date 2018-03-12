module HarrysFuncts where 
import Grammar
import Data.List
import Data.Maybe

type ColumnTable = [Column] 
type RowTable = [Row]
type ColumnData = [String]
type RowData = [String]
type ColumnID = Var

data Column = Column
            {
               columnID :: ColumnID,
               columnData :: ColumnData
            }
            
data Row = Row
            {
               columnIDs :: [ColumnID],
               rowData :: RowData
            }

conjunction :: [Column] -> [Column] -> [Column]
conjunction c1 c2 = removeDupCols $ transposeRow combined
                  where r1 = transposeCol c1
                        r2 = transposeCol c2
                        ids = (columnIDs $ head r1) ++ (columnIDs $ head r2)
                        vars = getDupCols ids
                        combined = [(Row ids (rowData a ++ rowData b))| a <- r1, b <- r2, sameVars vars a b]
                       -- vars  = getDupCols $ columnIDs $ head combined
                      --  reversed = (Row (reverse $ columnIDs $ head combined) (reverse $ rowData combined))
                     --   filteredList = filter (\x -> sameVars vars x reversed) combined
                     
combine :: [Row] -> [Row] -> [Row]
combine r1 r2 = [(Row ids (a ++ b))| a <- map rowData r1, b <- map rowData r2]
              where ids = (columnIDs $ head r1) ++ (columnIDs $ head r2)
                        
sameVars :: [Var] -> Row -> Row -> Bool
sameVars vars row1 row2 = foldr (&&) True [a | b <- vars, let a = sameVar b row1 row2]
            
sameVar :: Var -> Row -> Row -> Bool
sameVar var row1 row2 = (getVar var row1) == (getVar var row2)

getVar :: Var -> Row -> Var
getVar var (Row columnIDs rowData)
  | elem var columnIDs = rowData !! fromJust (elemIndex var columnIDs)
  | otherwise          = error ("getVar called with no element")--TODO Exception
 
getDupCols :: [ColumnID] -> [Var]
getDupCols [] = []
getDupCols (x:xs)
    | elem x xs = [x] ++ getDupCols xs
    | otherwise = getDupCols xs

removeDupCols :: [Column] -> [Column]
removeDupCols [] = []
removeDupCols (x:xs)
  | elem id idList = removeDupCols xs
  | otherwise = [x] ++ removeDupCols xs
  where idList = map columnID xs
        id = columnID x

transposeCol :: [Column] -> [Row]
transposeCol t = transposeCol' (map columnID t) (map columnData t)
transposeCol' :: [ColumnID] -> [[String]] -> [Row]
transposeCol' _ [] = []
transposeCol' ids columns = [(Row ids row)] ++ transposeCol' ids tailColumns
                     where row = map head columns
                           tailColumns = map tail columns
  
transposeRow :: [Row] -> [Column]
transposeRow (t:ts) = transposeRow' (columnIDs t) (map rowData (t:ts))
transposeRow' :: [ColumnID] -> [[String]] -> [Column]
transposeRow' _ [] = []
transposeRow' (x:xs) rows = [(Column x column)] ++ transposeRow' xs tailRows
                        where column = map head rows
                              tailRows = map tail rows

colStringArr :: [Column] -> [[String]]
colStringArr t = map columnData t

rowStringArr :: [Row] -> [[String]]
rowStringArr t = map rowData t
