
import Test.QuickCheck ()
import Data.List (elemIndices)
import Data.Maybe (isJust)
import System.Random 


-- Representation of 2048 puzzles 
type Tile = Maybe Int 
type Row  = [Tile]    

newtype Board = Board [Row]
 deriving ( Show, Eq )

type Pos = (Int,Int)

rows :: Board -> [Row]
rows (Board ms) = ms


setupNewBoard :: StdGen -> Board
setupNewBoard g = placeTile firtPlacement pos2 tile2
    where firtPlacement  = placeTile emptyBoard pos1 tile1 
          emptyBoard     = Board (replicate 4 (replicate 4 Nothing))
          ((tile1, g'), (tile2, f)) = (getRandomTile g, getRandomTile g') 
          ((pos1, f'), (pos2, _))  = (getRandomFromList (blanks emptyBoard) f,
                                   getRandomFromList (blanks firtPlacement) f')
          
          
getRandomTile :: StdGen -> (Tile, StdGen)
getRandomTile = getRandomFromList [Just 2, Just 2, Just 2, Just 4, Just 4] 

getRandomFromList :: [a] -> StdGen -> (a, StdGen)
getRandomFromList list g = (list !! i, g')
    where (i, g') = randomR (0, length list - 1) g
        
placeTile :: Board -> Pos -> Tile -> Board
placeTile (Board rows) (y,x) tile = Board (rows !!= (y, newRow))
   where newRow = (rows !! y) !!= (x, tile)

-- Replaces the element at given index with given value
(!!=) :: [a] -> (Int, a) -> [a]
xs !!= (i,y) = replaceNth (i,y) xs
  where replaceNth  _  [] = []
        replaceNth (i, y) (x:xs)
          | i == 0    = y:xs
          | otherwise = x:replaceNth (i-1, y) xs

printBoard :: Board -> IO ()
printBoard (Board [])   = putStrLn ""
printBoard (Board rows) = putStrLn (buildRows rows)

-- Convert each row into a string
buildRows :: [Row] -> String
buildRows []         = ""
buildRows (row:rows) = rowString ++ "\n" ++ buildRows rows
     where rowString = unwords [tileToString tiles | tiles <- row]

-- Converts the tile to a String representing that value
tileToString :: Tile -> String
tileToString Nothing   = "."
tileToString (Just a)  = show a

-- Get all the positions of blanks in a board
blanks :: Board -> [Pos]
blanks (Board rows) = getPositions (elemIndices Nothing (concat rows))
  where getPositions = map getElement
        getElement i =  allPos !! i
        allPos = [(x,y) | x <- [0..3], y <-[0..3]]


-- Functions for moving the board
{-
moveRight :: Board -> Board
moveRight (Board rows) = Board (map (temp (0,0)) rows)

moveLeft :: Board -> Board
moveLeft (Board rows) = Board (map (temp (0,0)) rows)

moveUp = undefined;

moveDown = undefined;


temp :: Row -> Row
temp tiles | i >= length tiles = tiles
           | otherwise = (filter (isJust) tiles) ++ (replicate (length tiles - (length $ filter (isJust) tiles)) Nothing)
-}




{-

                    case (tiles !! i) of
                    Nothing            -> temp (i+1, c+1) tiles
                    Just n | c > 0     -> temp (i+1, c) (addMergedTiles ((tiles !!= ((i-c), Just n)) !!= (i, Nothing)) (i,c))                                      
                           |otherwise  -> temp (i+1, 0) (addMergedTiles tiles (i,c))          
-}


-- TODO: Debugg, not working properly
addMergedTiles :: Int -> Row -> Row
addMergedTiles i row | i >= (length row - 1) = row
                     | otherwise         = addMergedTiles (i+1) (row !!= (i, nt1)) !!= ((i+1), nt2)
          where (nt1, nt2) = addTogheter ((row !! i), (row !! (i+1)))
  
  
  ---[ addTogheter (x,x+1) | x <- [0..(length row - 2)]]  

{-
 | (i-c-1) < 0 = row
                         | otherwise   =  (row !!= ((i-c-1), nt1)) !!= ((i-c), nt2)
      where (nt1, nt2) = addTogheter (row !! (i-c-1), row !! (i-c))  -- two tiles merged 
-} 
-- Functions for adding tiles togheter
addTogheter :: (Tile,Tile) -> (Tile,Tile) 
addTogheter tiles@(Just n, Just m) | n == m    = (Just (n+m), Nothing)
                                   | otherwise = tiles
addTogheter (Nothing, Just n)      = (Just n, Nothing)
addTogheter rest                   = rest

