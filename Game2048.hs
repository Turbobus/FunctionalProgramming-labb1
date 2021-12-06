
import Test.QuickCheck ()
import Data.List 
import Data.Maybe (isJust, isNothing)
import System.Random


-- Representation of 2048 puzzles 
type Tile = Maybe Int
type Row  = [Tile]

-- Represents a Board
newtype Board = Board [Row]
 deriving ( Show, Eq )

-- A position inside the board
type Pos = (Int,Int)


-- Setups a new board when game is started
setupNewBoard :: StdGen -> Board
setupNewBoard g = placeTile firtPlacement pos2 tile2
    where firtPlacement  = placeTile emptyBoard pos1 tile1
          emptyBoard     = Board (replicate 4 (replicate 4 Nothing))
          ((tile1, g'), (tile2, f)) = (getRandomTile g, getRandomTile g')
          ((pos1, f'), (pos2, _))  = (getRandomFromList (blanks emptyBoard) f,
                                   getRandomFromList (blanks firtPlacement) f')


-- TODO: make a main function (WIN / LOSE also)

-- TODO: quickCheck


-- Get a random new tile
getRandomTile :: StdGen -> (Tile, StdGen)
getRandomTile = getRandomFromList [Just 2, Just 2, Just 2, Just 4, Just 4]

-- Get a random element from a given list
getRandomFromList :: [a] -> StdGen -> (a, StdGen)
getRandomFromList list g = (list !! i, g')
    where (i, g') = randomR (0, length list - 1) g

-- Place a given tile on the given position in the board
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

-- Prints out the board
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
        allPos = [(x,y) | x <- [0..3], y <- [0..3]]


-- Places a random new tile
placeNewTile :: Board -> StdGen -> Board
placeNewTile board g = placeTile board pos randTile
    where pos = fst (getRandomFromList (blanks board) g)
          randTile = fst (getRandomTile g)

          
-- Functions for moving the board, standard is moving to the left
move :: Char -> Board -> StdGen -> Board
move char board g | char == 'w' = placeNewTile (moveUp board) g
                  | char == 'a' = placeNewTile (moveLeft board) g
                  | char == 'd' = placeNewTile (moveRight board) g
                  | char == 's' = placeNewTile (moveDown board) g


-- Moves tiles right
moveRight :: Board -> Board
moveRight (Board rows) = Board (map shiftRightAndMerge rows)

-- Moves the tiles to the left
moveLeft :: Board -> Board
moveLeft (Board rows) = Board (map shiftLeftAndMerge rows)

-- Moves the tiles up
moveUp :: Board -> Board
moveUp (Board rows) = Board (transpose (map shiftLeftAndMerge transposed)) 
       where transposed = transpose rows

-- Moves the tiles down
moveDown :: Board -> Board
moveDown (Board rows) = Board (transpose (map shiftRightAndMerge transposed)) 
       where transposed = transpose rows

-- Moves all the tiles to the right and merge the ones that can be merged
shiftRightAndMerge :: Row -> Row
shiftRightAndMerge = reverse . shiftLeftAndMerge . reverse

-- Moves all the tiles to the left and merge the ones that can be merged
shiftLeftAndMerge :: Row -> Row
shiftLeftAndMerge tiles = addMergedTiles (listOfJust ++ (replicate numOfNothings Nothing))
        where numOfNothings = length tiles - length listOfJust  
              listOfJust    = filter isJust tiles

-- Merge tiles, helper function for shiftLeftAndMerge
addMergedTiles :: Row -> Row
addMergedTiles (row:row2:rowRest) | isNothing nt2 = nt1 : addMergedTiles (rowRest ++ [nt2])
                                  | otherwise     = nt1 : addMergedTiles (nt2:rowRest)
        where (nt1, nt2) = addTogheter (row, row2)
addMergedTiles row = row
                  
                  
-- Function for adding tiles togheter
addTogheter :: (Tile,Tile) -> (Tile,Tile)
addTogheter tiles@(Just n, Just m) | n == m    = (Just (n+m), Nothing)
                                   | otherwise = tiles
addTogheter (Nothing, Just n)      = (Just n, Nothing) 
addTogheter rest                   = rest

