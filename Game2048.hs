
import Test.QuickCheck
import Data.List
import Data.Maybe (isJust, isNothing)
import Data.Char
import System.Random


-- Representation of 2048 puzzles 
type Tile = Maybe Int
type Row  = [Tile]


-- Represents a Board
newtype Board = Board [Row]
 deriving ( Show, Eq )


-- A position inside the board
type Pos = (Int,Int)




-- TODO
-- Make it so you can play after 2048
-- Ask for help with property
-- GUI?
-- Ask in main for board size, fix crash cases
-- Look over functions 
-- hlint



exampleWon :: Board
exampleWon =
    Board
      [ [j 3,j 6,n  ,n  ]
      , [n  ,j 5,n  ,n  ]
      , [n  ,n  ,j 9,j 2]
      , [n  ,n  ,j 1024  ,j 1024]
      ]
  where
    n = Nothing
    j = Just

exampleBlanks :: Board
exampleBlanks =
    Board
      [ [n,n,n,n  ]
      , [n ,n,n  ,n  ]
      , [n  ,n  ,n,n]
      , [n  ,n  ,n  ,n]
      ]
  where
    n = Nothing
    j = Just    

exampleLost :: Board
exampleLost =
    Board
      [ [j 5,j 2,j 3 ,j 4 ]
      , [j 4 ,j 6,j 7 ,j 8 ]
      , [j 9 ,j 10 ,n ,j 12]
      , [j 13 ,j 14 ,j 15,j 16]
      ]
  where
    n = Nothing
    j = Just


-- Setups a new board when game is started
setupNewBoard :: Int -> StdGen -> Board
setupNewBoard n g = placeNewTile firstPlacement f
    where firstPlacement = placeTile emptyBoard pos1 tile1
          emptyBoard     = Board (replicate n (replicate n Nothing))
          (tile1, g')    = getRandomTile g
          (pos1, f)      = getRandomFromList (blanks emptyBoard) g'


main :: IO()
main = do
    g <- randomIO :: IO Int
    putStr "What board size do you want?(nxn) n= "
    x <- getChar
    let n = digitToInt x 
    let board = setupNewBoard n (mkStdGen g)
    play board


play :: Board -> IO()
play board = case (haveWonOrLost board) of
    Just True  -> printHelper board "\nYou have won!\n"
    Just False -> printHelper board "\nYou lost!! :(\n"
    Nothing    -> do
      printHelper board "Make a move: "
      input <- getChar
      g <- randomIO :: IO Int
      let newBoard = move input board
      if board == newBoard
        then play newBoard
      else play (placeNewTile newBoard (mkStdGen g))

printHelper :: Board -> String -> IO()
printHelper b s = do
    putStr "\n"
    printBoard b
    putStr s

won :: Board -> Bool
won (Board rows) | maximum (concat rows) >= Just 2048 = True
                 | otherwise                          = False

lost :: Board -> Bool
lost (Board rows) | Nothing `elem` (concat rows) = False
                  | otherwise                    = moveAvailable
      where moveAvailable = available (transpose rows) && available rows
            available r   = True `notElem` (map canMakeMove r)

canMakeMove :: Row -> Bool
canMakeMove row | maxLength > 1 = True
                | otherwise     = False
      where maxLength = maximum (map length (group row))

haveWonOrLost :: Board -> Maybe Bool
haveWonOrLost board | won board  = Just True
                    | lost board = Just False
                    | otherwise  = Nothing


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
        allPos = [(x,y) | x <- [0..length rows-1], y <- [0..length rows-1]]


-- Places a random new tile
placeNewTile :: Board -> StdGen -> Board
placeNewTile board g = placeTile board pos randTile
    where pos = fst (getRandomFromList (blanks board) g)
          randTile = fst (getRandomTile g)


-- Functions for moving the board, standard is moving to the left
move :: Char -> Board -> Board
move char board | char == 'w' = moveUp board
                | char == 'a' = moveLeft board
                | char == 'd' = moveRight board
                | char == 's' = moveDown board
                | otherwise   = board

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


-- TODO: quickCheck
-- Check canMakeMove


-- Generator for non empty boards
instance Arbitrary Board where
  arbitrary = do
    rows <- vectorOf 4 (vectorOf 4 tile)
    i <- arbitrary
    if (prop_2048 (Board rows))
      then return (Board rows)
    else  
      return (setupNewBoard 4 (mkStdGen i))

-- Generate a tile
tile :: Gen Tile
tile = do
    num <- choose (1,11) :: Gen Int
    frequency [(5, return $ Just (2^num)), (5, return Nothing)]

-- Property for a 2048 board, it can not be empty
prop_2048 :: Board -> Bool
prop_2048 (Board rows) = length (filter isNothing (concat rows)) /= 0

-- Property for testing if a new tile is placed
prop_placeNewTile :: Board -> Int -> Property
prop_placeNewTile (Board rows) i = Nothing `elem` (concat rows) ==> (placeNewTile (Board rows) (mkStdGen i)) /= (Board rows)


-- Property for testing if board is a winning board
prop_won :: Board -> Bool
prop_won (Board rows) | won (Board rows) = Just 2048 `elem` (concat rows)
                      | otherwise        = Just 2048 `notElem` (concat rows)

-- Property for testing if board is a losing board, TODO: something more?
prop_lost :: Board -> Bool
prop_lost (Board rows) | lost (Board rows) = Nothing `notElem` (concat rows)
                       | otherwise         = Nothing `elem` (concat rows) || any canMakeMove rows || any canMakeMove (transpose rows)


-- Property for testig if we made a move
prop_move :: Board -> Int -> Bool
prop_move board i  | c == 'w' = False `notElem` (map testRow (map reverse (transpose rows)))
                   | c == 's' = False `notElem` (map testRow (transpose rows)) 
                   | c == 'a' = False `notElem` (map testRow (map reverse rows))
                   | c == 'd' = False `notElem` (map testRow rows) 
      where (Board rows) = move c board
            c = fst (getRandomFromList ['w','a','s','d'] (mkStdGen i))
            testRow row = length (takeWhile (isNothing) row) == length (filter isNothing row)


prop_canMakeMove :: Board -> Bool
prop_canMakeMove board = undefined
