
import Test.QuickCheck
import Data.List
import Data.Maybe (isJust, isNothing)
import Data.Char
import System.Random
import Data.IORef

import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Events
import Graphics.UI.Threepenny (rows)
import Text.Read

-- Representation of 2048 boards 
type Tile = Maybe Int
type Row  = [Tile]


-- Represents a Board
newtype Board = Board [Row]
  deriving ( Show, Eq )


-- A position inside the board
type Pos = (Int,Int)


-- ExampleBoards
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


-----------------------------------------------------------------------------
-- UI Sections

-- Uses GUI
main :: IO()
main = do
    startGUI defaultConfig setup

-- Setup for GUI
setup :: Window -> UI ()
setup w = do
    return w # set title "2048"

    -- All different elements
    output <- UI.h2 #. "out"
        # set text "What board size do you want?(nxn) n ="
        # set style [("text-align", "center")]
    down <- UI.button # set text "<"
       # set style [("background-color", "#008CBA"),("color", "white"),("font-size","16px"),("border-radius","10px")]

    size <- UI.p #. "size"
        # set text "4"
        # set style [("font-size","16px")]

    up <- UI.button # set text ">"
        # set style [("background-color", "#008CBA"),("color", "white"),("font-size","16px"),("border-radius","10px")]
    
    confirm <- UI.button # set text "Confirm"
        # set style [("background-color", "#008CBA"),("color", "white"),("font-size","16px"),("border-radius","10px")]
    holder <- UI.div #. "holder"
        # set style [("display","flex"), ("justify-content","center"),("gap","10px"), ("padding-bottom","10px"),("height","40px"),("align-items","baseline")]
    element holder #+ [ element down,
                        element size,
                        element up,
                        element confirm]


    -- Create start board with an IORef
    g <- liftIO (randomIO :: IO Int)
    let board = setupNewBoard 4 (mkStdGen g)
    boardRef <- liftIO $ newIORef board

    -- IORef for size input
    sizeRef <- liftIO (newIORef 4 :: IO (IORef Int))

    -- Turn our start board into an UI grid
    startGrid <- UI.grid (toUIGrid board)
        # set style [("width","400px"),("height","400px"),("border","solid black 2px"),("margin","auto"), ("table-layout", "fixed"),("border-spacing","10px"),("background","#bbada0"),("border-radius","10px")]
        # set (attr "tabindex") "1" -- allow key pressed

    -- Add all our elements to the body so we see them on the webpage
    getBody w #+ [element output,
                  element holder,
                  element startGrid]
    body <- getBody w
       # set style [("background", "#faf8ef")]
   
    -- When changing board size down
    on UI.click down $ \event -> do
      curSize <- liftIO $ readIORef sizeRef
      if curSize > 2
        then liftIO $ writeIORef sizeRef (curSize - 1)
      else return ()
      newSize <- liftIO $ readIORef sizeRef
      element size # set text (show newSize)
    
    -- When changing board size up
    on UI.click up $ \event -> do
      curSize <- liftIO $ readIORef sizeRef
      if curSize < 9
        then liftIO $ writeIORef sizeRef (curSize + 1)
      else return ()
      newSize <- liftIO $ readIORef sizeRef
      element size # set text (show newSize)

    -- Confirming the new size
    on UI.click confirm $ \event -> do
            c <- liftIO $ readIORef sizeRef
            g <- liftIO (randomIO :: IO Int)
            let board = setupNewBoard c (mkStdGen g)
            ele <- getElementsByClassName w "table"
            UI.delete (head ele)
            newGrid <- UI.grid (toUIGrid board)
                # set style [("width","400px"),("height","400px"),("border","solid black 2px"),("margin","auto"), ("table-layout", "fixed"),("border-spacing","10px"),("background","#bbada0"),("border-radius","10px")]
                # set (attr "tabindex") "1" -- allow key pressed
          
            liftIO $ writeIORef boardRef board
            getBody w #+ [element newGrid]

    -- Handles a move when playing with a board
    on UI.keydown body $ \c -> do
        let char = keyCodeConverter c
        oldBoard <- liftIO $ readIORef boardRef
        newBoard <- liftIO $ checkBoards oldBoard (move char oldBoard)
        ele <- getElementsByClassName w "table"
        UI.delete (head ele)
        newGrid <- UI.grid (toUIGrid newBoard)
          # set style [("width","400px"),("height","400px"),("border","solid black 2px"),("margin","auto"), ("table-layout", "fixed"),("border-spacing","10px"),("background","#bbada0"),("border-radius","10px")]
          # set (attr "tabindex") "1" -- allow key pressed
       
        liftIO $ writeIORef boardRef newBoard
        getBody w #+ [element newGrid]

        case haveWonOrLost newBoard False of
          Just True  -> element output # set text "You have won!! keep going if you want"
          Just False -> element output # set text "You have lost!! :("
          Nothing    -> element output # set text "Make a move"

        return ()

-- Converts from KeyCode to an char used for move
-- Handles both w a s d and arrow keys
keyCodeConverter :: KeyCode -> Char
keyCodeConverter code | code == 87 || code == 38 = 'w'
                      | code == 65 || code == 37 = 'a'
                      | code == 83 || code == 40 = 's'
                      | code == 68 || code == 39 = 'd'
                      | otherwise = 'g'

-- Check if boards are equal, otherwise place a new tile
checkBoards :: Board -> Board -> IO Board
checkBoards old temp = do
                g <- randomIO :: IO Int
                if old == temp
                  then return temp
                else return (placeNewTile temp (mkStdGen g))

-- Convert a Board to a UI grid
toUIGrid :: Board -> [[UI Element]]
toUIGrid (Board rows) = (map . map) tileToUIElement rows

-- Converts a tile to a usable UI Element
tileToUIElement :: Tile -> UI Element
tileToUIElement tile = do
            let strTile = tileToString tile
            let value = readMaybe strTile :: Maybe Int
            let colour = valueToColour value
            out  <- UI.div # set text strTile
            UI.div #. "elem"
                # set style [("display", "flex"),("justify-content","center"),("align-items", "center"), ("width","100%"),("height","100%"),("border","solid black 1px"), ("background",colour),("border-radius","10px")]
                #+ [element out]

valueToColour :: Maybe Int -> String
valueToColour Nothing = "#cdc1b4"
valueToColour (Just n)  = colourList !! (index 0 `mod` length colourList)
    where colourList = ["#eee4da","#eee1c9","#f3b27a","#f69664","#f77c5f","#f75f3b","#edd073","#edcc62"]
          index c | 2^c == n  = c-1
                  | otherwise = index (c+1) 


-------------------------------------------------------------------------------------
-- Terminal Section

-- Uses terminal
main' :: IO()
main' = do
    g <- randomIO :: IO Int
    putStr "What board size do you want? n >= 2 (nxn) n= "
    x <- getChar
    let n = digitToInt x
    let board = setupNewBoard n (mkStdGen g)
    play board False

-- Loop function for the game
play :: Board -> Bool -> IO()
play board continue = case haveWonOrLost board continue of
    Just True  -> do
                    printHelper board "\nYou have won!\n\nDo you want to continue? y/n?"
                    input <- getChar
                    if input == 'y'
                      then play board True
                    else putStr "\nGoodbye!\n"
    Just False -> printHelper board "\nYou lost!! :(\n"
    Nothing    -> do
      printHelper board "Make a move: "
      input <- getChar
      g <- randomIO :: IO Int
      let newBoard = move input board
      if board == newBoard
        then play newBoard continue
      else play (placeNewTile newBoard (mkStdGen g)) continue

-- Helps print things to the terminal
printHelper :: Board -> String -> IO()
printHelper b s = do
    putStr "\n"
    printBoard b
    putStr s

-- Prints out the board
-- Earlier work taken from lab 3
printBoard :: Board -> IO ()
printBoard (Board [])   = putStrLn ""
printBoard (Board rows) = putStrLn (buildRows rows)

-- Convert each row into a string
-- Earlier work taken from lab 3
buildRows :: [Row] -> String
buildRows []         = ""
buildRows (row:rows) = rowString ++ "\n" ++ buildRows rows
     where rowString = unwords [tileToString tiles | tiles <- row]

-- Converts the tile to a String representing that value
-- Earlier work taken from lab 3
tileToString :: Tile -> String
tileToString Nothing   = "."
tileToString (Just a)  = show a


-----------------------------------------------------------------------------
-- Common functions for UI and terminal section

-- Setups a new board when game is started
setupNewBoard :: Int -> StdGen -> Board
setupNewBoard n g = placeNewTile firstPlacement f
    where firstPlacement = placeTile emptyBoard pos1 tile1
          emptyBoard     = Board (replicate n (replicate n Nothing))
          (tile1, g')    = getRandomTile g
          (pos1, f)      = getRandomFromList (blanks emptyBoard) g'


-- Check if player has won
won :: Board -> Bool
won (Board rows) | maximum (concat rows) == Just 2048 = True
                 | otherwise                          = False

-- Check if player has lost
lost :: Board -> Bool
lost (Board rows) | Nothing `elem` concat rows = False
                  | otherwise                    = moveAvailable
      where moveAvailable = available (transpose rows) && available rows
            available r   = True `notElem` map canMakeMove r

-- Check if a row has an available move
-- Helper function for lost
canMakeMove :: Row -> Bool
canMakeMove row = maxLength > 1
      where maxLength = maximum (map length (group row))

-- Checks if the game is won or lost 
haveWonOrLost :: Board -> Bool -> Maybe Bool
haveWonOrLost board con | won board && not con = Just True
                        | lost board           = Just False
                        | otherwise            = Nothing


-- Get a random new tile
getRandomTile :: StdGen -> (Tile, StdGen)
getRandomTile = getRandomFromList [Just 2, Just 2, Just 2, Just 4, Just 4]

-- Get a random element from a given list
getRandomFromList :: [a] -> StdGen -> (a, StdGen)
getRandomFromList list g = (list !! i, g')
    where (i, g') = randomR (0, length list - 1) g

-- Place a given tile on the given position in the board
-- Earlier work taken from lab 3
placeTile :: Board -> Pos -> Tile -> Board
placeTile (Board rows) (y,x) tile = Board (rows !!= (y, newRow))
   where newRow = (rows !! y) !!= (x, tile)

-- Replaces the element at given index with given value
-- Earlier work taken from lab 3
(!!=) :: [a] -> (Int, a) -> [a]
xs !!= (i,y) = replaceNth (i,y) xs
  where replaceNth  _  [] = []
        replaceNth (i, y) (x:xs)
          | i == 0    = y:xs
          | otherwise = x:replaceNth (i-1, y) xs


-- Get all the positions of blanks in a board
-- Earlier work taken from lab 3
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
shiftLeftAndMerge tiles = addMergedTiles (listOfJust ++ replicate numOfNothings Nothing)
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



-----------------------------------------------------------------------------
-- Testing section

-- Generator for non empty boards
instance Arbitrary Board where
  arbitrary = do
    rows <- vectorOf 4 (vectorOf 4 tile)
    i <- arbitrary
    if prop_2048 (Board rows)
      then return (Board rows)
    else
      return (setupNewBoard 4 (mkStdGen i))

-- Generate a tile
tile :: Gen Tile
tile = do
    num <- choose (1,11) :: Gen Int
    frequency [(5, return $ Just (2^num)), (5, return Nothing)]

-- Property for a 2048 board, it can not be empty and should be a square
prop_2048 :: Board -> Bool
prop_2048 (Board rows) = length (filter isNothing (concat rows)) /= 0 && all validLength rows
    where validLength list = length list == length rows


-- Property for testing if a new tile is placed
prop_placeNewTile :: Board -> Int -> Property
prop_placeNewTile (Board rows) i = Nothing `elem` concat rows ==> placeNewTile (Board rows) (mkStdGen i) /= Board rows


-- Property for testing if board is a winning board
prop_won :: Board -> Bool
prop_won (Board rows) | won (Board rows) = Just 2048 `elem` concat rows
                      | otherwise        = Just 2048 `notElem` concat rows

-- Property for testing if board is a losing board, TODO: something more?
prop_lost :: Board -> Bool
prop_lost (Board rows) | lost (Board rows) = Nothing `notElem` concat rows
                       | otherwise         = Nothing `elem` concat rows || any canMakeMove rows || any canMakeMove (transpose rows)


-- Property for testig if we made a move
prop_move :: Board -> Int -> Bool
prop_move board i  | c == 'w' = all (testRow . reverse) (transpose rows)
                   | c == 's' = all testRow (transpose rows)
                   | c == 'a' = all (testRow . reverse) rows
                   | c == 'd' = all testRow rows
      where (Board rows) = move c board
            c = fst (getRandomFromList ['w','a','s','d'] (mkStdGen i))
            testRow row = length (takeWhile isNothing row) == length (filter isNothing row)


