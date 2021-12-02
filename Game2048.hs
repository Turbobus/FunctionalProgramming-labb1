
import Test.QuickCheck ()
import Data.List ()
import Data.Maybe ()
import System.Random (Random (randomR))

-- Representation of 2048 puzzles 
type Tile = Maybe Int 
type Row  = [Tile]    

newtype Board = Board [Row]
 deriving ( Show, Eq )

rows :: Board -> [Row]
rows (Board ms) = ms


setupNewBoard :: Board
setupNewBoard = undefined

getRandomTile :: Tile
getRandomTile = randomR ((a, a)) g;

printBoard :: Board -> IO ()
printBoard = undefined;

-- Functions for moving the board
moveRight = undefined;

moveLeft = undefined;

moveUp = undefined;

moveDown = undefined;

-- Functions for multiplying cells

multiply = undefined