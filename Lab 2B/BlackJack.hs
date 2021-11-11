module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random


hand1 :: Hand
hand1 = Add (Card (Numeric 5) Spades)
            (Add (Card Queen Hearts) Empty)
            
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)



-- A0
{-
size hand2
  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))

  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size Empty
  = 1 + 1 + 0
  = 2
-}

-- Computational steps for calculation size of hand2
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            , 2]



-- A1

-- Display the rank and suit of the card
displayCard :: Card -> String
displayCard (Card (Numeric i) s ) = show i ++ " of " ++ show s ++ " \n"
displayCard (Card r s ) = show r ++ " of " ++ show s ++ " \n"

-- Display all the cards in the hand
display :: Hand -> String
display Empty           = "\n"
display (Add card hand) = displayCard card ++ display hand



-- A2

-- Returns the value of the hand
value :: Hand -> Integer
value Empty           = 0
value (Add card hand)
   | totalValue > 21 = calculateHandValue (Add card hand) 1
   | otherwise       = totalValue
   where totalValue = calculateHandValue (Add card hand) 11

-- Returns the value representing the rank
valueRank :: Rank -> Integer
valueRank (Numeric i) = i
valueRank _           = 10

-- Calculate the value of each card in hand with given value for an ace card
calculateHandValue :: Hand -> Integer -> Integer
calculateHandValue Empty i           = 0
calculateHandValue (Add card hand) i
    | rank card == Ace = i + calculateHandValue hand i
    | otherwise        = valueRank (rank card) + calculateHandValue hand i



-- A3

-- Checks if player is bust, which means it's game over
gameOver :: Hand -> Bool
gameOver hand | points > 21 = True
              | otherwise   = False
  where points = value hand



-- A4

-- Returns the winner of the Game
winner :: Hand -> Hand -> Player
winner guestHand bankHand | gameOver guestHand               = Bank
                          | gameOver bankHand                = Guest
                          | value guestHand > value bankHand = Guest
                          | otherwise                        = Bank


-- B1
-- Defining operation to add two hands togheter
(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty             = Empty
(<+) hand1 Empty             = hand1
(<+) Empty hand2             = hand2
(<+) (Add card1 hand1) hand2 = Add card1 (hand1 <+ hand2)


-- B2
-- Returns a hand of all cards in a deck using list comprehension
fullDeck :: Hand
fullDeck = foldr Add Empty cardList 
  where cardList = [Card rank suit | rank <- rankList
                                   , suit <- [Hearts, Spades, Clubs, Diamonds]]
        rankList = Jack : Queen : King : Ace : [Numeric x | x <- [2..10]] 


-- B3
-- Draw a card form a given deck and add it to a given hand
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add cardDeck deck) hand = (deck, Add cardDeck hand) 



-- B4
-- Play the turns for the bank
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

-- Helper for playing the banks turns
playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck bankHand 
      | value bankHand < 16 = playBankHelper smallerDeck biggerHand
      | otherwise = bankHand
  where (smallerDeck , biggerHand) = draw deck bankHand  



-- B5
-- Shuffels a given deck of cards
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck _ Empty = Empty
shuffleDeck g deck  
  | deckSize > 0 = Add card (shuffleDeck g' (removeCardFromDeck card deck))
   where card       = getNthCard rand deck
         (rand, g') = randomR (0, deckSize-1) g
         deckSize   = size deck

-- Get the Card of given index from deck
getNthCard :: Int -> Hand -> Card
getNthCard _ Empty                       = error "No deck to get card from"
getNthCard i (Add card deck) | i > 0     = getNthCard (i-1) deck
                             | otherwise = card

-- Removes the given Card from the given deck
removeCardFromDeck :: Card -> Hand -> Hand
removeCardFromDeck  _ Empty               = Empty
removeCardFromDeck  card (Add card2 deck) 
               | card == card2 = deck
               | otherwise     = Add card2 (removeCardFromDeck card deck)


-- B6

-- Our implementation for the different functions
implementation :: Interface
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

-- The main game function
main :: IO ()
main = runGame implementation

-- Properties for QuickCheck
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffleDeck g hand )

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = size hand1 + size hand2 == size (hand1 <+ hand2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_value :: Hand -> Bool
prop_value hand = value hand >= 0

prop_valueRank :: Rank -> Bool
prop_valueRank rank = 11 > valueRank rank  &&  valueRank rank > 0

prop_gameOver :: Hand -> Bool
prop_gameOver hand = gameOver hand || not (gameOver hand)

prop_winner :: Hand -> Hand -> Bool
prop_winner guestHand bankHand =
  winner guestHand bankHand == Bank || winner guestHand bankHand == Guest