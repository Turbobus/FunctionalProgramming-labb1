module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

hand2 = Add (Card (Numeric 9) Hearts)
            (Add (Card Ace Spades)
            (Add (Card (Numeric 9) Spades) Empty))
            
hand3 = Add (Card (Numeric 10) Hearts)
            (Add (Card Ace Spades)
            (Add (Card (Numeric 10) Spades) 
            (Add (Card Ace Spades) Empty)))
        

david = Add (Card Ace Hearts)
            (Add (Card Ace Spades) Empty)

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


-- Display the rank and suit of the card
displayCard :: Card -> String
displayCard card = show (rank card) ++ " of " ++ show (suit card) ++ " \n"

-- Display all the cards in the hand
display :: Hand -> String
display Empty           = "\n"
display (Add card hand) = displayCard card ++ display hand

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

-- Checks if player is bust, which means it's game over
gameOver :: Hand -> Bool
gameOver hand | points > 21 = True
              | otherwise   = False
  where points = value hand

-- Returns the winner of the Game
winner :: Hand -> Hand -> Player
winner guestHand bankHand | gameOver guestHand               = Bank
                          | gameOver bankHand                = Guest
                          | value guestHand > value bankHand = Guest
                          | otherwise                        = Bank
                      
-- Properties for QuickCheck
prop_value :: Hand -> Bool
prop_value hand = value hand >= 0 

prop_valueRank :: Rank -> Bool
prop_valueRank rank = 11 > valueRank rank  &&  valueRank rank > 0

prop_gameOver :: Hand -> Bool
prop_gameOver hand = gameOver hand || not (gameOver hand)

prop_winner :: Hand -> Hand -> Bool
prop_winner guestHand bankHand = winner guestHand bankHand == Bank || winner guestHand bankHand == Guest