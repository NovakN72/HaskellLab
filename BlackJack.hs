module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

{- Lab 2B
   Date: November 14th 2024
   Authors: Narimaun Novak, Martin Berntsson
   Lab group: Group 48
 -}




-----------------------A0
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

            
sizeSteps :: [Integer]
sizeSteps = [ size hand2 
            , size (Add (Card (Numeric 2) Hearts) 
                        (Add (Card Jack Spades) Empty))
            , 1 + size  (Add (Card Jack Spades) Empty)
            , 2 + size Empty
            ,2]


-----------------------A1 
--The card is numeric, return the number, otherwise its king,queen,jack or ace
displayCard :: Card -> String 
displayCard c = 
    case rank c of  
        Numeric n -> show n ++ " of " ++ show (suit c) ++ "\n"    
        _         -> show (rank c) ++ " of " ++ show (suit c) ++ "\n" 

--Recusively go through the hand and display every card
display :: Hand -> String
display Empty = []
display (Add card hand) = displayCard card ++ display hand


-----------------------A2
--The value of Empty is zero, otherwise Recusively go through the hand and calculate
--the initial value. valueRank here is a local function and it just returns the value of different
--ranks. 
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add card hand) = valueRank (rank card) + initialValue hand
    where   
        valueRank :: Rank -> Integer
        valueRank r = 
            case r of 
                 Numeric n -> n
                 Jack      -> 10
                 Queen     -> 10
                 King      -> 10 
                 Ace       -> 11

--Goes through hand and returns the number of aces.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) = 
    case rank (card) of 
        Ace -> 1 + numberOfAces hand
        _   -> numberOfAces hand

--Counts the value of hand, it will return the value if it is less than or equal to 21
--Otherwise decrease the value with (numberOfAces * 10)
value :: Hand -> Integer
value hand 
    | initialValue hand <= 21 = initialValue hand
    | initialValue hand > 21 = initialValue hand - ((numberOfAces hand) * 10)


    
-----------------------A3
--If the value of hand is more than 21, return False, otherwise True.
gameOver :: Hand -> Bool
gameOver hand 
   | value hand <= 21 = False
   | value hand > 21 = True


-----------------------A4
winner :: Hand -> Hand -> Player
winner guest bank
    | gameOver guest                  = Bank --Guest is bust then bank wins
    | gameOver bank                   = Guest --Bank is bust, guest wins
    | value guest > value bank        = Guest --Value of guests hand is more than value of bank, guest wins
    | value guest <= value bank       = Bank  --Value of bank is more than value of guest's hand, bank wins
     


-----------------------B1
--Takes a hand, and recursively adds it to to the other hand. 
(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty           = Empty 
(<+) Empty hand'           = hand'
(<+) hand Empty            = hand
(<+) (Add card hand) hand' = Add card ((<+) hand hand')

--Checking for assosiation properties 
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

--Checking if sizes sizes of the aftermatch is the same as both hands together.
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size (p1 <+ p2) == (size p1) + (size p2)



-----------------------B2
-- Makes a list of cards using list comprehension. 
fullDeckList :: [Rank] -> [Suit] -> [Card]
fullDeckList ranks suits = [ Card r s | r <- ranks, s <- suits]

--Coverts the list of hand to the convetional Hand data type. 
convertToHand :: [Card] -> Hand
convertToHand []           = Empty
convertToHand (card:cards) = Add card (convertToHand cards)

--Builds a fulldeck using the helper methods above. 
fullDeck :: Hand
fullDeck      = convertToHand (fullDeckList ranks suits) 
    where 
        suits = [Hearts, Spades, Diamonds,Clubs]
        ranks = [Numeric 2,Numeric 3,Numeric 4,Numeric 5,Numeric 6,
                Numeric 7,Numeric 8,Numeric 9,Numeric 10,Jack,Queen,King,Ace]


-----------------------B3
--Draw a card for the deck, returns an erro if the deck is empty.
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand'           = error "draw: The deck is empty."  
draw (Add card hand) hand' = (hand,Add card hand')


-----------------------B4
--Draws a card for the bank, stops drawing a hand if the value of the banks hand is equal to or-
--less than 16.
playBankHelper :: Hand -> Hand -> (Hand,Hand)
playBankHelper deck hand = 
    case (draw deck hand) of 
        (smallerDeck,biggerHand) 
            | value biggerHand < 16 -> playBankHelper smallerDeck biggerHand
            | otherwise             -> (smallerDeck,biggerHand)

--Takes the second argument of playBankHelper
playBank :: Hand -> Hand
playBank hand = second (playBankHelper hand Empty)
    where 
        second :: (a, b) -> b 
        second (x,y) = y

-----------------------B5 
--Returns the Nth card in the deck.
getNth :: Integer -> Hand -> Card
getNth 0 (Add card hand) = card 
getNth n (Add card hand) 
    | n > 0              = getNth (n-1) hand
    | otherwise          = error "index out of bounds"

--Deletes the Nth card in the deck and returns the deck.
deleteNth :: Integer -> Hand -> Hand
deleteNth 0 (Add card hand) = hand
deleteNth n (Add card hand) = Add card (deleteNth (n-1) hand)
   
--uses stdGen to make random indexes to take cards from a non-shuffled deck to a shuffled deck.
--Recursively Continues untill the unshuffled deck is Empty.  
shuffleDeckHelper :: StdGen -> Hand -> Hand -> (Hand,Hand)
shuffleDeckHelper g Empty shuffledDeck = (Empty,shuffledDeck)
shuffleDeckHelper g deck shuffledDeck  = 
    let 
        (index, g') = randomR (0, ((size deck) - 1)) g
        card        = getNth index deck
        newDeck     = deleteNth index deck
    in shuffleDeckHelper g' newDeck (Add card shuffledDeck)
       
--Returns the shuffled deck from the helper function above. 
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g deck = second (shuffleDeckHelper g deck Empty)
        where 
          second :: (a, b) -> b 
          second (x,y) = y


prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool 
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h


belongsTo :: Card -> Hand -> Bool 
c `belongsTo` Empty = False 
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h


prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h)


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


main :: IO () 
main = runGame implementation