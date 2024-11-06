module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-----------------------A0
hand2 = 
    --Add (Card Ace Spades)
    (Add (Card (Numeric 3) Hearts)
    (Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)))

hand3 = 
   -- Add (Card Ace Spades)
   -- (Add (Card (Numeric 2) Hearts)
    (Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty))


cardEx :: Card
cardEx = Card Jack Hearts 


            
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size  (Add (Card Jack Spades) Empty)
            , 2 + size Empty
            ,2]


-----------------------A1 
displayCard :: Card -> String 
displayCard c = 
    case rank c of 
        Numeric n -> show n ++ " of " ++ show (suit c) ++ "\n"
        _         -> show (rank c) ++ " of " ++ show (suit c) ++ "\n"

display :: Hand -> String
display Empty = []
display (Add card hand) = displayCard card ++ display hand


-----------------------A2
valueRank :: Rank -> Integer
valueRank r = 
    case r of 
        Numeric n -> n
        Jack -> 10
        Queen -> 10
        King -> 10 
        Ace -> 11

initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add card hand) = valueRank (rank card) + initialValue hand


numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) = 
    case rank (card) of 
        Ace -> 1 + numberOfAces hand
        _   -> numberOfAces hand


value :: Hand -> Integer
value hand 
    | initialValue hand <= 21 = initialValue hand
    | initialValue hand > 21 = initialValue hand - ((numberOfAces hand) * 10)



    
-----------------------A3
gameOver :: Hand -> Bool
gameOver hand 
   | value hand <= 21 = False
   | value hand > 21 = True


-----------------------A4
winner :: Hand -> Hand -> Player
winner guest bank
    | gameOver guest && gameOver bank = Bank 
    | gameOver bank || value guest > value bank = Guest
    | gameOver guest || value guest < value bank = Bank
    | value guest == value bank = Bank 



