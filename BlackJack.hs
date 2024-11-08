module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-----------------------A0
hand2 = 
    --Add (Card Ace Spades)
  --  (Add (Card (Numeric 3) Hearts)
    (Add (Card (Numeric 4) Clubs)
            (Add (Card Queen Hearts) Empty))

hand3 = 
   -- Add (Card Ace Spades)
   -- (Add (Card (Numeric 2) Hearts)
    (Add (Card (Numeric 10) Hearts)
            (Add (Card Ace Spades) Empty))


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

--TODO: FIX this lockally(with where)
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



-----------------------B1
(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty = Empty 
(<+) Empty hand' = hand'
(<+) hand Empty = hand
(<+) (Add card hand) hand' = Add card ((<+) hand hand')


prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3


prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size (p1 <+ p2) == (size p1) + (size p2)



-----------------------B2
mixcards :: [Rank] -> [Suit] -> [Card]
mixcards ranks suits = [ Card r s | r <- ranks, s <- suits]

convertToHand :: [Card] -> Hand
convertToHand [] = Empty
convertToHand (card:cards) = Add card (convertToHand cards)

fullDeck :: Hand
fullDeck = convertToHand (mixcards ranks suits) 
    where 
        suits = [Hearts, Spades, Diamonds,Clubs]
        ranks = [Numeric 2,Numeric 3,Numeric 4,Numeric 5,Numeric 6,
                Numeric 7,Numeric 8,Numeric 9,Numeric 10,Jack,Queen,King,Ace]




