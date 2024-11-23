module Game where
import Deck
import Error

{- Commands and instructions, representing moves to be made -}
type StackIndex = Int
type Count = Int
type FromStack = StackIndex
type ToStack = Int

-- An instruction is a top-level command.
data Instruction = Quit | Undo | GameCommand Command

-- A Command is a move to be played in the game.
data Command = Move Count FromStack ToStack
             | MoveStack FromStack ToStack
             | Draw
             | MoveFromDiscard StackIndex
             | MoveToPillar CardSource
             | MoveFromPillar Suit StackIndex
             | Solve

data CardSource = FromStack StackIndex | FromDiscard

{- Board representation -}

-- A column is a list of (Card, Bool) pairs, where the Bool
-- represents visibility: true for visible, false for hidden
type Column = [(Card, Bool)]

-- The pillars / foundation stacks are represented as Maybe Card
-- values, where Nothing represents an empty pillar and Just c 
-- denotes that card 'c' is at the top of the pillar.
-- Note that we don't need to store all cards since they are stored
-- in ascending order.
data Pillars = MkPillars {
        spades :: Maybe Value,
        clubs :: Maybe Value,
        hearts :: Maybe Value,
        diamonds :: Maybe Value
  }
  deriving (Show, Eq)

emptyPillars :: Pillars
emptyPillars = MkPillars {
        spades = Nothing,
        clubs = Nothing,
        hearts = Nothing,
        diamonds = Nothing
    }

-- The board consists of a deck, discard pile, pillars, and 7 columns.
data Board = MkBoard {
    boardDeck :: [Card],
    boardDiscard :: [Card],
    boardPillars :: Pillars,
    boardColumns :: [Column]
}
    deriving (Eq)



{- EXERCISE 3: Show instance for the board -}
{- We recommend writing helper functions. -}

showDeck :: [Card] -> String
showDeck deck = "Deck: " ++ show deck

showDiscard :: [Card] -> String
showDiscard discard = "Discard: " ++ show discard

showPillars :: Pillars -> String
showPillars pillars = "Pillars: " ++ show pillars

showColumns :: [Column] -> String
showColumns columns = 
    "Columns:\n" ++ 
    unlines (zipWith (\i col -> "  Column " ++ show i ++ ": " ++ show col) [1..] columns)

instance Show Board where
     show b = 
        "Board:\n" ++
        showDeck (boardDeck b) ++ "\n" ++
        showDiscard (boardDiscard b) ++ "\n" ++
        showPillars (boardPillars b) ++ "\n" ++
        showColumns (boardColumns b)

{- EXERCISE 4: Board Setup -}
markCards :: [Card] -> Column
markCards cards = map (\(c, isLast) -> (c, isLast)) (zip cards faceStates)
  where
    faceStates = replicate (length cards - 1) False ++ [True]


dealColumns :: Deck -> Int -> ([Column], Deck)
dealColumns deck numCols = (columns, remainingDeck)
  where
    (columns, remainingDeck) = foldl dealColumn ([], deck) [1..numCols]

    dealColumn :: ([Column], Deck) -> Int -> ([Column], Deck)
    dealColumn (cols, cards) n = 
        let (columnCards, rest) = splitAt n cards
            markedCards = markCards columnCards
        in (cols ++ [markedCards], rest)

setup :: Deck -> Board
setup d = MkBoard
    { boardDeck = restDeck
    , boardDiscard = []
    , boardPillars = emptyPillars
    , boardColumns = columns
    }
  where
    (columns, restDeck) = dealColumns d 7



{- EXERCISE 5: Win checking -}
isWon :: Board -> Bool
isWon b = all isKing [Spades, Clubs, Hearts, Diamonds]
  where
    pillars = boardPillars b
    isKing suit = getPillar pillars suit == Just King
	
{- Pillar helper functions -}
-- Gets the pillar for a given suit.
getPillar :: Pillars -> Suit -> Maybe Value
getPillar ps Spades = spades ps
getPillar ps Clubs = clubs ps
getPillar ps Hearts = hearts ps
getPillar ps Diamonds = diamonds ps

-- Decrements a pillar. 
decValue :: Maybe Value -> Maybe Value
decValue Nothing = Nothing
decValue (Just Ace) = Nothing
decValue (Just x) = Just (pred x)

-- Increments a pillar.
incValue :: Maybe Value -> Maybe Value
incValue Nothing = Just Ace
incValue (Just x) = Just (succ x)

-- Increments the pillar for a given suit.
incPillar :: Pillars -> Suit -> Pillars
incPillar ps Spades = ps { spades = incValue (spades ps) }
incPillar ps Clubs = ps { clubs = incValue (clubs ps) }
incPillar ps Hearts = ps { hearts = incValue (hearts ps) }
incPillar ps Diamonds = ps { diamonds = incValue (diamonds ps) }

-- Decrements the pillar for a given suit.
decPillar :: Pillars -> Suit -> Pillars
decPillar ps Spades = ps { spades = decValue $ spades ps }
decPillar ps Clubs = ps { clubs = decValue $ clubs ps }
decPillar ps Hearts = ps { hearts = decValue $ hearts ps }
decPillar ps Diamonds = ps { diamonds = decValue $ diamonds ps }

{- EXERCISE 6: Helper Functions -}

-- Flips the top card of all columns, if not already flipped
flipTopCard :: Column -> Column
flipTopCard [] = []
flipTopCard col =
    let (rest, (topCard, isVisible):remaining) = splitAt (length col - 1) col
    in if isVisible
       then col 
       else rest ++ [(topCard, True)]

flipCards :: Board -> Board
flipCards b = b { boardColumns = map flipTopCard (boardColumns b) }

-- Checks whether it's possible to stack the first card onto the second.
canStack :: Card -> Card -> Bool
canStack card onto = 
    (isRed card /= isRed onto) && 
    (cardValue card == pred (cardValue onto))

-- Updates a column at the given index
updateColumn :: Int -> Column -> [Column] -> [Column]
updateColumn n newCol columns =
    take n columns ++ [newCol] ++ drop (n + 1) columns

-- Checks whether it's possible to place a card onto a pillar.
canStackOnPillar :: Card -> Maybe Value -> Bool
canStackOnPillar card Nothing = cardValue card == Ace  
canStackOnPillar card (Just v) = cardValue card == succ v  


{- EXERCISE 7: Draw -}
draw :: Board -> Either Error Board
draw b
    | null (boardDeck b) && null (boardDiscard b) = Left DeckEmpty
    | null (boardDeck b) =
        let newDeck = reverse (boardDiscard b)
        in Right b { boardDeck = tail newDeck, boardDiscard = [head newDeck] }
    | otherwise =
        let (card:restDeck) = boardDeck b
        in Right b { boardDeck = restDeck, boardDiscard = card : boardDiscard b }

 

{- EXERCISE 8: Move -}
move :: Int -> Int -> Int -> Board -> Either Error Board
move count from to b
    | count <= 0 = Left InvalidCount
    | from < 0 || from >= length cols = Left InvalidStack
    | to < 0 || to >= length cols = Left InvalidStack
    | null sourceVisible = Left ColumnEmpty
    | count > length sourceVisible = Left MovingTooManyCards
    | null target && topSourceCardValue /= King = Left ColumnKing
    | not (canStack topSourceCard targetTopCard) = Left WrongOrder
    | otherwise =
        let updatedSource = sourceHidden ++ drop count sourceVisible
            updatedTarget = sourceToMove ++ target
            newCols = updateColumn to updatedTarget (updateColumn from updatedSource cols)
        in Right b { boardColumns = newCols }
  where
    cols = boardColumns b
    source = cols !! from
    target = cols !! to
    (sourceHidden, sourceVisible) = span (not . snd) source
    sourceToMove = take count sourceVisible
    topSourceCard = fst (head sourceToMove)
    topSourceCardValue = cardValue topSourceCard
    targetTopCard = if null target then error "No target card" else fst (head target)




{- EXERCISE 9: Move Stack -}
moveStack :: Int -> Int -> Board -> Either Error Board
moveStack from to b = 
    let count = length (filter snd (boardColumns b !! from))
    in move count from to b



{- EXERCISE 10: Move from Discard -}
moveFromDiscard :: Int -> Board -> Either Error Board
moveFromDiscard idx b
    | null (boardDiscard b) = Left DiscardEmpty  -- Using Error constructor
    | idx < 0 || idx >= length cols = Left InvalidStack
    | null target && cardValue discardTop /= King = Left ColumnKing
    | not (canStack discardTop targetTopCard) = Left WrongOrder
    | otherwise =
        let updatedDiscard = tail (boardDiscard b)
            updatedTarget = (discardTop, True) : target
            newCols = updateColumn idx updatedTarget cols
        in Right b { boardDiscard = updatedDiscard, boardColumns = newCols }
  where
    discardTop = head (boardDiscard b)  -- The top card from the discard pile
    cols = boardColumns b               -- The list of columns
    target = cols !! idx                -- Target column
    targetTopCard = if null target then error "Unexpected empty target" else fst (head target)





{- EXERCISE 11: Move to Pillar -} 
moveToPillar :: CardSource -> Board -> Either Error Board
moveToPillar (FromStack idx) b
    | idx < 0 || idx >= length cols = Left InvalidStack
    | null source = Left ColumnEmpty
    | not (canStackOnPillar topCard pillarTop) = Left WrongPillarOrder
    | otherwise =
        let updatedSource = tail source
            updatedPillars = incPillar pillars (cardSuit topCard)
            newCols = updateColumn idx updatedSource cols
        in Right b { boardColumns = newCols, boardPillars = updatedPillars }
  where
    cols = boardColumns b
    pillars = boardPillars b
    source = cols !! idx
    topCard = fst (head source)
    pillarTop = getPillar pillars (cardSuit topCard)

moveToPillar FromDiscard b
    | null (boardDiscard b) = Left DiscardEmpty
    | not (canStackOnPillar topCard pillarTop) = Left WrongPillarOrder
    | otherwise =
        let updatedDiscard = tail (boardDiscard b)
            updatedPillars = incPillar pillars (cardSuit topCard)
        in Right b { boardDiscard = updatedDiscard, boardPillars = updatedPillars }
  where
    topCard = head (boardDiscard b)
    pillars = boardPillars b
    pillarTop = getPillar pillars (cardSuit topCard)




            
{- EXERCISE 12: Move from Pillar -}
moveFromPillar :: Suit -> Int -> Board -> Either Error Board
moveFromPillar suit idx b
    | idx < 0 || idx >= length cols = Left InvalidStack  -- Invalid column index
    | pillarTop == Nothing = Left PillarEmpty            -- Empty pillar
    | null target && cardValue topCard /= King = Left ColumnKing
    | not (canStack topCard targetTopCard) = Left WrongOrder
    | otherwise =
        let updatedPillars = decPillar pillars suit       -- Remove the top card from the pillar
            updatedTarget = (topCard, True) : target     -- Add the card to the target column
            newCols = updateColumn idx updatedTarget cols
        in Right b { boardPillars = updatedPillars, boardColumns = newCols }
  where
    cols = boardColumns b               -- List of columns
    pillars = boardPillars b            -- Current state of pillars
    pillarTop = getPillar pillars suit  -- Top card of the specified pillar
    topCard = case pillarTop of
        Just v -> MkCard suit v         -- Extract card from the pillar
        Nothing -> error "Unexpected empty pillar"
    target = cols !! idx                -- Target column
    targetTopCard = if null target then error "Unexpected empty target" else fst (head target)



{- EXERCISE 13: Solve -}
solve :: Board -> Board
solve b
    | isWon b = b
    | otherwise = case tryCommands b of
        Just newBoard -> solve newBoard
        Nothing -> b

tryCommands :: Board -> Maybe Board
tryCommands b = foldr tryMove Nothing [0..length (boardColumns b) - 1]
  where
    tryMove idx acc = case acc of
        Just x -> Just x
        Nothing -> tryMoveToPillar idx b

tryMoveToPillar :: Int -> Board -> Maybe Board
tryMoveToPillar idx b = case moveToPillar (FromStack idx) b of
    Right newBoard -> Just newBoard
    Left _ -> Nothing



{- Scaffolding: This checks input indexes and calls the relevant functions -}
checkStackIndex :: Int -> Either Error ()
checkStackIndex x | x >= 0 && x <= 6 = return ()
                  | otherwise = Left InvalidStack

makeMove' :: Command -> Board -> Either Error Board
makeMove' (Move count from to) b = do
    checkStackIndex from
    checkStackIndex to
    move count from to b
makeMove' (MoveStack from to) b = do
    checkStackIndex from
    checkStackIndex to
    moveStack from to b
-- If deck nonempty, move a card from the top of the deck to the top of the discard pile
-- If deck empty, reverse discard pile and put it back as deck
makeMove' Draw b = draw b
makeMove' (MoveFromDiscard idx) b = checkStackIndex idx >> moveFromDiscard idx b
-- Take the top card from the given stack and move to pillar -- if possible
makeMove' (MoveToPillar source) b =
    case source of
        FromDiscard -> moveToPillar source b
        FromStack idx -> checkStackIndex idx >> moveToPillar source b
makeMove' (MoveFromPillar suit idx) b = checkStackIndex idx >> moveFromPillar suit idx b
makeMove' Solve b = Right $ solve b

makeMove :: Command -> Board -> Either Error Board
makeMove cmd b = fmap flipCards (makeMove' cmd b)
