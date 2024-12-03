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

instance Show Board where
    show b =
        unlines [
            showDeckSize (boardDeck b),
            showDiscard (boardDiscard b),
            showPillars (boardPillars b),
            showColumnHeaders,
            showColumns (boardColumns b)
        ]

-- Show the size of the deck
showDeckSize :: [Card] -> String
showDeckSize deck = "Deck size: " ++ show (length deck)

-- Show the discard pile
showDiscard :: [Card] -> String
showDiscard discard = "Discard: " ++ unwords (map show (take 3 discard))

-- Show the pillars
showPillars :: Pillars -> String
showPillars pillars =
    "Pillars:\n" ++
    "  Spades:   " ++ showPillar (spades pillars) ++ "\n" ++
    "  Clubs:    " ++ showPillar (clubs pillars) ++ "\n" ++
    "  Hearts:   " ++ showPillar (hearts pillars) ++ "\n" ++
    "  Diamonds: " ++ showPillar (diamonds pillars)

showPillar :: Maybe Value -> String
showPillar Nothing = "<empty>"
showPillar (Just v) = show v

-- Show the column headers
showColumnHeaders :: String
showColumnHeaders = unwords [ "[" ++ show i ++ "]" | i <- [0..6 :: Int] ]

-- Show the columns
showColumns :: [Column] -> String
showColumns columns =
    let maxHeight = maximum (map length columns)
    in unlines [unwords [showColumnCard (getRow col row) | col <- columns] | row <- [0..maxHeight-1]]

-- Get a card from a row
getRow :: Column -> Int -> Maybe (Card, Bool)
getRow col row
    | row < length col = Just (col !! row)
    | otherwise = Nothing

-- Format a card
showColumnCard :: Maybe (Card, Bool) -> String
showColumnCard (Just (card, True))  = show card    
showColumnCard (Just (_, False)) = "???"           
showColumnCard Nothing = "   "                     

{- EXERCISE 4: Board Setup -}
cardUpOrDown :: [Card] -> Column
cardUpOrDown cards = map (\(c, isLast) -> (c, isLast)) (zip cards faceStates)
  where
    faceStates = replicate (length cards - 1) False ++ [True]


dealColumns :: Deck -> Int -> ([Column], Deck)
dealColumns deck numCols = (columns, remainingDeck)
  where
    (columns, remainingDeck) = foldl dealColumn ([], deck) [1..numCols]

    dealColumn :: ([Column], Deck) -> Int -> ([Column], Deck)
    dealColumn (cols, cards) n = 
        let (columnCards, rest) = splitAt n cards
            markedCards = cardUpOrDown columnCards
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
isWon b = all isKing [Spades, Clubs, Hearts, Diamonds] -- To who ever is reading this, I can never play solitaire again...
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
flipTopCard col
    | snd (last col) = col 
    | otherwise = init col ++ [(fst (last col), True)]  

flipCards :: Board -> Board
flipCards b = b { boardColumns = map flipTopCard (boardColumns b) }

-- Checks whether it's possible to stack the first card onto the second.
colour :: Suit -> String
colour Hearts   = "red"
colour Diamonds = "red"
colour Clubs    = "black"
colour Spades   = "black"

canStack :: Card -> Card -> Bool
canStack card1 card2 =
  colour (cardSuit card1) /= colour (cardSuit card2) &&
  fromEnum (cardValue card1) == fromEnum (cardValue card2) - 1

-- Updates a column at the given index
updateColumn :: Int -> Column -> [Column] -> [Column]
updateColumn n newCol columns =
    take n columns ++ [newCol] ++ drop (n + 1) columns

-- Checks whether it's possible to place a card onto a pillar.
canStackOnPillar :: Card -> Maybe Value -> Bool
canStackOnPillar card Nothing = cardValue card == Ace
canStackOnPillar card (Just v) = cardValue card /= King && cardValue card == succ v
  
{- EXERCISE 7: Draw -}
draw :: Board -> Either Error Board
draw board
    | null deck && null discard = Left DeckEmpty  
    | null deck = 
        let newDeck = reverse discard
            newDiscard = [head newDeck]
        in Right board { boardDeck = tail newDeck, boardDiscard = newDiscard }
    | otherwise =  
        let (card:restDeck) = deck
            updatedDiscard = card : discard
        in Right board { boardDeck = restDeck, boardDiscard = updatedDiscard }
  where
    deck = boardDeck board
    discard = boardDiscard board

{- EXERCISE 8: Move -}
isKing :: Card -> Bool
isKing card = cardValue card == King

move :: Int -> Int -> Int -> Board -> Either Error Board
move n s1 s2 board
    | s1 < 0 || s1 >= length columns || s2 < 0 || s2 >= length columns = Left InvalidStack
    | n < 1 = Left InvalidCount
    | null fromCol = Left ColumnEmpty
    | n > length visibleCards = Left MovingTooManyCards
    | null toCol && not (isKing topCard) = Left ColumnKing
    | not (null toCol) && not (canStack topCard (fst (last toCol))) = Left WrongOrder
    | otherwise = Right $ board { boardColumns = updatedColumns }
  where
    columns = boardColumns board
    fromCol = columns !! s1
    toCol = columns !! s2

    visibleCards = reverse $ takeWhile snd $ reverse fromCol

    cardsToMove = take n visibleCards

    topCard = fst $ head cardsToMove

    newFromCol = take (length fromCol - n) fromCol
    newToCol = toCol ++ cardsToMove
    updatedColumns = updateColumn s1 newFromCol $ updateColumn s2 newToCol columns


{- EXERCISE 9: Move Stack -}
moveStack :: Int -> Int -> Board -> Either Error Board
moveStack s1 s2 board
    | s1 < 0 || s1 >= length columns || s2 < 0 || s2 >= length columns = Left InvalidStack
    | null visibleCards = Left ColumnEmpty
    | otherwise = move (length visibleCards) s1 s2 board
  where
    columns = boardColumns board
    fromCol = columns !! s1

    visibleCards = reverse $ takeWhile snd $ reverse fromCol



{- EXERCISE 10: Move from Discard -}
moveFromDiscard :: Int -> Board -> Either Error Board
moveFromDiscard s board
    | null discard = Left DiscardEmpty
    | s < 0 || s >= length columns = Left InvalidStack
    | null targetCol && not (isKing topDiscard) = Left ColumnKing
    | not (null targetCol) && not (canStack topDiscard (fst $ last targetCol)) = Left WrongOrder
    | otherwise = Right $ board { 
        boardDiscard = tail discard, 
        boardColumns = updateColumn s (targetCol ++ [(topDiscard, True)]) columns 
      }
  where
    discard = boardDiscard board
    columns = boardColumns board
    targetCol = columns !! s

    topDiscard = head discard

{- EXERCISE 11: Move to Pillar -} 
moveToPillar :: CardSource -> Board -> Either Error Board
moveToPillar cardSource board = case cardSource of
    FromDiscard ->
        if null discard
        then Left DiscardEmpty
        else moveCardToPillar topDiscard board { boardDiscard = tail discard }
    FromStack s ->
        if s < 0 || s >= length columns
        then Left InvalidStack
        else if null (columns !! s)
        then Left ColumnEmpty
        else 
            let topCardFromStack = fst (last (columns !! s))
                newColumn = init (columns !! s)
            in moveCardToPillar topCardFromStack board { boardColumns = updateColumn s newColumn columns }
  where
    discard = boardDiscard board
    columns = boardColumns board
    pillars = boardPillars board

    topDiscard = head discard

    moveCardToPillar :: Card -> Board -> Either Error Board
    moveCardToPillar card b =
        let suit = cardSuit card
        in case suit of
            Spades   -> tryAddToPillar spades (\p -> b { boardPillars = pillars { spades = p } })
            Clubs    -> tryAddToPillar clubs (\p -> b { boardPillars = pillars { clubs = p } })
            Hearts   -> tryAddToPillar hearts (\p -> b { boardPillars = pillars { hearts = p } })
            Diamonds -> tryAddToPillar diamonds (\p -> b { boardPillars = pillars { diamonds = p } })
      where
        tryAddToPillar :: (Pillars -> Maybe Value) -> (Maybe Value -> Board) -> Either Error Board
        tryAddToPillar getPillar updateBoard =
            let currentValue = getPillar pillars
            in if canAddToPillar card currentValue
               then Right (updateBoard (incValue currentValue))
               else Left WrongPillarOrder

canAddToPillar :: Card -> Maybe Value -> Bool
canAddToPillar card Nothing = cardValue card == Ace
canAddToPillar card (Just v) = cardValue card == succ v

            
{- EXERCISE 12: Move from Pillar -}
updatePillar :: Suit -> Maybe Value -> Pillars -> Pillars
updatePillar Spades value pillars = pillars { spades = value }
updatePillar Clubs value pillars = pillars { clubs = value }
updatePillar Hearts value pillars = pillars { hearts = value }
updatePillar Diamonds value pillars = pillars { diamonds = value }

moveFromPillar :: Suit -> Int -> Board -> Either Error Board
moveFromPillar suit s board
    | pillarValue == Nothing = Left PillarEmpty
    | s < 0 || s >= length columns = Left InvalidStack
    | null targetCol && not (isKing topPillarCard) = Left ColumnKing
    | not (null targetCol) && not (canStack topPillarCard (fst $ last targetCol)) = Left WrongOrder
    | otherwise = Right $ board {
        boardPillars = updatePillar suit (decValue pillarValue) pillars,
        boardColumns = updateColumn s (targetCol ++ [(topPillarCard, True)]) columns
      }
  where
    pillars = boardPillars board
    columns = boardColumns board
    targetCol = columns !! s

    pillarValue = getPillar pillars suit

    topPillarCard = case pillarValue of
        Nothing -> error "Bro your pillars empty"
        Just value -> MkCard suit value

{- EXERCISE 13: Solve -}
solve :: Board -> Board
solve board
  | board == nextBoard = board
  | otherwise = solve nextBoard
  where
    nextBoard = solveStep board

solveStep :: Board -> Board
solveStep board = foldl tryMove board [0 .. (length (boardColumns board) - 1)]

tryMove :: Board -> StackIndex -> Board
tryMove board idx =
  case moveToPillar (FromStack idx) board of
    Right updatedBoard -> updatedBoard
    Left _ -> board



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
