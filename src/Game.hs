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
    show b = error "fill in 'Show' instance for Board data type in Game.hs"

{- EXERCISE 4: Board Setup -}
setup :: Deck -> Board
setup d = error "fill in 'setup' in Game.hs"



{- EXERCISE 5: Win checking -}
isWon :: Board -> Bool
isWon b = error "fill in 'isWon' in Game.hs"

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
flipCards :: Board -> Board
flipCards b = error "fill in 'flipCards' in Game.hs"

-- Checks whether it's possible to stack the first card onto the second.
canStack :: Card -> Card -> Bool
canStack card onto = error "fill in 'canStack' in Game.hs"

-- Updates a column at the given index
updateColumn :: Int -> Column -> [Column] -> [Column]
updateColumn n c cs = error "fill in 'updateColumn' in Game.hs"

-- Checks whether it's possible to place a card onto a pillar.
canStackOnPillar :: Card -> Maybe Value -> Bool
canStackOnPillar c mv = error "fill in 'canStackOnPillar' in Game.hs"

{- EXERCISE 7: Draw -}
draw :: Board -> Either Error Board
draw b = error "fill in 'draw' in Game.hs" 

{- EXERCISE 8: Move -}
move :: Int -> Int -> Int -> Board -> Either Error Board
move count from to b = error "fill in 'move' in Game.hs"

{- EXERCISE 9: Move Stack -}
moveStack :: Int -> Int -> Board -> Either Error Board
moveStack from to b = error "fill in 'moveStack' in Game.hs"

{- EXERCISE 10: Move from Discard -}
moveFromDiscard :: Int -> Board -> Either Error Board
moveFromDiscard idx b = error "fill in 'moveFromDiscard' in Game.hs"

{- EXERCISE 11: Move to Pillar -} 
moveToPillar :: CardSource -> Board -> Either Error Board
moveToPillar cs b = error "fill in 'moveToPillar' in Game.hs"
            
{- EXERCISE 12: Move from Pillar -}
moveFromPillar :: Suit -> Int -> Board -> Either Error Board
moveFromPillar suit idx b = error "fill in 'moveFromPillar' in Game.hs"

{- EXERCISE 13: Solve -}
solve :: Board -> Board
solve board = error "fill in 'solve' in Game.hs"




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
