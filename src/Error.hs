module Error where

-- Errors that can occur when trying to make a move
data Error = ColumnEmpty -- Trying to move from empty column
           | PillarEmpty -- Trying to take from empty pillar
           | DiscardEmpty -- Trying to take from empty discard
           | ColumnKing -- Trying to start a column with non-king
           | WrongOrder -- Trying to move card with unsupported order
           | WrongPillarOrder -- Trying to add card to pillar in non-ascending order
           | MovingTooManyCards -- Trying to move more cards than available
           | InvalidCount -- Trying to move an invalid number of cards
           | InvalidStack -- Invalid stack number
           | InvalidCommand -- Couldn't parse command
           | DeckEmpty -- Cannot draw from empty deck

instance Show Error where
    show ColumnEmpty = "You cannot move cards from an empty column."
    show PillarEmpty = "You cannot move cards from an empty pillar."
    show DiscardEmpty = "You cannot move cards from an empty discard pile."
    show ColumnKing = "A new column can only be started using a King."
    show WrongOrder = "Cards can only be placed in descending order, with alternating colours."
    show WrongPillarOrder = "Pillars must be placed in ascending order."
    show MovingTooManyCards = "You are trying to move too many cards." 
    show InvalidCount = "You are trying to move an invalid number of cards."
    show InvalidStack = "Invalid stack index (must be between 0 and 6)"
    show InvalidCommand = "Invalid command."
    show DeckEmpty = "Cannot draw from empty deck."


