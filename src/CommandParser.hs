module CommandParser where

import Text.Parsec
import Deck
import qualified Game as G
import Error
import Control.Monad (void)

type Parser a = Parsec String () a

draw :: Parser G.Command
draw = (keyword "draw" <|> keyword "d") >> return G.Draw

solve :: Parser G.Command
solve = keyword "solve" >> return G.Solve

int :: Parser Int
int = do
    spaces
    digits <- many1 digit
    spaces
    return (read digits :: Int)

keyword :: String -> Parser ()
keyword str = spaces >> try (void $ string str) >> spaces

moveLong :: Parser G.Command
moveLong = do
    _ <- keyword "move"
    cardCount <- int
    _ <- keyword "from"
    from <- int
    _ <- keyword "to"
    to <- int
    return $ G.Move cardCount from to

moveShort :: Parser G.Command
moveShort = (const G.Move) <$> (keyword "mv") <*> int <*> int <*> int

move :: Parser G.Command
move = moveLong <|> moveShort

moveStack :: Parser G.Command
moveStack = (const G.MoveStack) <$> ((keyword "movest" <|> keyword "mvs")) <*> int <*> int

suit :: Parser Suit
suit =
    (keyword "hearts" >> return Hearts)
    <|>
    (keyword "diamonds" >> return Diamonds)
    <|>
    (keyword "spades" >> return Spades)
    <|>
    (keyword "clubs" >> return Clubs)


moveFromDiscard :: Parser G.Command
moveFromDiscard = keyword "movefd" >> int >>= return . G.MoveFromDiscard

cardSource :: Parser G.CardSource
cardSource = ((keyword "discard" <|> keyword "d") >> return G.FromDiscard) <|> (int >>= return . G.FromStack)

moveToPillar :: Parser G.Command
moveToPillar = keyword "movetp" >> cardSource >>= return . G.MoveToPillar

moveFromPillar :: Parser G.Command
moveFromPillar = do
    _ <- keyword "movefp"
    s <- suit
    to <- int
    return $ G.MoveFromPillar s to

command :: Parser G.Command
command = draw <|> moveFromDiscard <|> moveToPillar <|> moveFromPillar <|> moveStack <|> move <|> solve

instruction :: Parser G.Instruction
instruction =
    (keyword "quit" >> return G.Quit)
    <|> (keyword "undo" >> return G.Undo)
    <|> (command >>= return . G.GameCommand)

parseInstruction :: String -> Either Error G.Instruction
parseInstruction s =
  case parse instruction "" s of
    Left _ -> Left InvalidCommand 
    Right instr -> Right instr

