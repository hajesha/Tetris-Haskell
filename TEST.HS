module Main where
    import Text.Printf (printf)
    import Test.HUnit
    import Data.Set (empty,fromList,member,delete,insert,Set)
    import Data.Map (empty,adjust,alter,foldlWithKey,elems,fromSet,Map)
    main = do
         input <- getContents
         putStr $ show $ readFrom input
    data PowerUp = Double | Escalator | AntiVenom deriving (Show,Eq)
    instance Read PowerUp where
        readsPrec _ value = tryParse value [("double",Double),("escalator",Escalator),("antivenom",AntiVenom)]
    tryParse value [] = []
    tryParse (' ':value) list = tryParse value list
    tryParse value ((attempt, result):xs) =
        if (take (length attempt) value) == attempt
        then [(result, drop (length attempt) value)]
        else tryParse value xs
    class Disp a where
        disp :: a -> String
    instance Disp PowerUp where
        disp Double = "d"
        disp Escalator = "e"
        disp AntiVenom = "a"
    data Square = Empty | Ladder Int | Snake Int | PowerUp PowerUp deriving (Show, Read, Eq)
    instance Disp Square where
        disp Empty = "  "
        disp (Ladder _) = " L"
        disp (Snake _) = " S"
        disp (PowerUp p) = disp p ++ " "
    data Player = Player String (Set PowerUp) Int deriving (Show,Eq)
    type BoardCell = (Square,Int,Maybe Player)
    type Board = Map Int Square
    type Dice = [Int]
    type Players = Map String Player
    data Game = Game Board Players Int Int Dice
    instance Show Game where
        show (Game board players width size dice) =
            possibleWinner width size players $ leftToRight board players width $ take size $ iterate succ 1

    leftToRight _ _ _ [] = ""
    leftToRight board players width boardNumbers =
        (rightToLeft board players width $ drop width boardNumbers) ++
        (formatRow board players $ take width boardNumbers)
    rightToLeft _ _ _ [] = ""
    rightToLeft board players width boardNumbers =
        (leftToRight board players width $ drop width boardNumbers) ++
        (formatRow board players $ reverse $ take width boardNumbers)
    rowBreak row =
        foldr (\x -> \y -> "+---" ++ y) "+\n" row
    formatRow board players row =
        rowBreak row ++
        foldr (\x -> \y -> "|" ++ row1 x ++ y) "|\n" row ++
        foldr (\x -> \y -> "|" ++ row2 x ++ y) "|\n" row

    row1,row2 :: BoardCell -> String
    row1 (square,int,player) = printf "%3d" int
    row2 (square,int,Just (Player name _ _)) = name ++ disp square
    row2 (square,int,_) = " " ++ disp square
    possibleWinner width size players string = string ++ rowBreak (replicate width 0)

    emptyBoard = Game Data.Map.empty Data.Map.empty 1 1 (cycle [1])
    readFrom = foldl doLine emptyBoard . lines
    doLine game = doCommand game . words

    doCommand :: Game -> [String] -> Game
    doCommand (Game board players width size dice) ["board",cols,rows] =
              Game board players width (int rows*width) dice
                   where width = int cols
    doCommand (Game board players width size _) ("dice":sequence) =
              Game board players width size (cycle $ map int sequence)
    doCommand (Game board _ width size dice) ["players",number] =
              Game board (foldl (placeAt 1 board) players (elems players)) width size dice
                   where players = fromSet (\n -> Player n Data.Set.empty 0) fromList $ take (int number) $ map (flip (:) []) $ iterate succ 'A'
    doCommand (Game board players width size dice) ("powerup":kind:cells) =
              Game (foldl (\b cell -> alter (\_ -> Just $ PowerUp $ read kind) cell b) board $ map int cells) players width size dice
    doCommand (Game board players width size dice) ["turns",number] =
              Game newBoard newPlayers width size newDice
                   where (newBoard,newDice,newPlayers) = foldl play (board,dice,players) (take (int number) . repeat 0)

    replaceCell :: Square -> Board -> Int -> Board
    replaceCell square ((_,int,player):rest) destination | int == destination = (square,int,player):rest
    replaceCell square (boardCell:rest) destination = boardCell:(replaceCell square rest destination)

    placeAt :: Int -> Board -> Players -> Player -> Players
    placeAt 0 board player = (board,player)
    placeAt position board player = placeAt newPosition newBoard newPlayer
            where (newBoard,newPosition,newPlayer) = positionPlayer player (board,position)
    positionPlayer :: Player -> (Board, Int) -> (Board, Int, Player)
    positionPlayer player ([(square,int,Nobody)],destination) =
        ([(square,int,player)],0,Winning player)
    positionPlayer player ((square,int,Nobody):rest,destination) | int == destination =
        ((square,int,player):rest,0,Nobody)
    positionPlayer player ((square,int,otherPlayer):rest,destination) | int == destination =
        ((square,int,player):newBoard,newPosition,newPlayer)
            where (newBoard,newPosition,newPlayer) = positionPlayer otherPlayer (rest,destination+1)
    positionPlayer player ((square,int,aPlayer):rest,destination) | player == aPlayer =
        ((square,int,Nobody):newBoard,newPosition,newPlayer)
            where (newBoard,newPosition,newPlayer) = positionPlayer player (rest,destination)
    positionPlayer player (boardCell:rest,destination) =
        (boardCell:newBoard,newPosition,newPlayer)
            where (newBoard,newPosition,newPlayer) = positionPlayer player (rest,destination)

    play :: (Board,Dice,Player) -> Player -> (Board,Dice,Player)
    play result@(_,_,Winning _) _ = result
    play ((square,int,player):rest,dice@(roll:newDice),_) currentPlayer | player == currentPlayer =
         (rest,newDice,player)
    play (cell:rest,dice,_) player = (cell:newBoard,newDice,newPlayer)
         where (newBoard,newDice,newPlayer) = play (rest,dice,Nobody) player

    int :: String -> Int
    int = read
