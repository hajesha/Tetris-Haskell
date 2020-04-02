
module Assign3 where
    import System.IO
    import Data.Map (empty,adjust,alter,foldlWithKey,elems,fromSet,Map)
    import Data.Set (empty,fromList,member,delete,insert,Set)
    import Data.List (takeWhile)
    import Data.Char (digitToInt)
    import Text.Printf (printf)
    import Test.HUnit
    main = do
      input <- getContents
      putStr $ show $ readFrom input

    type Status = Int                           --if game is on
    type Board = (Int,Int,Int,[String], [[Int]])            -- Row Col Middle listOfPowerUps
    type Dice = String                    --dices index
    type Block = (String,Int,[[Int]])           --type orientation points
    data Game = Game Board Block Dice Status Int

    emptyBoard = Game (1,1,1,[" "],[[]]) ("nil",1,(cycle[[1]])) (cycle['1']) 1 0
    doLine game = doCommand game . words
    readFrom = foldl doLine emptyBoard . lines


    boardCreator :: Int -> Int -> [String]
    boardCreator x y =
        (replicate y $ "|" ++ replicate x ' ' ++ "|") ++ ( ["+" ++ replicate x '-' ++ "+"])

    doCommand :: Game -> [String] -> Game
    doCommand (Game board block dice 0 count) (_) =
        Game board block dice 0 count                               --if the game is done
    doCommand (Game _ block dice 1 count) ["board",cols, rows] =
        Game (int rows,int cols,(middle (int cols)),(boardCreator (int cols) (int rows)),[[]]) block dice 1 count
    doCommand (Game board block _ 1 count) ("dice":sequence) =
        Game board block (cycle (eliminateWhite sequence)) 1 count
    doCommand (Game board block dice 1 count) ("moves":sequence) =
        movesCommand board block dice sequence 1 count

    doCommand (Game (r,c,m,board, [[]]) block dice 1 count) ["powerup",cols, rows] =
        (Game (r,c,m,board,([[int cols-1, (r - int rows)]])) block dice 1 count)

    doCommand (Game (r,c,m,board, pu) block dice 1 count) ["powerup",cols, rows] =
        (Game (r,c,m,board,( pu ++ [[int cols-1,(r - int rows)]])) block dice 1 count)

    middle :: Int -> Int
    middle x
          | remain == 1 = ((quot x 2))
          | remain == 0 = ((quot x 2)-1)
          where remain = mod x 2

    createNewBlock :: String -> Int -> ((String,Int,[[Int]]),Dice)
    createNewBlock dice m
          | shape == '1' = initializeShapeY (tail dice) m
          | shape == '2' = initializeShapeR (tail dice) m
          | shape == '3' = initializeShapeG (tail dice) m
          | shape == '4' = initializeShapeB (tail dice) m
          | shape == '5' = initializeShapeO (tail dice) m
          | shape == '6' = initializeShapeP (tail dice) m
          | shape == '7' = initializeShapeC (tail dice) m
          where shape = head dice

    int :: String -> Int
    int = read
    intC :: Char -> Int
    intC = digitToInt

    eliminateSpace :: [String] -> String
    eliminateSpace string =
        filter (/= '\n') (unlines string)

    eliminateWhite ::  [String] -> String
    eliminateWhite string =
        filter (/= '\n') (filter (/= ' ') (unlines string))

    movesCommand :: Board -> Block -> Dice -> [String] -> Int -> Int -> Game
    movesCommand (r,c,m,string,pu) ("nil",orient,points) dice moves 1 count -- method will create a new block and then send the new block to the move method
        |(canMove string points1 == True) = movesCommand (r,c,m,string,pu) (shape1,orient1,points1) newDice moves 1 (count+1)
        |otherwise = Game (r,c,m,string,pu) (shape1,orient1,points1) newDice 0 count
        where ((shape1,orient1,points1),newDice) = createNewBlock dice m

    movesCommand (r,c,m,string,pu) block dice moves 1 count     --method will do the moves on the piece
        |newMoves == [] =(Game (r,c,m,string,pu) block dice 1 count)
        |(head newMoves) == '+' = doCommand (Game newBoard ("nil",1,(cycle[[1]])) dice1 status count) ("moves":[afterMoves])
        |otherwise = doCommand (Game board1 block1 dice1 status count) ("moves":[afterMoves])
        where newMoves = eliminateSpace moves
              ((Game board1 block1 dice1 status count1),afterMoves) = doActions (r,c,m,string,pu) block dice newMoves 1 count
              newBoard = placeOnBoard board1 block1 dice1 status

    doActions :: Board -> Block -> Dice -> String -> Int -> Int -> (Game,String)
    doActions (r,c,m,string,pu) (shape,orient,points) dice [] 1 count  =
        ((Game (r,c,m,string,pu) (shape,orient,points) dice 1 count),[])
    doActions (r,c,m,string,pu) (shape,orient,points) dice moves 1 count
          | (any (\[x,y] -> y < 0) tempPoint ) = ((Game (r,c,m,string,pu) (shape,orient,points) dice 1 count),(tail moves))
          | canMove string tempPoint == True = ((Game (r,c,m,string,pu) (shape,orient,tempPoint) dice 1 count),(tail moves))
          | ((canMove string tempPoint == False) && ((head moves) == '.')) = ((Game newBoard ("nil",1,(cycle[[1]])) dice 1 count),(tail moves))
          | otherwise = ((Game (r,c,m,string,pu) (shape,orient,points) dice 1 count),(tail moves))
          where tempPoint = move (head moves) points shape string
                newBoard = placeOnBoard (r,c,m,string,pu) (shape,orient,points) dice 1

    canMove :: [String] -> [[Int]] -> Bool
    canMove board newPiecePoints=
        all (\p -> p == ' ' || p =='x') ((map (\[x,y] -> board !! y !! (x+1))) newPiecePoints)

    add :: Int -> Int -> Int
    add x y = x+y

    move :: Char -> [[Int]] -> String -> [String]-> [[Int]]
    move mv points shape board
          |mv == 'r' = map (\[x,y] -> [x+1,y]) points
          |mv == 'l' = map (\[x,y] -> [x-1,y]) points
          |mv == '.' = map (\[x,y] -> [x,y+1]) points
          |mv == '+' = dropMove points board
          |(mv == 'R') &&  (shape == "y") = points
          |(mv == 'R') &&  (shape /= "y") = map (\[x,y] -> [(y-pY) + pX+1 , -(x-pX)+pY]) points
          |(mv == 'L') &&  (shape == "y") = points
          |(mv == 'L') &&  (shape /= "y") = map (\[x,y] -> [-(y-pY) + pX+1 , (x-pX)+pY]) points
          |otherwise = points
          where [pX,pY] = points !! 0

    dropMove :: [[Int]] -> [String] -> [[Int]]
    dropMove points board
          | canMove board newPoints == False = points
          | canMove board newPoints == True = dropMove newPoints board
          where newPoints = map (\[x,y] -> [x,y+1]) points

    goThruPoints :: [String] -> String -> [[Int]] -> [String]
    goThruPoints board shape [] = board
    goThruPoints board shape points =
        goThruPoints newBoard shape (tail points)
        where newBoard = pointOnboard board shape (head points)

    pointOnboard :: [String] -> String -> [Int] -> [String]
    pointOnboard board shape pair =
        replaceLine board line (last pair)
        where line = replacePoint board shape (head pair) (last pair)

    replacePoint :: [String] -> String-> Int -> Int -> String
    replacePoint board shape x y =
        front ++ shape ++ (tail end)
        where (front, end) = splitAt (x+1) (board !! y)

    replaceLine :: [String] -> String-> Int -> [String]
    replaceLine board line y =
        front ++ [line] ++ (tail end)
        where (front, end) = splitAt y board


    initializeShapeY :: String -> Int -> ((String,Int,[[Int]]),Dice)
    initializeShapeY dice x
        | orient == 1 = (("y",1,[[x,0], [x,1], [x+1,0], [x+1,1]]),(tail dice))
        | orient == 2 = (("y",2,[[x,0], [x,1], [x+1,0], [x+1,1]]),(tail dice))
        | orient == 3 = (("y",3,[[x,0], [x,1], [x+1,0], [x+1,1]]),(tail dice))
        | orient == 4 = (("y",4,[[x,0], [x,1], [x+1,0], [x+1,1]]),(tail dice))
        where orient = (mod ((intC $ head dice)-1) 4)+1
    initializeShapeR :: String -> Int -> ((String,Int,[[Int]]),Dice)
    initializeShapeR dice x
        | orient == 1 = (("r",1,[[x,1], [x,0], [x-1,0], [x+1,1]]),(tail dice))
        | orient == 2 = (("r",2,[[x,1], [x+1,0], [x+1,1], [x,2]]),(tail dice))
        | orient == 3 = (("r",3,[[x,0], [x-1,0], [x,1], [x+1,1]]),(tail dice))
        | orient == 4 = (("r",4,[[x,1], [x+1,1], [x,2], [x+1,0]]),(tail dice))
        where orient = (mod ((intC $ head dice)-1) 4)+1
    initializeShapeG :: String -> Int -> ((String,Int,[[Int]]),Dice)
    initializeShapeG dice x
        | orient == 1 = (("g",1,[[x,1], [x,0], [x+1,0], [x-1,1]]),(tail dice))
        | orient == 2 = (("g",2,[[x,1], [x,0], [x+1,2], [x+1,1]]),(tail dice))
        | orient == 3 = (("g",3,[[x,0], [x+1,0], [x,1], [x-1,1]]),(tail dice))
        | orient == 4 = (("g",4,[[x,1], [x-1,0], [x-1,1], [x,1]]),(tail dice))
        where orient = (mod ((intC $ head dice)-1) 4)+1
    initializeShapeB :: String -> Int -> ((String,Int,[[Int]]),Dice)
    initializeShapeB dice x
        | orient == 1 = (("b",1,[[x,1], [x-1,1], [x+1,1], [x-1,0]]),(tail dice))
        | orient == 2 = (("b",2,[[x,1], [x,0], [x+1,0], [x,2]]),(tail dice))
        | orient == 3 = (("b",3,[[x,0], [x+1,0], [x-1,0], [x+1,1]]),(tail dice))
        | orient == 4 = (("b",4,[[x,1], [x,0], [x,2], [x-1,2]]),(tail dice))
        where orient = (mod ((intC $ head dice)-1) 4)+1
    initializeShapeO :: String -> Int -> ((String,Int,[[Int]]),Dice)
    initializeShapeO dice x
        | orient == 1 = (("o",1,[[x,1], [x-1,1], [x+1,1], [x+1,0]]),(tail dice))
        | orient == 2 = (("o",2,[[x,1], [x,2], [x+1,2], [x,0]]),(tail dice))
        | orient == 3 = (("o",3,[[x,0], [x-1,0], [x+1,0], [x-1,1]]),(tail dice))
        | orient == 4 = (("o",4,[[x,1], [x,2], [x,0], [x-1,0]]),(tail dice))
        where orient = (mod ((intC $ head dice)-1) 4)+1
    initializeShapeP :: String -> Int -> ((String,Int,[[Int]]),Dice)
    initializeShapeP dice x
        | orient == 1 = (("p",1,[[x,1], [x+1,1], [x-1,1], [x,0]]),(tail dice))
        | orient == 2 = (("p",2,[[x,1], [x,0], [x,2], [x+1,1]]),(tail dice))
        | orient == 3 = (("p",3,[[x,0], [x-1,0], [x+1,0], [x,1]]),(tail dice))
        | orient == 4 = (("p",4,[[x,1], [x,0], [x,2], [x-1,1]]),(tail dice))
        where orient = (mod ((intC $ head dice)-1) 4)+1
    initializeShapeC :: String -> Int -> ((String,Int,[[Int]]),Dice)
    initializeShapeC dice x
        | orient == 1 = (("c",1,[[x,0], [x-1,0], [x+1,0], [x+2,0]]),(tail dice))
        | orient == 2 = (("c",2,[[x,1], [x,0], [x,2], [x,3]]),(tail dice))
        | orient == 3 = (("c",3,[[x,0], [x-1,0], [x-2,0], [x+1,0]]),(tail dice))
        | orient == 4 = (("c",4,[[x,2], [x,1], [x,0], [x,3]]),(tail dice))
        where orient = (mod ((intC $ head dice)-1) 4)+1


    checkForFullRow :: Board -> Board
    checkForFullRow (r,c,m,board,pu)
      | (anyFullRow (r,c,m,board,pu) == True) = checkForFullRow (r,c,m,(findIndex board c r),pu)
      | otherwise = (r,c,m,board,pu)

    anyFullRow :: Board -> Bool
    anyFullRow (r,c,m,board,pu) =
        any (== True) (map (\x -> lineCheck x) (init board))

    lineCheck :: String -> Bool
    lineCheck line =
        all (== True) ((map (\x -> (/= ' ') x) line))

    findIndex :: [String] -> Int -> Int -> [String]
    findIndex board cols rows
        |(newBoard == board) =  lineShifter (rows-1) newBoard cols
        |otherwise = newBoard
        where newBoard = lineShifter rows board cols

    lineShifter :: Int -> [String]-> Int -> [String]
    lineShifter index board cols
        | (lineCheck (board !! index) == True) = shiftyShifter board index cols
        |otherwise = board

    shiftyShifter :: [String] -> Int -> Int -> [String]
    shiftyShifter board index cols=
      [("|" ++ replicate cols ' ' ++ "|")] ++ (init front) ++ end
        where (front, end) = splitAt index board

    pieceCounter :: Int -> String
    pieceCounter count
        | count == 1 = " 1 piece"
        | otherwise  = " " ++ (show count) ++ " pieces"

    counterAppender :: [String] -> Int -> [String]
    counterAppender board count =
        [head board ++ (pieceCounter count)] ++ (tail board)

-----------------------------------------------------------------------------------------
----powerup doesnt work but here are the methods i had in progress (essentially the powerups are just aesthetic)
    anyPowerupLines :: Board -> Bool
    anyPowerupLines (r,c,m,board,pu) =
        any (== True) (map (\x -> powerupCheck x c) (init board))

    powerupCheck :: String -> Int -> Bool
    powerupCheck line cols
        |(length (filter (/= ' ') line) == (cols - 3)) = True
        |otherwise = False

    findPowerupIndex :: [String] -> Int -> Int -> [String]
    findPowerupIndex board cols rows
        |(newBoard == board) =  lineShifterPowerup (rows-1) newBoard cols
        |otherwise = newBoard
        where newBoard = lineShifterPowerup rows board cols

    lineShifterPowerup :: Int -> [String]-> Int -> [String]
    lineShifterPowerup index board cols
        | ((powerupCheck (board !! index) cols) == True) = shiftyShifter board index cols
        |otherwise = board

    checkPowerup :: Board -> Block -> Bool
    checkPowerup (r,c,m,board,[[]]) (shape,orient,points) =
        False
    checkPowerup (r,c,m,board,pu) (shape,orient,points) =
        any (/= ' ') (map (\[x,y] -> board !! y !!(x+1)) pu)

    placeOnBoard :: Board -> Block -> Dice -> Int -> Board
    placeOnBoard (r,c,m,board,pu) (shape,orient,points) dice 1
        | (((checkPowerup newBoard2 (shape,orient,points)) == True) && (anyPowerupLines newBoard2 == True)) =newBoard2     ---activate the checking for one less
        | otherwise = newBoard2
        where newBoard = (r,c,m,(goThruPoints board shape points),pu)
              newBoard2 = checkForFullRow newBoard

    placeOnBoard (r,c,m,board,pu) (shape,orient,points) dice 0 =
        (r,c,m,board,pu)

    placePowerup :: Board -> [String]
    placePowerup (r,c,m,board,l) =
        goThruPoints board "x" l
-----------------------------------------------------------------------------------------
    instance Show Game where
        show (Game (r,c,m,board,l) (shape, orient, points) dice status count)
          |((shape == "nil") && (l /= [[]])) = unlines  (counterAppender powerupBoard count)
          |shape == "nil"                    = unlines  (counterAppender board count)
          |(l == [[]])                       = unlines (counterAppender newBoard count)
          |otherwise                         = unlines (counterAppender newBoard2 count)
          where (r1,c1,m1,newBoard,l1)  = placeOnBoard (r,c,m,board,l) (shape, orient, points) dice status
                powerupBoard = placePowerup (r,c,m,board,l)
                (r2,c2,m2,newBoard2,l2) =  placeOnBoard (r,c,m,powerupBoard,l) (shape, orient, points) dice status


  -----------------------------------------------------------------------------------------
  --these are the tests
    tests = test [
      --  testSimpleEmpty
            "tiny empty board" ~:
                       "|       | 0 pieces\n\
                       \|       |\n\
                       \|       |\n\
                       \|       |\n\
                       \+-------+\n" ~=?
                     (show $ readFrom "board 7 4")

         	,"larger empty board" ~:
                       "|        | 0 pieces\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 6")

         	,"odd-size board with powerups" ~:
                       "|       | 0 pieces\n\
                       \|  x    |\n\
                       \|       |\n\
                       \| x     |\n\
                       \+-------+\n\
                       \" ~=?
                     (show $ readFrom "board 7 4\n\
                                      \powerup 3 3\n\
                                      \powerup 2 1")

         --  testSimpleEvenWidth
         	,"podium" ~:
                       "|   p    | 1 piece\n\
                       \|  ppp   |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 6\n\
                                      \dice 6 1\n\
                                      \moves")

         	,"podium rotated" ~:
                       "|   p    | 1 piece\n\
                       \|   pp   |\n\
                       \|   p    |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 6\n\
                                      \dice 6 2\n\
                                      \moves")

         	,"green piece" ~:
                       "|   gg   | 1 piece\n\
                       \|  gg    |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 6\n\
                                      \dice 3 1\n\
                                      \moves")

         	,"green rotated" ~:
                       "|   g    | 1 piece\n\
                       \|   gg   |\n\
                       \|    g   |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 6\n\
                                      \dice 3 2\n\
                                      \moves ")

                  ,"upside-w" ~:
                        "|   gg   | 1 piece\n\
                        \|  gg    |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                      \dice 3 3\n\
                                      \moves R")

         	,"green can rotate" ~:
                       "|   g    | 1 piece\n\
                       \|   gg   |\n\
                       \|    g   |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 6\n\
                                      \dice 3 1\n\
                                      \moves R")

         	,"2 pieces" ~:
                       "|   c    | 2 pieces\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|   yy   |\n\
                       \|   yy   |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 10\n\
                                      \dice 1 2 7 2 2\n\
                                      \moves +")

         	,"cyan unrotated" ~:
                       "|  cccc  | 2 pieces\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|   yy   |\n\
                       \|   yy   |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 10\n\
                                      \dice 1 1 7 1 2\n\
                                      \moves +")

         	,"3 pieces" ~:
                       "|  rr    | 3 pieces\n\
                       \|   rr   |\n\
                       \|        |\n\
                       \|        |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   yy   |\n\
                       \|   yy   |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 10\n\
                                      \dice 1 2 7 2 2\n\
                                      \moves ++")

         	,"2 pieces with powerup" ~:
                       "|        | 2 pieces\n\
                       \| x      |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|        |\n\
                       \|        |\n\
                       \|yy      |\n\
                       \|yy      |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 10\n\
                                      \powerup 2 9\n\
                                      \dice 1 2 7 2 2\n\
                                      \moves ll ll\n\
                                      \moves +..")

         	,"2 pieces with powerup hidden" ~:
                       "|        | 2 pieces\n\
                       \| c      |\n\
                       \| c      |\n\
                       \| c      |\n\
                       \| c      |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|yy      |\n\
                       \|yy      |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 10\n\
                                      \powerup 2 9\n\
                                      \dice 1 2 7 2 2\n\
                                      \moves ll ll\n\
                                      \moves +ll.")


         --  testSimpleOddWidth
         	,"odd green" ~:
                       "|   gg  | 1 piece\n\
                       \|  gg   |\n\
                       \|       |\n\
                       \|       |\n\
                       \|       |\n\
                       \|       |\n\
                       \+-------+\n\
                       \" ~=?
                     (show $ readFrom "board 7 6\n\
                                      \dice 3 1\n\
                                      \moves")

         	,"odd green rotated" ~:
                       "|   g   | 1 piece\n\
                       \|   gg  |\n\
                       \|    g  |\n\
                       \|       |\n\
                       \|       |\n\
                       \|       |\n\
                       \+-------+\n\
                       \" ~=?
                     (show $ readFrom "board 7 6\n\
                                      \dice 3 2\n\
                                      \moves ")

         	,"odd 2 pieces" ~:
                       "|   c   | 2 pieces\n\
                       \|   c   |\n\
                       \|   c   |\n\
                       \|   c   |\n\
                       \|       |\n\
                       \|       |\n\
                       \|       |\n\
                       \|       |\n\
                       \|   yy  |\n\
                       \|   yy  |\n\
                       \+-------+\n\
                       \" ~=?
                     (show $ readFrom "board 7 10\n\
                                      \dice 1 2 7 2 2\n\
                                      \moves +")

         	,"odd 2 cyan horizontal" ~:
                       "|  cccc | 2 pieces\n\
                       \|       |\n\
                       \|       |\n\
                       \|       |\n\
                       \|       |\n\
                       \|       |\n\
                       \|       |\n\
                       \|       |\n\
                       \|   yy  |\n\
                       \|   yy  |\n\
                       \+-------+\n\
                       \" ~=?
                     (show $ readFrom "board 7 10\n\
                                      \dice 1 1 7 1 2\n\
                                      \moves +")

         	,"odd 3 pieces" ~:
                       "|  rr   | 3 pieces\n\
                       \|   rr  |\n\
                       \|       |\n\
                       \|       |\n\
                       \|   c   |\n\
                       \|   c   |\n\
                       \|   c   |\n\
                       \|   c   |\n\
                       \|   yy  |\n\
                       \|   yy  |\n\
                       \+-------+\n\
                       \" ~=?
                     (show $ readFrom "board 7 10\n\
                                      \dice 1 2 7 2 2\n\
                                      \moves ++")


         --  testMultipiece
         	,"4 pieces" ~:
                       "|   gg  | 4 pieces\n\
                       \|  gg   |\n\
                       \|   c   |\n\
                       \|   c   |\n\
                       \|   c   |\n\
                       \|   c   |\n\
                       \|  rr   |\n\
                       \|   rr  |\n\
                       \|   gg  |\n\
                       \|  gg   |\n\
                       \+-------+\n\
                       \" ~=?
                     (show $ readFrom "board 7 10\n\
                                      \dice 3 1 2 1 7 2\n\
                                      \moves +++++++++++")

         	,"20 pieces" ~:
                       "|        | 20 pieces\n\
                       \|        |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 10\n\
                                      \dice 7 2\n\
                                      \moves r+ rr+ rrr+ rrrr+ l+ ll+ lll+ Rrrrrr+ rrrr+ rrrr+ rrrr+ rrrr+ + Rllll+ lll+ ll+ l+  +++++++")

         	,"14 pieces" ~:
                       "|   c    | 14 pieces\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|  xc    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|   c    |\n\
                       \|cccc    |\n\
                       \|cccc    |\n\
                       \|cccc    |\n\
                       \+--------+\n\
                       \" ~=?
                     (show $ readFrom "board 8 10\n\
                                      \dice 7 2\n\
                                      \powerup 3 7\n\
                                      \moves r+ rr+ rrr+ rrrr+ l+ ll+ lll+ l....r +Rrrrr+lll+ll+l++++")


          --  testRotates
          	,"yellow rotated1" ~:
                        "|   yy   | 1 piece\n\
                        \|   yy   |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 1 1\n\
                                       \moves ")

          	,"yellow rotated2" ~:
                        "|   yy   | 1 piece\n\
                        \|   yy   |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 1 2\n\
                                       \moves ")

          	,"yellow rotated3" ~:
                        "|   yy   | 1 piece\n\
                        \|   yy   |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 1 3\n\
                                       \moves ")

          	,"yellow rotated4" ~:
                        "|   yy   | 1 piece\n\
                        \|   yy   |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 1 4\n\
                                       \moves ")

          	,"yellow rotated5" ~:
                        "|   yy   | 1 piece\n\
                        \|   yy   |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 1 5\n\
                                       \moves ")
          	,"red rotated1" ~:
                        "|  rr    | 1 piece\n\
                        \|   rr   |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 2 1\n\
                                       \moves ")

          	,"red rotated2" ~:
                        "|    r   | 1 piece\n\
                        \|   rr   |\n\
                        \|   r    |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 2 2\n\
                                       \moves ")

          	,"red rotated3" ~:
                        "|  rr    | 1 piece\n\
                        \|   rr   |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 2 3\n\
                                       \moves ")

            ,"red rotated4" ~:
                        "|    r   | 1 piece\n\
                        \|   rr   |\n\
                        \|   r    |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 2 4\n\
                                       \moves ")

          	,"blue rotated1" ~:
                        "|  b     | 1 piece\n\
                        \|  bbb   |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 4 1\n\
                                       \moves ")
          	,"blue rotated2" ~:
                        "|   bb   | 1 piece\n\
                        \|   b    |\n\
                        \|   b    |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 4 2\n\
                                       \moves ")

          	,"blue rotated3" ~:
                        "|  bbb   | 1 piece\n\
                        \|    b   |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 4 3\n\
                                       \moves ")

          	,"blue rotated4" ~:
                        "|   b    | 1 piece\n\
                        \|   b    |\n\
                        \|  bb    |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 4 4\n\
                                       \moves ")

          	,"orange rotated1" ~:
                        "|    o   | 1 piece\n\
                        \|  ooo   |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 5 1\n\
                                       \moves ")

          	,"orange rotated2" ~:
                        "|   o    | 1 piece\n\
                        \|   o    |\n\
                        \|   oo   |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                      (show $ readFrom "board 8 6\n\
                                       \dice 5 2\n\
                                       \moves ")

        	,"orange rotated3" ~:
                      "|  ooo   | 1 piece\n\
                      \|  o     |\n\
                      \|        |\n\
                      \|        |\n\
                      \|        |\n\
                      \|        |\n\
                      \+--------+\n\
                      \" ~=?
                    (show $ readFrom "board 8 6\n\
                                     \dice 5 3\n\
                                     \moves ")

        	,"orange rotated4" ~:
                      "|  oo    | 1 piece\n\
                      \|   o    |\n\
                      \|   o    |\n\
                      \|        |\n\
                      \|        |\n\
                      \|        |\n\
                      \+--------+\n\
                      \" ~=?
                    (show $ readFrom "board 8 6\n\
                                     \dice 5 4\n\
                                     \moves ")

           ,"pink rotated1" ~:
                       "|   p    | 1 piece\n\
                       \|  ppp   |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \|        |\n\
                       \+--------+\n\
                       \" ~=?
                      (show $ readFrom "board 8 6\n\
                                        \dice 6 1\n\
                                        \moves ")


            ,"pink rotated2" ~:
                        "|   p    | 1 piece\n\
                        \|   pp   |\n\
                        \|   p    |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                       (show $ readFrom "board 8 6\n\
                                         \dice 6 2\n\
                                         \moves ")

            ,"pink rotated3" ~:
                        "|  ppp   | 1 piece\n\
                        \|   p    |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \|        |\n\
                        \+--------+\n\
                        \" ~=?
                       (show $ readFrom "board 8 6\n\
                                         \dice 6 3\n\
                                         \moves ")

              ,"pink rotated4" ~:
                          "|   p    | 1 piece\n\
                          \|  pp    |\n\
                          \|   p    |\n\
                          \|        |\n\
                          \|        |\n\
                          \|        |\n\
                          \+--------+\n\
                          \" ~=?
                         (show $ readFrom "board 8 6\n\
                                           \dice 6 4\n\
                                           \moves ")
              ,"cyan rotated1" ~:
                          "|  cccc  | 1 piece\n\
                          \|        |\n\
                          \|        |\n\
                          \|        |\n\
                          \|        |\n\
                          \|        |\n\
                          \+--------+\n\
                          \" ~=?
                         (show $ readFrom "board 8 6\n\
                                           \dice 7 1\n\
                                           \moves ")

               ,"cyan rotated2" ~:
                           "|   c    | 1 piece\n\
                           \|   c    |\n\
                           \|   c    |\n\
                           \|   c    |\n\
                           \|        |\n\
                           \|        |\n\
                           \+--------+\n\
                           \" ~=?
                          (show $ readFrom "board 8 6\n\
                                            \dice 7 2\n\
                                            \moves ")

                ,"cyan rotated3" ~:
                            "| cccc   | 1 piece\n\
                            \|        |\n\
                            \|        |\n\
                            \|        |\n\
                            \|        |\n\
                            \|        |\n\
                            \+--------+\n\
                            \" ~=?
                           (show $ readFrom "board 8 6\n\
                                             \dice 7 3\n\
                                             \moves ")

                  ,"cyan rotated4" ~:
                              "|   c    | 1 piece\n\
                              \|   c    |\n\
                              \|   c    |\n\
                              \|   c    |\n\
                              \|        |\n\
                              \|        |\n\
                              \+--------+\n\
                              \" ~=?
                             (show $ readFrom "board 8 6\n\
                                               \dice 7 4\n\
                                               \moves ")
                  --testing the moves
                  ,"move l" ~:
                              "|  c     | 1 piece\n\
                              \|  c     |\n\
                              \|  c     |\n\
                              \|  c     |\n\
                              \|        |\n\
                              \|        |\n\
                              \+--------+\n\
                              \" ~=?
                             (show $ readFrom "board 8 6\n\
                                               \dice 7 4\n\
                                               \moves l")

                  ,"move r" ~:
                              "|    c   | 1 piece\n\
                              \|    c   |\n\
                              \|    c   |\n\
                              \|    c   |\n\
                              \|        |\n\
                              \|        |\n\
                              \+--------+\n\
                              \" ~=?
                             (show $ readFrom "board 8 6\n\
                                               \dice 7 4\n\
                                               \moves r")

                ,"move ." ~:
                            "|        | 1 piece\n\
                            \|   c    |\n\
                            \|   c    |\n\
                            \|   c    |\n\
                            \|   c    |\n\
                            \|        |\n\
                            \+--------+\n\
                            \" ~=?
                           (show $ readFrom "board 8 6\n\
                                             \dice 7 4\n\
                                             \moves .")

                ,"move +" ~:
                            "|        | 1 piece\n\
                            \|        |\n\
                            \|   c    |\n\
                            \|   c    |\n\
                            \|   c    |\n\
                            \|   c    |\n\
                            \+--------+\n\
                            \" ~=?
                           (show $ readFrom "board 8 6\n\
                                             \dice 7 4\n\
                                             \moves +")

                ,"move to the wall" ~:
                            "|c       | 1 piece\n\
                            \|c       |\n\
                            \|c       |\n\
                            \|c       |\n\
                            \|        |\n\
                            \|        |\n\
                            \+--------+\n\
                            \" ~=?
                           (show $ readFrom "board 8 6\n\
                                             \dice 7 4\n\
                                             \moves lllllllllllllllllllllllllllllll")

                ,"move to the other wall" ~:
                            "|       c| 1 piece\n\
                            \|       c|\n\
                            \|       c|\n\
                            \|       c|\n\
                            \|        |\n\
                            \|        |\n\
                            \+--------+\n\
                            \" ~=?
                           (show $ readFrom "board 8 6\n\
                                             \dice 7 4\n\
                                             \moves rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr")


                ,"failed rotate" ~:
                            "|  cccc  | 1 piece\n\
                            \|        |\n\
                            \|        |\n\
                            \|        |\n\
                            \|        |\n\
                            \|        |\n\
                            \+--------+\n\
                            \" ~=?
                           (show $ readFrom "board 8 6\n\
                                             \dice 7 1\n\
                                             \moves R")
                  ,"succesful rotate" ~:
                              "|        | 1 piece\n\
                              \|        |\n\
                              \|  cccc  |\n\
                              \|        |\n\
                              \|        |\n\
                              \|        |\n\
                              \+--------+\n\
                              \" ~=?
                             (show $ readFrom "board 8 6\n\
                                               \dice 7 4\n\
                                               \moves R")


                   ,"clear all rows" ~:
                              "|   yy   | 5 pieces\n\
                              \|   yy   |\n\
                              \|        |\n\
                              \|        |\n\
                              \+--------+\n" ~=?
                            (show $ readFrom "board 8 4\ndice 1\nmoves lll+l+r+rrr+")


                  ,"failed move" ~:
                            "|        | 3 pieces\n\
                            \| yy     |\n\
                            \| yyyy   |\n\
                            \|   yy   |\n\
                            \|   yy   |\n\
                            \|   yy   |\n\
                            \+--------+\n" ~=?
                            (show $ readFrom "board 8 6\n dice 1\nmoves ++ll.r")

                ,"frozen ." ~:
                            "|   yy  | 5 pieces\n\
                            \|yy yy  |\n\
                            \|yy     |\n\
                            \|yyyyyy |\n\
                            \|yyyyyy |\n\
                            \+-------+\n" ~=?
                            (show $ readFrom "board 7 5\ndice 1\nmoves lll+l+r+lll..")

                ---testing powerup
                ,"simple powerup ." ~:
                            "|       | 4 pieces\n\
                            \|  yy   |\n\
                            \|  yy   |\n\
                            \|       |\n\
                            \|yyyyyy |\n\
                            \+-------+\n" ~=?
                            (show $ readFrom "board 7 5\ndice 1\nmoves lll+l+r+..")






                  ]
