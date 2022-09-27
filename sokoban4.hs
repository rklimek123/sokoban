{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Text (pack)

{- List Extensions -}
foldList :: (a -> b -> b) -> b -> [a] -> b
foldList _ acc [] = acc
foldList func acc (lh:lt) = foldList func (func lh acc) lt

elemList :: Eq a => a -> [a] -> Bool
elemList el list =
    foldList (\toComp res -> res || el == toComp) False list

reverseList :: [a] -> [a]
reverseList l =
    foldList (:) [] l

appendList :: [a] -> [a] -> [a]
appendList l1 l2 =
    foldList (:) l2 $ reverseList l1

listLength :: [a] -> Integer
listLength l =
    foldList (\_ acc -> acc + 1) 0 l

filterList :: (a -> Bool) -> [a] -> [a]
filterList pred l =
    reverseList $ foldList (\el acc ->
        if pred el then el:acc else acc) [] l

data Pair a b = Pair a b

pairFirst :: Pair a b -> a
pairFirst (Pair a b) = a

pairSecond :: Pair a b -> b
pairSecond (Pair a b) = b

unpackMaybe :: Maybe a -> a
unpackMaybe (Just a) = a

nth :: [a] -> Integer -> a
nth l n =
  (unpackMaybe.pairSecond) $ foldList
        (\el (Pair cnt res) ->
            if cnt == 0 then (Pair (cnt - 1) (Just el)) else (Pair (cnt - 1) res))
        (Pair n Nothing) l

mapList :: (a -> b) -> [a] -> [b]
mapList mapping l =
    reverseList $ foldList (\el acc -> (mapping el):acc) [] l

andList :: [Bool] -> Bool
andList l = foldList (&&) True l

allList :: (a -> Bool) -> [a] -> Bool
allList pred = andList . (mapList pred)

{- Graph logic -}

dfsIfOk :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> [a]
dfsIfOk initialNode neighbours isOk =
    propInner initialNode [initialNode]
    where
        propInner currentNode visited =
            if isOk currentNode then
                foldList(\node acc ->
                    let visited' = appendList acc visited in
                    if elemList node visited'
                        then acc
                        else node:
                            (appendList acc
                                (propInner node (node:visited'))
                            )) [] (neighbours currentNode)
            else []

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk =
    allList isOk (initial:(dfsIfOk initial neighbours isOk))

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours =
    propInner [initial] [initial]
    where
        propInner [] visited = False
        propInner (n:l) visited =
            let visited' = n:visited in
            if n == v
                then True
            else propInner
                (foldList
                    (\neigh acc -> if elemList neigh visited' then acc else neigh:acc)
                    l
                    (neighbours n))
                visited'

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours =
    andList $ mapList (\v -> reachable v initial neighbours) vs

{- Activity -}
data Activity world = Activity {
    actState  :: world,
    actHandle :: (Event -> world -> world),
    actDraw   :: (world -> Picture)
}

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw)
    = Activity state0 handle' draw
      where 
        handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

data MetaState world = StartScreen | Running world deriving Eq

startScreenBase :: Picture
startScreenBase =
    scaled 3 3 (lettering "Sokoban!") &
    scaled 2 2 (
        translated (-2) (-1.5) playerDown &
        translated 0 (-1.5) box &
        translated 2 (-1.5) storage
    )

endScreen :: Integer -> Bool -> Picture
endScreen moveCount hasNextLvl =
    lettering (Data.Text.pack ("Level passed in " ++ show moveCount ++ " moves!")) &
    if hasNextLvl
        then
            scaled 2 2 (
                translated 0 (-1.5) (lettering "[N] - Next level")
            )
        else
            blank

withMenuScreen :: (s -> Bool) -> Activity s -> Activity (MetaState s)
withMenuScreen isEnd (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen

    handle' e             (Running st) = Running (handle e st)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

runActivity :: Activity s -> IO()
runActivity (Activity state handle draw) =
    activityOf state handle draw

genericHolds :: (s -> Bool) -> s -> Bool
genericHolds f el = f el

{-/ WITH UNDO \-}
data WithUndo a = WithUndo a [a]

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s
{-\ WITH UNDO/-}


{- View -}
{-/ Tiles \-}
blockSize = 1
solidSquare n = solidRectangle n n
thickSquare lineWidth n = thickRectangle lineWidth n n

beige :: Color
beige = light brown


wall :: Picture
solidWall = colored gray (solidSquare blockSize)
outlineWall = colored black (thickSquare (blockSize/10) (9/10 * blockSize))
wall = outlineWall & solidWall

ground :: Picture
solidGround = colored (dull $ light pink) (solidSquare blockSize)
outlineGround = colored purple (thickSquare (blockSize/20) (19/20 * blockSize))
ground = outlineGround & solidGround

storage :: Picture
storageMinibox = scaled 0.5 0.5 box
storageCircle = colored (light green) (thickCircle (blockSize/10) ((9/20) * blockSize))
storageIcon = scaled 0.8 0.8 (storageMinibox & storageCircle)
storage = storageIcon & ground

box :: Picture
solidBox = colored beige (solidSquare blockSize)
outlineBox = colored brown (thickSquare (blockSize/10) (9/10 * blockSize))
lineBox = colored brown (thickRectangle (blockSize/10) 0 (9/10 * blockSize))
box = rectangle blockSize blockSize & rotated (pi/2) lineBox & lineBox & outlineBox & solidBox
{-\ Tiles /-}

{-/ Player \-}
faceDist = (blockSize / 8)

kopf =
  thickSquare (blockSize/10) (blockSize / 2) &
  colored (light orange) $ solidSquare (blockSize / 2)

playerBody =
  thickRectangle (blockSize/10) (blockSize / 4) (blockSize / 2) &
  colored blue (solidRectangle (blockSize / 4) (blockSize / 2))

playerDown :: Picture
faceDown =
  translated (-faceDist) faceDist (solidCircle (faceDist/2)) &
  translated faceDist faceDist (solidCircle (faceDist/2)) &
  translated 0 (-faceDist) (solidRectangle (blockSize / 4) (faceDist/2))
playerHeadDown = faceDown & kopf
playerDown =
  translated 0 (blockSize / 4) playerHeadDown &
  translated 0 (-blockSize / 4) playerBody

playerRight :: Picture
faceRight =
  translated faceDist faceDist (solidCircle (faceDist/2)) &
  translated faceDist (-faceDist) (solidRectangle (blockSize / 4) (faceDist/2))
playerHeadRight = faceRight & kopf
playerRight =
  translated 0 (blockSize / 4) playerHeadRight &
  translated 0 (-blockSize / 4) playerBody

playerLeft :: Picture
playerLeft = scaled (-1) 1 playerRight

playerUp :: Picture
playerUp =
  translated 0 (blockSize / 4) kopf &
  translated 0 (-blockSize / 4) playerBody

player :: Direction -> Picture
player U = playerUp
player R = playerRight
player D = playerDown
player L = playerLeft
{-\ Player /-}

{- Model -}
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

drawTileAt :: Tile -> Coord -> Picture
drawTileAt tile (C x y) =
  translated
    ((fromInteger x) * blockSize)
    ((fromInteger y) * blockSize)
    (drawTile tile)


data Coord = C {
  cx :: Integer,
  cy :: Integer
} deriving Eq

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord D (C x y) = C x (y - 1)
adjacentCoord L (C x y) = C (x - 1) y


removeBoxes :: Mapping -> Mapping
removeBoxes maze =
    f . maze
      where
        f t
          | Box == t = Ground
          | otherwise = t


data Direction = U | R | D | L deriving Eq

type Mapping = Coord -> Tile

data State = S {
  stPlayer   :: Coord,
  stDir      :: Direction,
  stBoxes    :: [Coord],
  stStartMap :: Mapping,
  stMoves    :: Integer,
  stCurrentLvl :: Integer,
  stMazes    :: [Maze]
}

{- Not true equality, doesn't compare mappings -}
instance Eq State where
  S p1 d1 b1 _ m1 cl1 _ == S p2 d2 b2 _ m2 cl2 _ =
    p1 == p2 && d1 == d2 && b1 == b2 && m1 == m2 && cl1 == cl2


drawState :: State -> Picture
drawState s@(S (C px py) dir boxes startMap moves currentLvl mazes) =
    if isWinning s then endScreen moves ((listLength mazes) /= 0)
    else
        translated 7 7 (lettering (Data.Text.pack (show moves))) &
        translated (-7) (-7) (lettering (Data.Text.pack ("Level " ++ (show (currentLvl + 1))))) &
        translated (fromInteger px) (fromInteger py) (player dir) &
        foldList (\b acc -> acc & drawTileAt Box b) blank boxes &
        foldList
            (\c acc ->
            acc &
                drawTileAt (startMap c) c
            ) blank (mazeCoordsToDraw (Maze (C px py) startMap))

boxCoords (Maze initial startMap) =
  foldList
    (\c acc -> if Box == startMap c then c:acc else acc)
    []
    (mazeCoordsToDraw (Maze initial startMap))
    

isWinning :: State -> Bool
isWinning s@(S player _ boxes startMap _ _ _) =
    allList (\c -> not (reachable c player graph) || startMap c == Storage) boxes
    where
        graph = getMappingGraph startMap


{- Maze -}
data Maze = Maze Coord Mapping

getMazeMapping :: Maze -> Mapping
getMazeMapping (Maze _ mapping) = mapping

mazeToState :: Maze -> State
mazeToState m@(Maze initial mapping) =
    S initial D initialBoxes (removeBoxes mapping) 0 0 []
    where
        initialBoxes = filterList (\c -> reachable c initial (getMazeGraph m)) (boxCoords m)

getMappingGraph :: Mapping -> (Coord -> [Coord])
getMappingGraph mapping (C x y) =
    filterList (\c -> mapping c /= Wall) [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]

getMappingGraphToDraw :: Mapping -> (Coord -> [Coord])
getMappingGraphToDraw mapping (C x y) =
    filterList (\c -> mapping c /= Blank) [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]

mazeCoordsToDraw :: Maze -> [Coord]
mazeCoordsToDraw (Maze initial startMap) =
    initial:(dfsIfOk initial (getMappingGraphToDraw startMap) (\c -> True))

getMazeGraph :: Maze -> (Coord -> [Coord])
getMazeGraph (Maze _ mapping) = getMappingGraph mapping


isClosed :: Maze -> Bool
isClosed m@(Maze starting mapping) =
    (mapping starting == Ground || mapping starting == Storage) &&
        isGraphClosed starting (getMazeGraph m) (\c -> mapping c /= Blank)
        

isSane :: Maze -> Bool
isSane m@(Maze starting mapping) =
    reachableStoragesCount >= reachableBoxesCount
    where
        reachableFields = dfsIfOk starting (getMazeGraph m) (\c -> mapping c /= Blank)
        reachableStoragesCount =
            listLength $ filterList (\c -> mapping c == Storage) reachableFields
        reachableBoxesCount =
            listLength $ filterList (\c -> mapping c == Box) reachableFields



{- Event controller -}
coordIsBox :: State -> Coord -> Bool
coordIsBox (S _ _ boxes _ _ _ _) c = elemList c boxes

coordIsWall :: State -> Coord -> Bool
coordIsWall (S _ _ _ startMap _ _ _) c = Wall == startMap c

coordIsBlocking :: State -> Coord -> Bool
coordIsBlocking s c = coordIsBox s c || coordIsWall s c

keyInput :: Event -> State -> State
keyInput (KeyPress key) s@(S pl _ _ _ moves currentLvl mazesList)
    | key == "Right" = nextCoord R
    | key == "Up"    = nextCoord U
    | key == "Left"  = nextCoord L
    | key == "Down"  = nextCoord D
    | key == "N"     = nextLevel
  where
    nextLevel =
        if (listLength mazesList) - 1 > currentLvl
            then (mazeToState (nth mazesList (currentLvl + 1))){stCurrentLvl = currentLvl + 1, stMazes = mazesList}
            else s
    nextCoord dir
        | coordIsWall nextS adj = nextS
        | coordIsBox nextS adj =
            if coordIsBlocking nextS (adjacentCoord dir adj) then nextS
            else
              nextS{
                stPlayer = adj,
                stBoxes = map (\b -> if b == adj then adjacentCoord dir adj else b) (getStateBoxes nextS)}
        | otherwise = nextS{stPlayer = adj}
      where
        adj = adjacentCoord dir pl
        nextS = s{stDir = dir, stMoves = moves + 1}
        getStateBoxes (S _ _ bxs _ _ _ _) = bxs
keyInput _ s      = s

{- Level initialization -}
{-/ Mazes \-}

maze1 :: Maze
maze1 = Maze start mapping where
    start = (C 0 1)
    mapping (C x y)
      | abs x > 4  || abs y > 4  = Blank
      | abs x == 4 || abs y == 4 = Wall
      | x ==  2 && y <= 0        = Wall
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0        = Box
      | otherwise                = Ground

maze2 :: Maze
maze2 = Maze start mapping where
    start = (C 0 1)
    mapping (C x y)
      | abs x > 6  || abs y > 6  = Blank
      | abs x == 6 || abs y == 6 = Wall
      | x == 5                   = Storage
      | x == 4                   = Box
      | otherwise                = Ground

maze3 :: Maze
maze3 = Maze start mapping where
    start = (C 0 1)
    mapping (C x y)
      | abs x > 4  || abs y > 4  = Blank
      | abs x == 4 || abs y == 4 = Wall
      | x ==  2 && y <= 0        = Storage
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0        = Box
      | x >= -2 && y == (-2)     = Box
      | otherwise                = Ground

mazes :: [Maze]
mazes = [maze1, maze2, maze3]

-- Brak większości ścian. Gracz w więzieniu
badMaze1 :: Maze
badMaze1 = Maze start mapping where
    start = (C 0 1)
    mapping (C x y)
      | abs x > 4  || abs y > 4  = Blank  -- blank
      | x == (-1) && abs (y - 1) <= 1 = Wall
      | x == 1 && abs (y - 1) <= 1 = Wall
      | y == 2 && abs x <= 1 = Wall
      | y == 0 && abs x <= 1 = Wall
      | x ==  2 && y <= 0        = Wall  -- wall
      | x ==  3 && y <= 0        = Storage  -- storage
      | x >= -2 && y == 0        = Box  -- box
      | otherwise                = Ground  -- ground

-- Za dużo boxów.
badMaze2 :: Maze
badMaze2 = Maze start mapping where
    start = (C 0 1)
    mapping (C x y)
      | abs x > 4  || abs y > 4  = Blank  -- blank
      | abs x == 4 || abs y == 4 = Wall  -- wall
      | x ==  3 && y <= 0        = Storage  -- storage
      | x >= -2 && y == 0        = Box  -- box
      | otherwise                = Ground  -- ground

-- Za dużo boxów.
badMaze3 :: Maze
badMaze3 = Maze start mapping where
    start = (C 0 1)
    mapping (C x y)
      | abs x > 4  || abs y > 4  = Blank  -- blank
      | abs x == 4 || abs y == 4 = Wall  -- wall
      | x ==  2 && y <= 0        = Wall  -- wall
      | x ==  3 && y <= (-1)     = Storage  -- storage
      | x >= -2 && y == 0        = Box  -- box
      | otherwise                = Ground  -- ground

-- Brak większości ścian.
badMaze4 :: Maze
badMaze4 = Maze start mapping where
    start = (C 0 1)
    mapping (C x y)
      | abs x > 4  || abs y > 4  = Blank  -- blank
      | x ==  2 && y <= 0        = Wall  -- wall
      | x ==  3 && y <= 0        = Storage  -- storage
      | x >= -2 && y == 0        = Box  -- box
      | otherwise                = Ground  -- ground

badMazes :: [Maze]
badMazes = [badMaze1, badMaze2, badMaze3, badMaze4]
{-\ Mazes /-}

{-/ Mazes info \-}
pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (0.4 -fromIntegral n / 2) 0 (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i))
                     0
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

etap4 =
    translated 0 6.5 startScreenBase &
    translated (-3) 1 (lettering "Good closed levels") &
    translated (-3) 0 (pictureOfBools (mapList isClosed mazes)) &
    translated 3 (-1) (lettering "Bad closed levels") &
    translated 3 (-2) (pictureOfBools (mapList isClosed badMazes)) &
    translated (-3) (-3) (lettering "Good sane levels") &
    translated (-3) (-4) (pictureOfBools (mapList isSane mazes)) &
    translated 3 (-5) (lettering "Bad sane levels") &
    translated 3 (-6) (pictureOfBools (mapList isSane badMazes))
startScreen = etap4
{-\ Mazes info /-}

startState = (mazeToState (nth mazes 0)){stCurrentLvl = 0, stMazes = mazes}

etap5 :: IO()
etap5 = (runActivity.withUndo.(withMenuScreen isWinning).resettable) (Activity startState keyInput drawState)

main :: IO()
main = etap5

{- Extras -}
addBoxes :: [Coord] -> Mapping -> Mapping
addBoxes boxes maze coord
    | elemList coord boxes = Box
    | otherwise = maze coord