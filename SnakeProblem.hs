import System.IO
import Data.List -- for intersperse
import Control.Concurrent -- for threadDelay
import Data.Maybe

type GameState = Maybe (GameMap, Maybe Direction)

data Direction = N | S | E | W deriving (Eq, Show)

data Cell = Empty | Body | Snak | Wall | Head deriving Eq
type GameMap = [[Cell]]

renderCell :: Cell -> Char
renderCell Empty  = ' '
renderCell Body   = 'o'
renderCell Snak   = 'x'
renderCell Wall   = '#'
renderCell Head   = '@'

renderMap :: GameMap -> String
renderMap m = concat $ map (\x -> x ++ "\n") (map (map renderCell) m)

mapWidth :: Int
mapWidth = 100

mapHeight :: Int
mapHeight = 30

emptyMap :: GameMap
emptyMap = [wallRow] ++ (take (mapHeight - 2) (repeat emptyRow)) ++ [wallRow]
    where wallRow  = take mapWidth (repeat Wall)
          emptyRow = [Wall] ++ (take (mapWidth - 2) (repeat Empty)) ++ [Wall]

mapSquares :: Int
mapSquares = mapWidth * mapHeight

type Place = (Int, Int)

getCell :: GameMap -> Place -> Cell
getCell m (x, y) = (m!!x)!!y

setCell :: GameMap -> Place -> Cell -> GameMap
setCell m (x,y) cell = (take x m) ++ [newRow] ++ (drop (x + 1) m)
    where newRow = (take y oldRow) ++ [cell] ++ (drop (y + 1) oldRow)
          oldRow = m!!x

initState1 :: GameState
initState1 = Just (setCell emptyMap (10, 10) Head, Nothing)

gameStep1 :: GameState -> Maybe Char -> (GameState, String)
gameStep1 Nothing c = (Nothing, "")
gameStep1 (Just (m, dir)) c = (state, renderMap m)
    where state = Just (m, dir)

move :: Direction -> Place -> Place
move N (i, j) = (i-1, j)
move S (i, j) = (i+1, j)
move E (i, j) = (i, j+1)
move W (i, j) = (i, j-1)

charToDir :: Char -> Maybe Direction
charToDir 'i' = Just N
charToDir 'k' = Just S
charToDir 'l' = Just E
charToDir 'j' = Just W
charToDir _ = Nothing

dirToChar :: Direction -> Char
dirToChar N = 'i'
dirToChar S = 'k'
dirToChar E = 'l'
dirToChar W = 'j'

findHead :: GameMap -> Place
findHead m = head $ filter (\x -> getCell m x == Head) coords
    where coords = [(x, y) | x <- [0..mapHeight - 1], y <- [0..mapWidth - 1]]

intToPlace :: Int -> Place
intToPlace p = (div p mapWidth, mod p mapWidth)

gameStep2 :: GameState -> Maybe Char -> (GameState, String)
gameStep2 Nothing _                   = (Nothing, "")
gameStep2 (Just (m, Nothing)) Nothing = (Just (m, Nothing), renderMap m)
gameStep2 (Just (m, Just d)) Nothing  = gameStep2 (Just (m, Just d)) (Just (dirToChar d))
gameStep2 (Just (m, maybePrevDir)) (Just c)
    | not $ elem c "ijkl" = if (isNothing maybePrevDir)
                              then gameStep2 (Just (m, maybePrevDir)) Nothing
                              else gameStep2 (Just (m, maybePrevDir)) (Just (dirToChar (fromJust maybePrevDir)))
    | newHeadCell == Wall || newHeadCell == Body = (Nothing, "")
    | otherwise = (Just (newMap, Just newDir), renderMap newMap)
  where (Just newDir) = charToDir c
        prevHeadPlace = findHead m
        newHeadPlace  = move newDir prevHeadPlace
        newHeadCell   = getCell m newHeadPlace
        newMap        = setCell (setCell m prevHeadPlace Body) newHeadPlace Head

nextRand :: Int -> Int
nextRand x = mod (75 * x) 65537

randsFrom :: Int -> [Int]
randsFrom seed = iterate nextRand seed

randToPlace :: Int -> Place
randToPlace x = intToPlace $ (mod x mapSquares)

updateEmpty :: Cell -> GameMap -> Place -> GameMap
updateEmpty c m p
    | getCell m p == Empty = setCell m p c
    | otherwise            = m

countMapCells :: Cell -> GameMap -> Int
countMapCells c m
    | c == Wall = numOfCells - (mapHeight * mapWidth - 4)
    | otherwise = numOfCells
    where numOfCells = length (filter (== c) (concat m))

populate :: GameMap -> Cell -> Int -> GameMap
populate m Wall n = foldl (\x -> updateEmpty Wall x) m (map randToPlace (take n (randsFrom 70)))
populate m c n = foldl (\x -> updateEmpty c x) m (map randToPlace (take n (randsFrom 159)))

placeSnakeHead :: GameMap -> Int -> GameMap
placeSnakeHead m i
    | getCell m (randToPlace ((randsFrom 112)!!i)) == Empty = setCell m (randToPlace ((randsFrom 9)!!i)) Head
    | otherwise = placeSnakeHead m (i+1)

initState :: GameState
initState = Just (populatedMap, Nothing)
    where mapWithSnaks = populate emptyMap Snak 20
          mapWithWalls = populate mapWithSnaks Wall 10
          populatedMap = placeSnakeHead mapWithWalls 1

gameStep :: GameState -> Maybe Char -> (GameState, String)
gameStep Nothing _ = (Nothing, "")
gameStep (Just (m, Nothing)) Nothing
    | countMapCells Snak m == 0 = (Nothing, "")
    | otherwise      = (Just (m, Nothing), renderMap m)
gameStep (Just (m, Just d)) Nothing  = gameStep (Just (m, Just d)) (Just (dirToChar d))
gameStep (Just (m, maybePrevDir)) (Just c)
    | countMapCells Snak m == 0 = (Nothing, "")
    | not $ elem c "ijkl" = if (isNothing maybePrevDir)
                                then gameStep (Just (m, maybePrevDir)) Nothing
                                else gameStep (Just (m, maybePrevDir)) (Just (dirToChar (fromJust maybePrevDir)))
    | newHeadCell == Wall || newHeadCell == Body = (Nothing, "")
    | otherwise = (Just (newMap, Just newDir), renderMap newMap)
    where (Just newDir) = charToDir c
          prevHeadPlace = findHead m
          newHeadPlace  = move newDir prevHeadPlace
          newHeadCell   = getCell m newHeadPlace
          newMap        = setCell (setCell m prevHeadPlace Body) newHeadPlace Head

csi :: [Int] -> String -> String
csi args code = "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

setCursorPositionCode n m = csi [n + 1, m + 1] "H"
hSetCursorPosition h n m = hPutStr h $ setCursorPositionCode n m

clearScreenCode = csi [2] "J"
hClearScreen h = hPutStr h clearScreenCode

hideCursorCode = csi [] "?25l"
showCursorCode = csi [] "?25h"

hHideCursor h = hPutStr h hideCursorCode
hShowCursor h = hPutStr h showCursorCode

setCursorPosition = hSetCursorPosition stdout
clearScreen = hClearScreen stdout
showCursor = hShowCursor stdout
hideCursor = hHideCursor stdout

readKey :: IO (Maybe Char)
readKey = do ready <- hReady stdin
             if ready then
               do c <- getChar
                  return (Just c)
             else
               return Nothing

game :: GameState -> IO()
game Nothing  = do hSetEcho stdout True
                   showCursor
                   return ()
game (Just s) = do -- Wait a fraction of a second to make the game playable.
                   threadDelay 200000
                   -- Get a keypress if one is available.
                   c <- readKey
                   -- Using the current state and keypress, compute the new
                   -- state and what to display on the screen.
                   let (s2, out) = (gameStep (Just s) c) in
                     do -- Move the cursor to the top-left corner.
                        setCursorPosition 0 0
                        -- Print out the display string.
                        putStrLn out
                        -- Continue the loop with the new state.
                        game s2

main :: IO()
main = do hSetBuffering stdin NoBuffering
          clearScreen
          hideCursor
          hSetEcho stdout False
          game initState
