import System.IO
--import System.Console.ANSI -- for showing/hiding cursor, clearing screen...
import Data.List -- for intersperse
import Control.Concurrent -- for threadDelay

---

-- As System.Console.ANSI isn't installed on the teaching machines, I've
-- copied and pasted the relevant bits of code here.

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

-- Also these helper functions:

setCursorPosition = hSetCursorPosition stdout
clearScreen = hClearScreen stdout
showCursor = hShowCursor stdout
hideCursor = hHideCursor stdout

---

-- The current state of a game.
-- If it is Nothing, indicates that the game is over and should quit.
type GameState = Maybe Integer

-- Initial state of the game.
initState :: GameState
initState = Just 0

-- Function to progress the game.
-- Takes as arguments the current state and an input character.
-- Returns the new state of the game and a string to display on the screen.
gameStep :: GameState -> Maybe Char -> (GameState, String)
gameStep _ (Just _) = (Nothing, "")
gameStep (Just n) _ = (Just (n+1), show n)

-- Read and return Just a keypress if one is available;
-- return Nothing if one is not.
readKey :: IO (Maybe Char)
readKey = do ready <- hReady stdin
             if ready then
               do c <- getChar
                  return (Just c)
             else
               return Nothing

-- Main loop that executes one step of the game.
game :: GameState -> IO()
-- If the current state is nothing, reset the terminal and terminate.
game Nothing  = do hSetEcho stdout True
                   showCursor
                   return ()
game (Just s) = do -- Wait a fraction of a second to make the game playable.
                   threadDelay 100000
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

-- Initialise the screen and start the game with the initial state.
main :: IO()
main = do hSetBuffering stdin NoBuffering
          clearScreen
          hideCursor
          hSetEcho stdout False
          game initState

