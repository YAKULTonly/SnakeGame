Week 8 Tutorial Work
--------------------

Although the exercises you have been given so far are computationally
interesting, they are a little boring from a practical perspective because
they mostly revolve revolve around taking a single input and producing a
single output. So this week, I thought it would be worth trying to write
something interactive, namely a simple game in the style of Snake:

http://en.wikipedia.org/wiki/Snake_%28video_game%29
https://www.youtube.com/watch?v=UmeKHtei0qo

Note the following from the Wikipedia article:

"As it moves forward, it leaves a trail behind, resembling a moving snake.
In some games the end of the trail is in a fixed position, so the snake
continually gets longer as it moves. In another common scheme the snake has
a specific length, so there is a moving tail a fixed number of units away
from the head."

We will start with the first kind of game, as this is easier to write. The
video is of the second kind of game.

The first problem is that lectures haven't covered how to write interactive
programs in Haskell, as these necessarily involve I/O: they have to read
the user's input and write to a display. So that you don't have to learn the 
details of this, I have supplied a short framework that will let you write  
an interactive program without having to know this.

As this is beyond the normal set of problem sheets, please prioritise
finishing your practicals and don't spend much more time than you would on a
typical tutorial sheet. As the intent is that you will be able to run the
code you write, you might like to try this one on a computer (rather than
write it by hand) and send me your work by e-mail.

I have tried the supplied code on the computers in the Thom Lab. If you are
using a Mac or Windows computer, it might not work for you. In this case,
you might find it helpful to connect remotely to the department's computers
using SSH. The remote access service is ecs.ox.ac.uk .  Use the username
and password you would usually use when logging on in the Thom Lab. If you
don't have an SSH client installed on your Windows computer, you can
download and use PuTTY:

http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html

This file is written in Literate Haskell (.lhs):

https://www.haskell.org/haskellwiki/Literate_programming

This makes it easy to write text with code interspersed (rather than having
to put everything in long comments). You can write your answers in this form
if you like, or just copy everything to an ordinary (.hs) file and write
your textual answers as comments or separately.

Apart from providing an interesting end result, these questions are also
meant to serve as a review of the term's material, in particular concerning
lists and higher order functions. In order to help you to practise this,
please DO NOT WRITE ANY RECURSIVE FUNCTIONS in your answers; use the
standard higher order functions and operations on lists instead. If you have
to write any small helper functions that aren't named in the questions,
try to write them as anonymous lambdas (\x -> ...) rather than defining new
named functions.

0. The framework

Have a look at the file NullGame.hs, which demonstrates the framework I have
supplied. You can either compile this with ghc and run it, or load it in
ghci and run "main".

A "game" is defined by:

* a type GameState, which should be a Maybe type;
* a value initState :: GameState, which defines how the game starts;
* a function gameStep :: GameState -> Maybe Char -> (GameState, String),
which takes the existing state and a character of input from the user and
returns a pair of updated game state and a string to print on the screen.

The framework is responsible for actually reading a keypress from the user
(if there is one), passing it (and the current game state) to gameStep and
printing out the string it returns (at the top-left corner of the terminal,
so the previous output is overwritten). So you only need to change the three
things mentioned above.

The reason we make GameState a Maybe type is to give an easy way of
signalling whether to the framework whether to terminate the program. If the
GameState returned by gameStep is Nothing (instead of some Just a), the
framework will stop the program.

> -- We need to use these libraries:
> import System.IO
> import Data.List -- for intersperse
> import Control.Concurrent -- for threadDelay
> import Data.Maybe

1. The game state and map

To keep things simple, let's store just two things in the game state: a game
map and the current direction the snake is moving in.

> type GameState = Maybe (GameMap, Maybe Direction)

The Direction datatype will look familiar to you from Practical 2. We make
the current direction a Maybe so that the snake can be stationary at the
start of the game.

> data Direction = N | S | E | W deriving (Eq, Show)

The map will be a list of rows, each of which will be a list of column
cells. A cell can either empty, a piece of snake body, a flower the snake
can eat, a wall or the snake's head.

> data Cell = Empty | Body | Flower | Wall | Head deriving Eq
> type GameMap = [[Cell]]

This means that, for a map m, the cell at co-ordinates (x,y) is (m!!x)!!y .

1.(i)

Suppose we want to be able to print the map to the screen. We can assign a
character representation for each possible kind of cell like this:

> renderCell :: Cell -> Char
> renderCell Empty  = ' '
> renderCell Body   = 'o'
> renderCell Flower = 'i'
> renderCell Wall   = '#'
> renderCell Head   = '@'

Write a function renderMap to produce a string representation of the whole
map:

> renderMap :: GameMap -> String
> renderMap m = concat $ map (\x -> x ++ "\n") (map (map renderCell) m)

1.(ii)

We need a basic map to start off with. Write a function that creates a map
consisting just of a rectangle of walls with empty space in the middle, of
mapWidth width and mapHeight height:

> mapWidth :: Int
> mapWidth = 80

> mapHeight :: Int
> mapHeight = 25

> emptyMap :: GameMap
> emptyMap = [wallRow] ++ (take (mapHeight - 2) (repeat emptyRow)) ++ [wallRow]
>     where wallRow  = take mapWidth (repeat Wall)
>           emptyRow = [Wall] ++ (take (mapWidth - 2) (repeat Empty)) ++ [Wall]

While we are doing this, we can also calculate mapSquares, the total number of
cells in the map:

> mapSquares :: Int
> mapSquares = mapWidth * mapHeight

1.(iii)

We are going to want to be able to read and write map cells easily. Take the
Place type from Practical 2, encoding a pair of (x,y) co-ordinates:

> type Place = (Int, Int)

Now write functions to get/set the contents of a map cell:

> getCell :: GameMap -> Place -> Cell
> getCell m (x, y) = (m!!x)!!y

> setCell :: GameMap -> Place -> Cell -> GameMap
> setCell m (x,y) cell = (take x m) ++ [newRow] ++ (drop (x + 1) m)
>     where newRow = (take y oldRow) ++ [cell] ++ (drop (y + 1) oldRow)
>           oldRow = m!!x

1.(iv)

Now check that everything is working so far.

Write a value initState that is the empty map with the snake's head set at
co-ordinates (10, 10):

> initState :: GameState
> initState = Just (setCell emptyMap (10, 10) Head, Nothing)

Write a function gameStep that returns the GameState unchanged and a string
representation of the map from the GameState:

> gameStep :: GameState -> Maybe Char -> (GameState, String)
> gameStep Nothing c = (Nothing, "")
> gameStep (Just (m, dir)) c = (state, renderMap m)
>     where state = Just (m, dir)

We will replace these later.

2. Snake movement

2.(i)

You wrote a function move in Practical 2 that takes a direction and a place
and returns a new place one square in that direction. We need something
similar here:

> move :: Direction -> Place -> Place
> move N (i, j) = (i, j - 1)
> move S (i, j) = (i, j + 1)
> move E (i, j) = (i + 1, j)
> move W (i, j) = (i - 1, j)

Note I am using left-handed co-ordinates, so (0,0) is at the top-left
corner. This is often more natural in terminal-based systems, where the
cursor moves from top to bottom as you write.

2.(ii)

The player can interact with the game by pressing keys, which indicate a
direction. Choose a suitable set of keys for controlling the game and write
a function that turns the corresponding character into Just the appropriate
direction, or Nothing if there is no direction:

> charToDir :: Char -> Maybe Direction
> charToDir 'i' = Just N
> charToDir 'k' = Just S
> charToDir 'l' = Just E
> charToDir 'j' = Just W
> charToDir _ = Nothing

Justify your choice of keys.

2.(iii)

We don't store the co-ordinates of the snake's head outside the game map.
Write a function that, given a map, returns the co-ordinates of the head.

> findHead :: GameMap -> Place
> findHead m = head $ filter (\x -> getCell m x == Head) coords
>     where coords = [(x, y) | x <- [0..mapHeight - 1], y <- [0..mapWidth - 1]]

Alternative solution:
head [(x, y) | x <- [0..mapHeight - 1], y <- [0..mapWidth - 1], getCell m (x, y) == Head]

(We can assume that a map will always contain exactly one snake head.)

One neat way of doing this involves writing a function to turn a cell index
into a place. For example, the 0th is cell is at (0,0) the 1st cell is at
(1,0) and the 80th cell is at (0,1):

> intToPlace :: Int -> Place
> intToPlace = undefined

2.(iv)

Now we can write a new gameStep function that:

* finds the current (old) position of the head;
* checks if the key pressed by the player corresponds to a direction;
* if so, calculates the new position of the head;
* sets the cell for the new position to be a snake head cell;
* sets the cell for the old position to be a snake body cell.

Don't forget to return the new state and the string representation of the
map, as before.

2.(v)

Following on from (iv), update the gameStep function to store the direction
moved in the game state, and use that as a default direction if the player
does not press a key correpsonding to one.

2.(vi)

Update the gameStep function yet again, this time to check if the new
position of the head contains a wall or snake body; if so, the snake dies
and the game should finish.

3. Populating the map with flowers and walls

The game as it stands is a little boring, as there is nothing to eat and no
way to win. We would like to put in some flowers and some more walls as
obstacles. We could just draw some pre-defined maps, but let's use the
computer to generate them instead.

3.(i)

First we need a "random" number generator. We can't generate a random number
programatically; we need some external source of entropy. Unfortunately,
accessing such a source would be an I/O operation. So instead we will use a
"pseudo-random" number generator. This is a sequence of numbers that is
sufficiently disorderly to appear random.

A pseudo-random number generator still needs a random "seed" to start with.
A common trick is to use the current time of day. Again, reading the time is
an I/O action. Another common trick used in games with a "menu screen" is to
use the time (in terms of number of game steps) between starting the game
program and choosing the option to start the game. We could do that, but for
simplicity, let's just use 1 all the time!

The random number generator we will use is described here:

http://en.wikipedia.org/wiki/Lehmer_random_number_generator

Write a function nextRand that, given X_k, computes X_(k+1). Take g to be 75
and n to be 65537. (These values were used by the ZX Spectrum home computer
in the 1980s.)

Hence write rands, an infinite list of pseudo-random numbers (assuming the
seed value X_0 = 1):

> nextRand :: Int -> Int
> nextRand = undefined

> rands :: [Int]
> rands = undefined

3.(ii)

Next we need to be able to turn a random number into a place on the map.
Write a function randToPlace that, given a random number which came from
the random number generator (and hence is in the range 0-65536), returns a
random place on the map:

> randToPlace :: Int -> Place
> randToPlace = undefined

3.(iii)

Our strategy is going to be to pick a random cell in the map, put a wall or
flower there if there isn't one already, and keep going until we have placed
enough walls and flowers. This is OK provided the density of filled cells in
the map is low. Why is this important?

Write a function that takes a cell type (such as wall or flower), a map and
a place, and sets the cell at that place to be of that type, but only if the
cell was empty:

> updateEmpty :: Cell -> GameMap -> Place -> GameMap
> updateEmpty = undefined

3.(iv)

We also need to be able to count how many cells of a particular exist, so we
know when to stop adding more. Write a function that, given a cell type and
a map, counts the number of cells of that type in the map:

> countMapCells :: Cell -> GameMap -> Int
> countMapCells = undefined

3.(v)

Now write a function populate that, given a map, a cell type, and a number
n, returns an updated version of that map where "random" empty cells have
been filled until there are at least n cells of the corresponding type:

> populate :: GameMap -> Cell -> Int -> GameMap
> populate = undefined

For example, "populate m Flower 10" should return the map m updated to have
at least 10 flowers at random positions. This will only be run at the start
of the game, so it need not be too efficient.

You may find it helpful to start with a function that, given a map and cell type,
produces an infinite list of maps with non-strictly increasing numbers of
cells of that type:

> tryRandomAdd :: GameMap -> Cell -> [GameMap]
> tryRandomAdd = undefined

3.(vi)

Write a new initState that populates the empty map with 10 randomly-placed
flowers, 10 randomly-placed walls (in addition to those in the empty map)
and 1 randomly-placed snake head.

3.(vii)

Update gameStep to stop the game if there are no flowers left on the map (so
the player has won).

4. Moving snake tail

4.(i)

If you are feeling enthusiastic, change the game so that the snake has a
fixed maximum length, which increases every time it eats a flower.

You will need to extend the game state to track not only the maximum snake
length, but also a list of snake body cells, either in the order they were
added or the order they need to be removed.

4.(ii)

This could be very slow for a long snake, as body cells have to be
continually added to one end of the list and removed from the other. What is
the worst case time complexity in terms of the map width w and map height h?

4.(iii)

How does this compare with the time complexity of gameStep before making
this change?

4.(iv)

How could you make the time complexity of maintaining the body list constant
on average (that is, "amortised constant")?

> -- Game framework:

> -- As System.Console.ANSI isn't installed on the teaching machines, I've
> -- copied and pasted the relevant bits of code here.

> csi :: [Int] -> String -> String
> csi args code = "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

> setCursorPositionCode n m = csi [n + 1, m + 1] "H"
> hSetCursorPosition h n m = hPutStr h $ setCursorPositionCode n m

> clearScreenCode = csi [2] "J"
> hClearScreen h = hPutStr h clearScreenCode

> hideCursorCode = csi [] "?25l"
> showCursorCode = csi [] "?25h"

> hHideCursor h = hPutStr h hideCursorCode
> hShowCursor h = hPutStr h showCursorCode

> -- Also these helper functions:

> setCursorPosition = hSetCursorPosition stdout
> clearScreen = hClearScreen stdout
> showCursor = hShowCursor stdout
> hideCursor = hHideCursor stdout

> -- Read and return Just a keypress if one is available;
> -- return Nothing if one is not.
> readKey :: IO (Maybe Char)
> readKey = do ready <- hReady stdin
>              if ready then
>                do c <- getChar
>                   return (Just c)
>              else
>                return Nothing

> -- Main loop that executes one step of the game.
> game :: GameState -> IO()
> -- If the current state is nothing, reset the terminal and terminate.
> game Nothing  = do hSetEcho stdout True
>                    showCursor
>                    return ()
> game (Just s) = do -- Wait a fraction of a second to make the game playable.
>                    threadDelay 100000
>                    -- Get a keypress if one is available.
>                    c <- readKey
>                    -- Using the current state and keypress, compute the new
>                    -- state and what to display on the screen.
>                    let (s2, out) = (gameStep (Just s) c) in
>                      do -- Move the cursor to the top-left corner.
>                         setCursorPosition 0 0
>                         -- Print out the display string.
>                         putStrLn out
>                         -- Continue the loop with the new state.
>                         game s2

> -- Initialise the screen and start the game with the initial state.
> main :: IO()
> main = do hSetBuffering stdin NoBuffering
>           clearScreen
>           hideCursor
>           hSetEcho stdout False
>           game initState

