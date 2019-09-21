{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module EscapeTheRoom where

import CodeWorld
import EscapeTheRoom.Levels

playerTile :: Picture
playerTile = lettering "\x1F6B6"

floorTile :: Picture
floorTile = colored yellow (solidRectangle 0.95 0.95)

wallTile :: Picture
wallTile = colored black (solidRectangle 0.95 0.95)

pictureOfButton :: Color -> Picture
pictureOfButton c = colored c (solidCircle 0.3)

buttonTile :: Color -> Picture
buttonTile c = pictureOfButton c <> floorTile

doorTile :: Color -> Picture
doorTile c = pictureOfButton c <> wallTile

exitTile :: Picture
exitTile = colored red (thickRectangle 0.1 0.3 0.3) <>
  colored red (thickRectangle 0.1 0.7 0.7) <>
  colored black (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Player = playerTile
drawTile Wall = wallTile
drawTile Floor = floorTile
drawTile Exit = exitTile
drawTile (Button c) = buttonTile (doorColorToColor c)
drawTile (Door c) = doorTile (doorColorToColor c)

doorColorToColor:: DoorColor -> Color
doorColorToColor CRed = red
doorColorToColor CGreen = green
doorColorToColor CBlue = blue
doorColorToColor CPink = pink
doorColorToColor CPurple = purple
doorColorToColor CYellow = yellow
doorColorToColor CCyan = cyan
doorColorToColor COrange = orange
doorColorToColor CWhite = white
doorColorToColor CGray = gray
doorColorToColor CBrown = brown
doorColorToColor CBlack = black
doorColorToColor CDarkRed = dark red
doorColorToColor CDarkGreen = dark green
doorColorToColor CLightBlue = light blue


drawTileAt :: (Coords -> Tile) -> [Coords] -> Integer -> Integer -> Picture
drawTileAt levelMap openedDoors i j
  | elem coords openedDoors = translated x y (drawTile Floor)
  | otherwise = translated x y (drawTile (levelMap coords))
  where
    x = fromInteger i
    y = fromInteger j
    coords = (Coords i j)

drawPlayerAt :: Coords -> Picture
drawPlayerAt (Coords i j) =
  translated x y (drawTile Player)
  where
    x = fromInteger i
    y = fromInteger j

drawFromTo :: (Integer, Integer) -> (Integer -> Picture) -> Picture
drawFromTo (from, to) drawSomething
  | from > to = blank
  | otherwise
     = drawSomething from
    <> drawFromTo (from + 1, to) drawSomething

-- | Draw a given level map of size 21x21.
drawLevelMap :: (Coords -> Tile) -> [Coords] -> Picture
drawLevelMap levelMap openedDoors = drawFromTo rows
           (\i -> drawFromTo columns (drawTileAt levelMap openedDoors i))
           where
             rows = (-10, 10)
             columns = (-10, 10)

canMove :: Tile -> Bool
canMove Floor = True
canMove (Button _) = True
canMove Exit = True
canMove _ = False

nextCoords :: Dir -> Coords -> Coords
nextCoords Up (Coords i j) = Coords i (j + 1)
nextCoords Down (Coords i j) = Coords i (j - 1)
nextCoords ToLeft (Coords i j) = Coords (i - 1) j
nextCoords ToRight (Coords i j) = Coords (i + 1) j

-- | Try move character in a given direction.
tryMove :: Dir -> State -> State
tryMove dir (State (Level coords levelMap openedDoors) levels)
  | elem newCoords openedDoors || canMove (levelMap newCoords)
    = checkButton (State (Level newCoords levelMap openedDoors) levels)
  | otherwise = (State (Level coords levelMap openedDoors) levels)
  where
    newCoords = nextCoords dir coords

getColor :: Tile -> DoorColor
getColor (Button color) = color
getColor (Door color) = color
getColor _ = CYellow

isButton :: Tile -> Bool
isButton (Button _) = True
isButton _ = False

isExit :: Tile -> Bool
isExit Exit = True
isExit _ = False

checkButton :: State -> State
checkButton (State (Level coords levelMap openedDoors) (next_level:levels))
  | isExit tile = (State next_level levels)
  | isButton tile
      = State (Level coords levelMap (triggerDoors color levelMap openedDoors)) (next_level:levels)
  | otherwise = State (Level coords levelMap openedDoors) (next_level:levels)
  where
    tile = levelMap coords
    color = getColor tile
checkButton (State (Level coords levelMap openedDoors) []) -- ^ last level case
  | isButton tile
      = State (Level coords levelMap (triggerDoors color levelMap openedDoors)) []
  | otherwise = State (Level coords levelMap openedDoors) []
  where
    tile = levelMap coords
    color = getColor tile

isDoor :: Tile -> Bool
isDoor (Door _) = True
isDoor _ = False

-- | Open some doors.
openDoors :: DoorColor -> (Coords -> Tile) -> [Coords] -> [Coords]
openDoors _ _ [] = []
openDoors color levelMap (coords:cs)
  | isDoor tile && getColor tile == color = openDoors color levelMap cs
  | otherwise = coords : openDoors color levelMap cs
  where
    tile = levelMap coords

isClosedDoor :: DoorColor -> (Coords -> Tile) -> [Coords]
  -> Coords -> Bool
isClosedDoor color levelMap openedDoors (Coords i j) =
  isDoor tile && color == getColor tile && closed
  where
    tile = (levelMap (Coords i j))
    closed = not (elem (Coords i j) openedDoors)

-- https://wiki.haskell.org/List_comprehension
getClosedDoors :: DoorColor -> (Coords -> Tile) -> [Coords] -> [Coords]
getClosedDoors color levelMap openedDoors =
    [Coords i j |
    i <- [-10..10],
    j <- [-10..10],
    isClosedDoor color levelMap openedDoors (Coords i j)]

triggerDoors :: DoorColor -> (Coords -> Tile) -> [Coords] -> [Coords]
triggerDoors color levelMap openedDoors =
  getClosedDoors color levelMap openedDoors
  ++ openDoors color levelMap openedDoors

-- | Is current level complete given some game 'State'?
isLevelComplete :: Level -> Bool
isLevelComplete (Level coords levelMap openedDoors) = isExit (levelMap coords)

initialWorld :: State
initialWorld = State first_level game_levels
  where
    first_level = head levels
    game_levels = tail levels


handleWorld :: Event -> (State -> State)
handleWorld (KeyPress "Up") = tryMove Up
handleWorld (KeyPress "Down") = tryMove Down
handleWorld (KeyPress "Left") = tryMove ToLeft
handleWorld (KeyPress "Right") = tryMove ToRight
handleWorld _ = id

drawWorld :: State -> Picture
drawWorld (State (Level playerCoords levelMap openedDoors) levels)
  | isLevelComplete (Level playerCoords levelMap openedDoors) = lettering "Good Game. Hope you enjoyed!"
  | otherwise = drawPlayerAt playerCoords <> drawLevelMap levelMap openedDoors


run :: IO ()
run = activityOf initialWorld handleWorld drawWorld