-- | Levels for Escape the Room.
module EscapeTheRoom.Levels where

-- | Tiles used in Escape the Room game.
data Tile
  = Wall                -- ^ An unpassable wall.
  | Floor               -- ^ Floor to walk on.
  | Door DoorColor      -- ^ Door of a given color.
  | Button DoorColor    -- ^ A button that opens/toggles
                        -- all doors of the same color.
  | Player              -- ^ player tile
  | Exit                -- ^ An exit.

-- | Available door and button colors.
data DoorColor
  -- standard colors
  = CRed | CGreen | CBlue
  -- extra colors
  | CPink | CPurple | CYellow | CCyan | COrange
  | CWhite | CGray | CBrown | CBlack
  | CDarkRed | CDarkGreen  | CLightBlue  deriving (Eq)

-- | Coordinates on a level map.
data Coords = Coords Integer Integer deriving (Eq)

-- | Direction
data Dir = Up | Down | ToLeft | ToRight

-- | A level map with initial coordinates.
data Level = Level
  Coords            -- Start coordinates of the player.
  (Coords -> Tile)  -- Level map.
  [Coords]          -- Doors, opened on start.

data State = State Level [Level]

-- | A list of all level maps.
levels :: [Level]
levels =
  [ level1  -- Author: Ildar Yalalov
  , level2  -- Author: Ilya Dubovitsky
  , level3  -- Author: Timur Khalilev
  , level4  -- Author: Ivan Lyagaev
  , level5  -- Author: Rozaliya Amirova
  , level6  -- Author: Ilgizar Murzakov
  , level7  -- Author: Arsen Kuzmin
  , level8  -- Author: Ilshat Gibadullin
  , level9  -- Author: Dmitry Turenko
  , level10 -- Author: Nikolay Buldakov
  , level11 -- Author: Aidar Valeev
  , level12 -- Author: Boris Guryev
  , level13 -- Author: Mike Kuskov
  ]

-- | Author: Ildar Yalalov
level1 :: Level
level1 = Level (Coords (- (n -1) ) (- (n-1) )) levelMap []
  where
    n = 10
    levelMap (Coords i j)
      | abs i > n-1 || abs j > n-1 = Wall  -- walls around map
      | i == 0 && j == -3 = Exit
      | i == -8 && j == 3  = Button CGreen
      | i == 6 && j == -7 = Door CRed
      | i == 8 && j == -5 = Button CBlue
      | i == 2 && j == -4 = Door CBlue
      | i == 0 && j == -5 = Button CRed
      | i == 2 && j == -3 = Button CRed
      | i == -5 && j == 0 = Door CGreen
      | i == -3 && j == -2 = Door CRed
      | i == -2 && j > 1 = Wall
      | i == -4 && j < n-1 && j >=0 = Wall
      | i == 0 && j >=0 && j < n-1 = Wall
      | i == -3 && j <= 0 && j >= -3 = Wall
      | i == 1 && j < 0 && j >= -3 = Wall
      | i == 3 && j < 0 && j >= -3 = Wall
      | i <= 1 && j == 0 = Wall
      | i == 1 && j < -4 && abs j < n-1 = Wall
      | i < 6 && j == -4 = Wall
      | i == 6 && j < (-2) = Wall
      | i > 6 && j == -3 = Wall
      | otherwise = Floor -- floor otherwise

-- | Author: Ilya Dubovitsky
level2 :: Level
level2 = Level (Coords (-9) (-9)) levelMap []
  where
    levelMap (Coords i j)
      | abs i == 10 || abs j == 10 = Wall  -- walls around map
      | (i, j) == (0, 0) = Exit
      | (i, j) == (-8, 8) = Button CRed
      | (i, j) == (-2, 8) = Button CGreen
      | (i, j) == (8, -5) = Button CBlue
      | (i, j) == (-5, 5) = Door CBlue
      | (i, j) == (1, 2) = Door CRed
      | (i, j) == (5, -8) = Door CGreen
      | i == -8 && j < -3 = Wall
      | i == -5 && j > -8 && j < -1 = Wall
      | i == -4 && j > -1 && j < 4 = Wall
      | i == -4 && j > 4 = Wall
      | i == -2 && j > -3 && j < 3 = Wall
      | i == 2 && j > -3 && j < 3 = Wall
      | i == 3 && j > -9 && j < -3 = Wall
      | i == 5 && j < -2 = Wall
      | i == 5 && j > 6 && j < 9 = Wall
      | i == 5 && j > 0 && j < 6 = Wall
      | i == 8 && j > -2 && j < 7 = Wall
      | i == -3 && j < -7 = Wall
      | i == 0 && j > 4 && j < 9 = Wall
      | i == 1 && j > -5 && j < -1 = Wall
      | j == 5 && i > -10 && i < 6 = Wall
      | j == -2 && i > -10 && i < -4 = Wall
      | j == -8 && i > -2 && i < 4 = Wall
      | j == -6 && i > -3 && i < 4 = Wall
      | j == -3 && i > 4 = Wall
      | j == 0 &&  i > -9 && i < -3 = Wall
      | j == 2 && i > -3 && i < 3 = Wall
      | j == -2 && i > -3 && i < 3 = Wall
      | j == 7 && i > -1 && i < 4 = Wall
      | j == 6 && i > 5 && i < 9 = Wall
      | j == -1 && i > 4 && i < 9 = Wall
      | (i, j) == (4, -9) = Wall
      | (i, j) == (-7, -6) = Wall
      | (i, j) == (-3, 3) = Wall
      | (i, j) == (-1, 4) = Wall
      | (i, j) == (2, -4) = Wall
      | (i, j) == (3, 4) = Wall
      | (i, j) == (4, 1) = Wall
      | (i, j) == (6, 4) = Wall
      | (i, j) == (7, 2) = Wall
      | (i, j) == (4, -9) = Wall
      | otherwise = Floor

-- | Author: Timur Khalilev
level3 :: Level
level3 = Level (Coords 0 0) levelMap []
  where
    levelMap (Coords x y)
      | abs(x) > 9 ||abs(y) > 9 = Wall
      | x == 0 && y /= 0 = Wall
      | x == 5 && y>6 = Wall
      | y==5 && x>1 = Wall
      | y==2 && x>1 = Wall
      | x == (-1) && y == 1 = Wall
      | x== (-2) && y ==1 = Wall
      | x== (-2) && y ==0 = Wall
      | x == (-2) && y == (-2) = Wall
      | x == (-1) && y == (-2) = Wall
      | x == 7 && y== 4 = Wall
      | x == -6 && y < (-5) = Wall
      | y == -6 && x < (-7) = Wall
      | x ==5 && y == 6 = Door CBlue
      | x == 8 && y==(-9) = Button CBlue
      | x ==7 && y == 3 = Door CRed
      | x == 9 && y==9 = Button CRed
      | x== 9 && y==4 = Button CPink
      | x== -2 && y==(-1) = Door CPink
      | x== -7 && y==(-6) = Door CPurple
      | y ==9 && x== -1 = Button CPurple
      | x== (-9) && y ==(-9) = Exit
      | otherwise = Floor

-- | Author: Ivan Lyagaev
level4 :: Level
level4 = Level (Coords 2 2) levelMap []
  where
    levelMap (Coords x y)
      | abs x > 9 || abs y > 9 = Wall

      | x == -2 && y == 2      = Exit

      | x == 2 && y == 4       = Floor
      | x == 4 && y == -6      = Floor
      | x == 0 && y == -6      = Floor
      | x == -4 && y == -6     = Floor
      | x == 4 && y == -2      = Floor
      | x == 0 && y == -2      = Floor
      | x == -4 && y == -2     = Floor

      | x == -6 && y == -2        = Button CGreen
      | x == 0 && y > 0 && y < 4  = Door CGreen
      | x == -4 && y > 0 && y < 4 = Door CGreen
      | x > -4 && x < 0 && y == 4 = Door CGreen
      | x > -4 && x < 0 && y == 0 = Door CGreen

      | x == 2 && y == 6       = Button CRed
      | x == 2 && y == 8       = Door CRed
      | x == 4 && y == 6       = Door CRed

      | x == -6 && y == 6      = Button CPurple
      | x == 8 && y == -2      = Door CPurple

      | x == 6 && y == 8       = Button CBlue
      | x == 6 && y == -8      = Button CBlue
      | x == 8 && y == 9       = Door CBlue
      | x == 9 && y == 8       = Door CBlue
      | x == 9 && y == 4       = Door CBlue
      | x == 9 && y == 0       = Door CBlue
      | x == 9 && y == -4      = Door CBlue
      | x == 9 && y == -8      = Door CBlue
      | x == 8 && y == -9      = Door CBlue

      | x == -6 && y == 8       = Button CBlue
      | x == -6 && y == -8      = Button CBlue
      | x == -8 && y == 9       = Door CBlue
      | x == -9 && y == 8       = Door CBlue
      | x == -9 && y == 4       = Door CBlue
      | x == -9 && y == 0       = Door CBlue
      | x == -9 && y == -4      = Door CBlue
      | x == -9 && y == -8      = Door CBlue
      | x == -8 && y == -9      = Door CBlue

      | y `mod` 4 == 0         = Wall
      | x `mod` 4 == 0         = Wall
      | otherwise              = Floor

-- | Author: Rozaliya Amirova
level5 :: Level
level5 = Level (Coords 0 (-1)) levelMap []
  where
    levelMap (Coords i j)
     | (i, j) == (4, -3)                                   = Exit
     | (i, j) == (5, -3)                                   = Door CRed
     | (i, j) `elem` [(6, -3), (-3, -5), (9, 8), (-5, -2)] = Door CBlue
     | (i, j) `elem` [(7, -3), (-3, -4)]                   = Door CGreen
     | (i, j) == (8, -3)                                   = Door CBlack
     | (i, j) == (9, 9)                                    = Door CPink
     | (i, j) == (0, -7)                                   = Button CPink
     | (i, j) `elem` [(5, 9), (6, 9)]                      = Button CGreen
     | (i, j) `elem` [(7, 9), (8, 9)]                      = Button CRed
     | (i, j) == (-2, -5)                                  = Button CBlack
     | (i, j) == (-7, -4)                                  = Button CBlue
     | abs i > 9 || abs j > 9
         || j == 9 && i `elem` [-1, 4]
         || j == 8 && i `elem` [-8, -7, -6, -4, -3, -1, 1, 2, 4, 5, 6, 7, 8]
         || j == 7 && i `elem` [-8, -6, -3, -1, 1, 4]
         || j == 6 && i `elem` [-8, -6, -5, -3, -1, 0, 1, 3, 4, 6, 7, 8]
         || j == 5 && i `elem` [-8, -6, -3, 3, 8]
         || j == 4 && not (i `elem` [-9, -7, -5, 6])
         || j == 3 && i `elem` [-6, 2]
         || j == 2 && not (i `elem` [-9, 1, 3, 9])
         || j == 1 && i `elem` [-8, -4, 0, 2, 8]
         || j == 0 && i `elem` [-6, -5, -4, -2, 0, 2, 3, 4, 5, 6, 7, 8]
         || j == -9 && i `elem` [-6, 0]
         || j == -8 && not (i `elem` [-9, 9, -7, -5, -1, 4])
         || j == -7 && i `elem` [-8, -6, -2,3,  8]
         || j == -6 && not (i `elem` [-9, 9, -7, -5, 0, 7])
         || j == -5 && i `elem` [-8, -6, -4, -1, 8]
         || j == -4 && not (i `elem` [-9, -7, -5, -3, 9])
         || j == -3 && i `elem` [-8, -7, -6, -4, 3]
         || j == -2 && not (i `elem` [-9, -8, -7, -5, 2])
         || j == -1 && i `elem` [-9, -8, -2, -6]            = Wall
      | otherwise                                           = Floor

-- | Author: Ilgizar Murzakov
level6 :: Level
level6 = Level (Coords 9 5) levelMap []
  where
    levelMap (Coords i j)
      | abs i > 9 || abs j > 9 = Wall
      -- bonus
      | i == 9 && j == 9 = Button CYellow
      -- blue
      | j == (-9) && i == (-8) = Door layer1
      | j == (-8) && i == (-9) = Door layer1
      | j == (-8) && i == (-8) = Door layer1
      | i == 8 && j == -8 = Button layer1
      --green
      | j == (-9) && i == (-7) = Door layer2
      | j == (-8) && i == (-7) = Door layer2
      | j == (-7) && i == (-7) = Door layer2
      | j == (-7) && i == (-8) = Door layer2
      | j == (-7) && i == (-9) = Door layer2
      | i == -8 && j == 8 = Button layer2
      --mines
      | j == (-4) && i == (-2) = Door c
      | j == (-5) && i == (-1) = Door c
      | j == (-5) && i == (-2) = Button c
      | j == (-5) && i == (-3) = Door c
      | j == (-6) && i == (-2) = Door c

      | j == (-4) && i == (4) = Door c
      | j == (-5) && i == (5) = Door c
      | j == (-5) && i == (4) = Button c
      | j == (-5) && i == (3) = Door c
      | j == (-6) && i == (4) = Door c

      | j == (-4) && i == (4) = Door c
      | j == (-5) && i == (5) = Door c
      | j == (-5) && i == (4) = Button c
      | j == (-5) && i == (3) = Door c
      | j == (-6) && i == (4) = Door c

      | j == 0 && i == -8 = Door c
      | j == 1 && i == -7 = Door c
      | j == 1 && i == -8 = Button c
      | j == 1 && i == -9 = Door c
      | j == 2 && i == -8 = Door c

      | j == -4 && i == 5 = Door c
      | j == -3 && i == 6 = Door c
      | j == -3 && i == 5 = Button c
      | j == -3 && i == 4 = Door c
      | j == -2 && i == 5 = Door c

      | j == -2 && i == 7 = Door c
      | j == -1 && i == 8 = Door c
      | j == -1 && i == 7 = Button c
      | j == -1 && i == 6 = Door c
      | j == 0 && i == 7 = Door c

      | j == 8 && i == -2 = Door c
      | j == 9 && i == -1 = Door c
      | j == 9 && i == -2 = Button c
      | j == 9 && i == -3 = Door c
      | j == 10 && i == -2 = Door c

      | j == -8 && i == 6 = Door c
      | j == -7 && i == 7 = Door c
      | j == -7 && i == 6 = Button c
      | j == -7 && i == 5 = Door c
      | j == -6 && i == 6 = Door c

      | j == -1 && i == -4 = Door c
      | j == 0 && i == -3 = Door c
      | j == 0 && i == -4 = Button c
      | j == 0 && i == -5 = Door c
      | j == 1 && i == -4 = Door c

      | j == -7 && i == 0 = Door c
      | j == -6 && i == 1 = Door c
      | j == -6 && i == 0 = Button c
      | j == -6 && i == -1 = Door c
      | j == -5 && i == 0 = Door c

      | j == 7 && i == 1 = Door c
      | j == 8 && i == 2 = Door c
      | j == 8 && i == 1 = Button c
      | j == 8 && i == 0 = Door c
      | j == 9 && i == 1 = Door c

      | j == -5 && i == 9 = Door c
      | j == -4 && i == 10 = Door c
      | j == -4 && i == 9 = Button c
      | j == -4 && i == 8 = Door c
      | j == -3 && i == 9 = Door c

      | j == 5 && i == 6 = Door c
      | j == 6 && i == 7 = Door c
      | j == 6 && i == 6 = Button c
      | j == 6 && i == 5 = Door c
      | j == 7 && i == 6 = Door c

      | j == -6 && i == -9 = Door c
      | j == -5 && i == -8 = Door c
      | j == -5 && i == -9 = Button c
      | j == -5 && i == -10 = Door c
      | j == -4 && i == -9 = Door c

      | j == 7 && i == 4 = Door c
      | j == 8 && i == 5 = Door c
      | j == 8 && i == 4 = Button c
      | j == 8 && i == 3 = Door c
      | j == 9 && i == 4 = Door c

      | j == -3 && i == -8 = Door c
      | j == -2 && i == -7 = Door c
      | j == -2 && i == -8 = Button c
      | j == -2 && i == -9 = Door c
      | j == -1 && i == -8 = Door c

      | j == -4 && i == -6 = Door c
      | j == -3 && i == -5 = Door c
      | j == -3 && i == -6 = Button c
      | j == -3 && i == -7 = Door c
      | j == -2 && i == -6 = Door c

      | j == -8 && i == -1 = Door c
      | j == -7 && i == 0 = Door c
      | j == -7 && i == -1 = Button c
      | j == -7 && i == -2 = Door c
      | j == -6 && i == -1 = Door c

      | j == -3 && i == 6 = Door c
      | j == -2 && i == 7 = Door c
      | j == -2 && i == 6 = Button c
      | j == -2 && i == 5 = Door c
      | j == -1 && i == 6 = Door c

      | j == 4 && i == 0 = Door c
      | j == 5 && i == 1 = Door c
      | j == 5 && i == 0 = Button c
      | j == 5 && i == -1 = Door c
      | j == 6 && i == 0 = Door c

      | j == 0 && i == -1 = Door c
      | j == 1 && i == 0 = Door c
      | j == 1 && i == -1 = Button c
      | j == 1 && i == -2 = Door c
      | j == 2 && i == -1 = Door c

      | j == 2 && i == -3 = Door c
      | j == 3 && i == -2 = Door c
      | j == 3 && i == -3 = Button c
      | j == 3 && i == -4 = Door c
      | j == 4 && i == -3 = Door c

      | j == -1 && i == 0 = Door c
      | j == 0 && i == 1 = Door c
      | j == 0 && i == 0 = Button c
      | j == 0 && i == -1 = Door c
      | j == 1 && i == 0 = Door c

      | j == 4 && i == 2 = Door c
      | j == 5 && i == 3 = Door c
      | j == 5 && i == 2 = Button c
      | j == 5 && i == 1 = Door c
      | j == 6 && i == 2 = Door c

      | j == 1 && i == 5 = Door c
      | j == 2 && i == 6 = Door c
      | j == 2 && i == 5 = Button c
      | j == 2 && i == 4 = Door c
      | j == 3 && i == 5 = Door c

      -- exit
      | i == (-9) && j == (-9) = Exit
      | otherwise = Floor
        where
          c = CYellow
          layer1 = CBlue
          layer2 = CGreen

-- | Author: Arsen Kuzmin
level7 :: Level
level7 = Level (Coords 0 3) levelMap []
  where
    levelMap (Coords i j)
      | i==0 && j==0 = Exit
      | i==(-6) && j==0 = (Button CBlue)
      | i==(-5) && j==0 = (Wall)
      | j==(-5) && i==0 = (Wall)
      | j==5 && i==0 = (Wall)
      | i*i+j*j<=4  = (Door CBlue)
      | i*i+j*j<=16 = (Button CRed)
      | i*i+j*j>25 &&  i*i+j*j<36 = (Wall)
      | i*i+j*j>16 &&  i*i+j*j<49 = (Door CRed)
      | i*i+j*j>=49 &&  i*i+j*j<=81 = (Floor)
      |otherwise = Wall


-- | Author: Ilshat Gibadullin
level8 :: Level
level8 = Level (Coords (-9) 9) levelMap []
  where
    levelMap (Coords i j)
      | i == -4 && j == 9 = Door CPurple
      | i == -9 && j == 5 = Door CBlue
      | i == 0 && j == 5 = Door CRed
      | i == -4 && j == 3 = Door CRed
      | i == 1 && j == 3 = Door CBlue
      | i == -5 && j == 0 = Door CBlue
      | i == -7 && j == -5 = Door CBlue
      | i == -3 && j == -5 = Door CBlack
      | i == 1 && j == -9 = Door CBlue
      | i == -4 && j == -8 = Door CBlue
      | i == 2 && j == 0 = Door CBlue
      | i == 4 && j == 0 = Door CCyan
      | i == 5 && j == -7 = Door CCyan
      | i == 9 && j == 4 = Door CYellow
      | i == 6 && j == 9 = Door CGreen
      | i == -9 && j == 6 = Button CBlue
      | i == 0 && j == -6 = Button CPurple
      | i == 0 && j == 1 = Button CCyan
      | i == 0 && j == -1 = Button CGreen
      | i == -9 && j == -9 = Button CYellow
      | i == -3 && j == 7 = Button CRed
      | i == 5 && j == 5 = Button CBlack
      | abs i > 9 || abs j > 9 = Wall
      | i > 6 && j == 6 = Wall
      | j > 5 && i == 6 = Wall
      | i > 3 && j == 4 = Wall
      | j > 4 && i == 4 = Wall
      | i == 7 && j == 7  = Exit
      | i == 1 || i == 4 || i == -4 = Wall
      | i < 3 && j == 0    = Wall
      | i == 3 && j == -1    = Wall
      | i < 1 && j == -5 = Wall
      | i < 1 && j == 5 = Wall
      | i > 3 && j == -7 = Wall
      | otherwise = Floor

-- | Author: Dmitry Turenko
level9 :: Level
level9 = Level (Coords (-1) (-1)) levelMap []
  where
    levelMap (Coords i j)
      | i > 7 && j > 7 && i < 10 && j < 10 = Exit
      | i == 8 && j == 7                   = Door CYellow
      | i == 7 && j == 7                   = Button CYellow
      | i == 7 && j == 6                   = Button CYellow
      | i == 7 && (j == 6 || j == 5)       = Floor
      | i == 6 && j == 5                   = Door CBlue
      | i == 0 && j == 9                   = Door CBlue
      | i == 0 && j == 0                   = Button CBlue
      | i == -3 && j == -3                 = Button CRed
      | i == -4 && j == -3                 = Door CRed
      | i == -4 && j == -4                 = Door CRed
      | i == -5 && j == -4                 = Door CRed
      | i == -7 && j == -8                 = Button CRed
      | i == -7 && j == -7                 = Door CRed
      | i == -8 && j == -6                 = Floor
      | i == -8 && j == -7                 = Door CGreen
      | i == -8 && j == -8                 = Button CGreen
      | i == -9 && j == 0                  = Door CPurple
      | i == 0 && j == -9                  = Door CGreen
      | i == 4 && (j > -6 && j < -2)       = Door CPurple
      | i == 4 && j == -9                  = Button CPurple
      | abs (i) + abs(j) == 1              = Door CGreen
      | i == 9 && j == -7                  = Button CYellow
      | i > 5 && i < 9 && j == -7          = Floor
      | i == 8 && j == -6                  = Floor
      | i == 7 && j == -5                  = Floor
      | i == 8 && j == -8                  = Floor
      | i == 7 && j == -9                  = Floor
      | i == 5 && j == -7                  = Floor
      | i > 5                              = Wall
      | i == 5 && j < 0                    = Wall
      | i*i + j*j >= 100                   = Wall
      | i*i + j*j >24 && i*i + j*j < 50    = Wall
      | i == 0 || j == 0                   = Wall
      | otherwise                          = Floor

-- | Author: Nikolay Buldakov
level10 :: Level
level10 = Level (Coords (-9) (-6)) levelMap []
  where
    levelMap (Coords i j)
      | mapValue == '0' = Floor
      | mapValue == '1'  = Wall
      | mapValue == '2' = Button CGreen
      | mapValue == '3' = Button CBlue
      | mapValue == '4' = Button CRed
      | mapValue == '5' = Door CGreen
      | mapValue == '6' = Door CBlue
      | mapValue == '7' = Door CRed
      | mapValue == '8' = Exit
      | otherwise = Floor -- floor otherwise
      where
        mapValue = levelMapRaw !! (fromIntegral y) !! (fromIntegral x)
        y = -j + 10
        x = i + 10

    -- | A raw representation of the map
    -- | 0 - floor, 1 - wall, 2 - button green, 3 - button blue
    -- | 4 - button red, 5 - door green, 6 - door blue, 7 - door red
    -- | 8 - exit
    levelMapRaw:: [String]
    levelMapRaw = [
        "111111111111111111111",
        "100000003000001000001",
        "101611101011111010151",
        "101010001000000010101",
        "101011111511111110101",
        "101000001000100070101",
        "101111101110101110111",
        "101000000010100010001",
        "101011111117101211101",
        "161210000800101010601",
        "131116111711111010101",
        "101010100000000010131",
        "121010101111111110101",
        "151050101000000010101",
        "101710101011111510111",
        "100010131010001010501",
        "101111101010111011101",
        "106000001010100010601",
        "101111111010161110101",
        "101400030010000000101",
        "111111111111111111111"
        ]

-- | Author: Aidar Valeev
level11 :: Level
level11 = Level (Coords 7 (-8)) levelMap openDoors
  where
    openDoors = [Coords 1 (-6), Coords 6 7, Coords (-3) (-2), Coords 0 8]
    levelMap (Coords i j)
      | abs i > 9 || abs j > 9 = Wall
      -- V
      | i == -9 && j > 3 && j < 8 = Door CBlue
      | i == -8 && j == 3 = Door CBlue
      | i == -7 && j > 3 && j < 8 = Door CBlue
      -- A
      | i == -5 && j > 2 && j < 7  = Wall
      | i == -4 && (j == 4 || j == 7) = Wall
      | i == -3 && j > 2 && j < 7  = Wall
      -- L
      | i == -1 && j > 0 && j < 8  = Wall
      | i > -1 && i < 4 && j == 1  = Wall
      -- EE
      | i == 1 && j > 2 && j < 8  = Wall
      | i == 2 && j > 2 && j < 8 && j `mod` 2 == 1 = Wall
      | i == 4 && j > 2 && j < 8  = Wall
      | i == 5 && j > 2 && j < 8 && j `mod` 2 == 1 = Wall
      -- V
      | i == 7 && j > 3 && j < 8 = Door CBlue
      | i == 8 && j == 3 = Door CBlue
      | i == 9 && j > 3 && j < 8 = Door CBlue
      -- A
      | i == -8 && j > -8 && j < -3 = Wall
      | i == -7 && (j == -6 || j == -3) = Wall
      | i == -6 && j > -8 && j < -3 = Wall
      -- I
      | i == -4 && j > -8 && j < -2 = Wall
      -- D
      | i == -2 && j > -8 && j < -2 = Wall
      | i == -1 && (j == -3 || j == -7) = Wall
      | i == 0 && j > -7 && j < -3 = Wall
      -- A
      | i == 2 && j > -8 && j < -3 = Wall
      | i == 3 && (j == -6 || j == -3) = Wall
      | i == 4 && j > -8 && j < -3 = Wall
      -- R
      | i == 6 && j > -8 && j < -2 = Wall
      | i == 7 && (j == -3 || j == -5) = Wall
      | i == 8 && j == -4 = Wall
      | i == 8 && j > -8 && j < -5 = Wall
      --
      | i == 9 && j == -8 = Door CWhite
      | i == 9 && j == -7 = Button CWhite
      | i == -9 && j == -8 = Door CBlack
      | abs i == 5 && j == -8 = Door CBlack
      | i == -3 && j == -8 = Door CGray
      | i == 9 && j == -9 = Button CRed
      | i == 4 && j == -9 = Door CRed
      | i == -9 && j == -9 = Button CGreen
      | i == 1 && j == -7 = Door CGreen
      | i == -2 && j == -8 = Button CDarkGreen
      | i == 1 && j == -6 = Door CDarkGreen
      | i == -2 && j == -9 = Door CDarkGreen
      --
      | i > 3 && j == -1 = Door CBlack
      | i < -3 && j == -1 = Door CBlack
      | i == -3 && j == -2 = Door CRed
      | i == 4 && j == 0 = Door CBrown
      | i == -9 && j == -7 = Button CBrown
      | i == 4 && j == -2 = Door CPurple
      | i == -8 && j == 1 = Button CPurple
      --
      | i == 6 && j == 5 = Door CRed
      | i == 6 && j == 3 = Door CDarkGreen
      | i == 6 && j == 7 = Door CBrown
      | i == 4 && j == 2 = Door CLightBlue
      | i == -8 && j == 4 = Button CLightBlue
      | i == -6 && j == 3 = Door CDarkRed
      | i == 8 && j == 1 = Button CDarkRed
      | i == 0 && j == 8 = Door CGreen
      | i == 0 && j == 7 = Button CGray
      | i == 1 && j == -7 = Door CGreen
      | i == 3 && j > 7 = Door CBlack
      | i == -3 && j > 7 = Door CBlack
      | i == 6 && j == 8 = Button CBlack
      | i == 8 && j == 4 = Exit
      | otherwise = Floor

-- | Author: Boris Guryev
level12 :: Level
level12 = Level (Coords (-7) (-7)) levelMap []
  where
    levelMap (Coords i j)
    -- x  &&  y
      | abs i > 9 || abs j > 9 = Wall  -- walls around map
      | i == 0  && j == 0  = Exit

      | i == -7 && j == -3 = Door CBlue
      | i == -7 && j == -4 = Button CBlue

      | i == -7 && j == 3 = Button CYellow
      | i == -7 && j == 4 = Door CYellow

      | i == -4 && j == 7 = Button COrange
      | i == -3 && j == 7 = Door COrange

      | i == 3 && j == 7 = Button CBlack
      | i == 4 && j == 7 = Door CBlack

      | i == 7 && j == 3 = Door CBlue
      | i == 7 && j == 4 = Button CBlue

      | i == 7 && j == -3 = Button CYellow
      | i == 7 && j == -4 = Door CYellow

      | i == 4 && j == -7 = Button CGreen
      | i == 3 && j == -7 = Door CGreen

      | i == 0  && j == -7 = Button CRed
      | i == -3 && j == -7 = Door CRed
      | i == -4 && j == -7 = Door CRed
      | i == -3 && j == 0 = Door CRed
      | i == -4 && j == 0 = Door CRed

      | i == -6 && j >= -2 && j <= 2 = Door CRed

      | i == -3            = Wall
      | i == -4            = Wall
      | i == 4             = Wall
      | i == 3             = Wall
      |            j == -3 = Wall
      |            j == -4 = Wall
      |            j == 4  = Wall
      |            j == 3  = Wall
      | otherwise = Floor -- floor otherwise

-- | Author: Mike Kuskov
level13 :: Level
level13 = Level (Coords 0 0) levelMap []
  where
    levelMap (Coords i j)
      | abs i > 9 || abs j > 9 = Wall  -- walls around map
      | trigger 0 7  = Exit
      | trigger 0 0  = Floor
      | trigger 0 1 = Door CPink
      | trigger (-6) 0 = Button CPink
      | trigger (-2) 0 = Door CBlue
      | trigger (-4) 0 = Door CBlue
      | trigger 0 (-2) = Door CGreen
      | trigger 2 0 = Door CRed
      | trigger (-1) 0 = Button CRed
      | trigger 0 (-1) = Button CBlue
      | trigger (-3) 0 = Button CBlue
      | trigger (1) 0 = Button CGreen
      | trigger (-6) (-6) = Door CRed
      | trigger (6) (-6) = Door CBlue
      | i == j       = Wall
      | -i == j      = Wall
      | i < 0 && j == 1 = Wall
      | -8 <= i && i < 0 && j == -1 = Wall
      | j == 6 && (-6) < i && i < 6 = Door CYellow
      | trigger (-4) 5 = Button CYellow
      | otherwise = Floor -- floor otherwise
      where
        trigger x y = (i == x) && (j == y)