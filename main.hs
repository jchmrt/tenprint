import Graphics.Gloss
import Data.Fixed
import Graphics.Gloss.Data.ViewPort
import Data.List
import Control.Monad.Random
import System.Random

bslash :: Float          -- ^ Thickness
       -> (Float, Float) -- ^ Width, height
       -> Picture
bslash t (w,h) = Polygon [(0,0),(t,0),(w,h-t),(w,h),(w-t,h),(0,t)]

fslash :: Float          -- ^ Thickness
       -> (Float, Float) -- ^ Width, height
       -> Picture
fslash t (w,h) = Polygon [(w,0),(w,t),(t,h),(0,h),(0,h-t),(w-t,0)]

render :: (Float,Float) -> Float -> [Picture] -> Picture
render (w,h) x ps =
  let maxX = x*w - w
      f (xs,(x,y)) p
        | x < maxX  = (translate (x+w) y p : xs, (x+w,y))
        | otherwise = (translate 0 (y+h) p : xs, (0,y+h))
  in pictures $ fst $ foldl' f ([],(-w,0)) ps

randList :: RandomGen g => Int -> [a] -> Rand g [a]
randList 0 _  = return []
randList n xs = do
  i <- getRandomR (0,length xs - 1)
  rest <- randList (n-1) xs
  return ((xs!!i):rest)

renderMaze :: MazeState -> Picture
renderMaze ms =
  let baseX = (-screenWidth ms / 2)
      baseY = (-screenHeight ms / 2)
  in translate (baseX - transX ms) (baseY - transY ms)
       $ render (slashWidth ms, slashHeight ms)
       (screenWidth ms / slashWidth ms) (slashes ms)

step :: ViewPort -> Float -> MazeState -> MazeState
step v t ms
  | transY ms > slashHeight ms = step v t (addRows ms)
  | otherwise = ms { transY = transY ms + 100 * t }

addRows :: MazeState -> MazeState
addRows ms =
  let needed = floor (transY ms / slashHeight ms
                      * (screenWidth ms / slashWidth ms))
      choices = map (\s -> color (slashColor ms)
                           $ s (slashThickness ms)
                               (slashWidth ms, slashHeight ms))
                    [fslash,bslash]
      (new,g') = runRand (randList needed choices) (gen ms)
  in ms { slashes = drop needed (slashes ms) ++ new
        , transY = transY ms `mod'` slashHeight ms
        , gen = g' }

data MazeState = MazeState
  { slashes :: [Picture]
  , transX :: Float
  , transY :: Float
  , screenWidth :: Float
  , screenHeight :: Float
  , slashWidth :: Float
  , slashHeight :: Float
  , slashThickness :: Float
  , slashColor :: Color
  , gen :: StdGen
  }

main :: IO ()
main = do
  g <- getStdGen
  let w = 1536; w' = fromIntegral w
      h = 960;  h' = fromIntegral h
      lx = w `div` n'
      ly = h `div` n'
      t = 12
      n = 48
      n' = round n
      bc = makeColor 0.15 0.4 1 1
      fc = makeColor 1 1 1 0.9
      sls = evalRand (randList (lx*ly*2)
               (map (\s -> color fc $ s t (n,n)) [fslash,bslash])) g
  simulate (InWindow "Float" (w,h) (0,0)) bc 60
    (MazeState sls 5 0 w' h' n n t fc g)
    renderMaze step
