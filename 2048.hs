{-# LANGUAGE FlexibleContexts #-}
import Prelude hiding (succ)
import Control.Lens
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Word
import System.IO

newRange :: (Word8, Word8)
newRange = (1,2)
rows, cols, cellWidth :: Int
rows = 4
cols = 4
cellWidth = 5
cellSep :: String
cellSep = " | "

type Cell   = Word8
type Line   = [Cell]
type Board  = [Line]
type Keymap = [(Char, Dir)]

runGame :: (RandomGen g, Traversable t) => [(Char, Dir)] -> Board -> g -> t Char -> [Char]
runGame km s g ls = execWriter (evalRandT (evalStateT (addRndCell >> dispBoard >> mapM (interaction km) ls) s) g)

addRndCell :: (MonadState Board m, MonadRandom m) => m ()
addRndCell =
  do b <- get
     let cs = freeCells b
     when (null cs) (error "Game over")
     (i,j) <- (cs !!) <$> getRandomR (0,length cs - 1)
     e <- getRandomR newRange
     put (b & ix i . ix j .~ e)

interaction :: (MonadState Board m, MonadWriter String m, MonadRandom m) => Keymap -> Char -> m ()
interaction km c | Just d <- lookup c km =
  do changed <- modifyC (applyDir d move)
  -- Need to improve the end of game condition
     when changed addRndCell
     dispBoard
interaction _  c = tell $ "Unexpected character  " ++ show c ++ "\n"

freeCells :: Board -> [(Int,Int)]
freeCells = concat . concat . zipWith f [0..] where
  f i = zipWith (g i) [0..]
  g i j x | x == 0    = [(i,j)]
          | otherwise = []

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          hSetEcho      stdin False
          keymap    <- mapM pick [("Up",N),("Down",S),("Right",E),("Left",W)]
          randomGen <- getStdGen
          interact $ runGame keymap initBoard randomGen
  where
    pick (s, d) = do
      putStrLn $ "Pick key for " ++ s
      c <- getChar
      pure (c, d)
    initBoard = replicate rows (replicate cols 0)

dispBoard :: (MonadState Board m, MonadWriter String m) => m ()
dispBoard = tell =<< showBoard <$> get
  where
    showBoard = ("\ESC[2J" ++) . (++ "\n") . (++ ln) . (ln ++) . intercalate ln . map showLine
    showLine = (++ " |") . ("| " ++) . intercalate cellSep . map showCell
    showCell = padL ' ' cellWidth . replace "1" "" . show . ((2 :: Integer)^)
    ln = "\n+" ++ replicate ((cellWidth + length cellSep) * cols - 1) '-' ++ "+\n"

class Nat a where
  zero :: a
  succ :: a -> a

-- This function implements the core mechanics of the game
(.:) :: (Nat a, Eq a) => a -> [a] -> [a]
x .: xs     | x == zero =          xs ++ [zero]
x .: (y:xs) | x == y    = succ x : xs ++ [zero]
x .: xs                 =      x : xs

move :: (Nat a, Eq a) => [a] -> [a]
move = foldr (.:) []

padR :: a -> Int -> [a] -> [a]
padR p n xs = take n (xs ++ repeat p)

padL :: a -> Int -> [a] -> [a]
padL p n = reverse . padR p n . reverse

replace :: Eq a => a -> a -> a -> a
replace x y z | x == z    = y
              | otherwise = z

modifyC :: (Eq s, MonadState s m) => (s -> s) -> m Bool
modifyC f = do s <- get
               let s' = f s
               put s'
               return (s /= s')

data Dir = N | S | E | W

applyDir :: Dir -> ([a] -> [a]) -> [[a]] -> [[a]]
applyDir N f = transpose . applyDir W f . transpose
applyDir S f = transpose . applyDir E f . transpose
applyDir E f = map (reverse . f . reverse)
applyDir W f = map f

instance Nat Word8 where
  zero = 0
  succ = (+) 1
