{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Nonogram
  ( emptyBoard,
    solve,
    step,
    printer,
    Task (Task),
    NonoStep (),
  )
where

import Control.Applicative
import Control.Monad hiding (join)
import Data.List hiding (null)
import qualified Data.List (null)
import Data.Monoid
import Prelude hiding (null)

isNull :: Maybe a -> Bool
isNull = Data.List.null

data Task = Task
  { rows :: [[Int]],
    cols :: [[Int]]
  }
  deriving (Show, Eq)

data Field
  = X
  | O
  | P
  deriving (Show, Eq, Read, Enum)

class NonoAttr a where
  isOpen :: a -> Bool
  isSep :: a -> Bool
  isBlock :: a -> Bool

instance NonoAttr Field where
  isOpen = (== O)
  isSep = (== X)
  isBlock = (== P)

data NonState = NonSt
  { st_rows :: [([Int], Maybe [[Field]])],
    st_cols :: [([Int], Maybe [[Field]])]
  }

data NonoStep

emptyBoard :: Integral a => a -> a -> [[Field]]
emptyBoard w h =
  replicate (fromIntegral h) $
    replicate (fromIntegral w) O

solve :: Task -> [[Field]] -> Maybe [[Field]]
solve t@Task {rows, cols} fs
  | isNull $ find (elem O) fs = return fs
  | otherwise = do
    f' <- forM (zip rows fs) $ uncurry update
    f'' <- forM (zip cols $ transpose f') $ uncurry update
    solve t $ transpose f''

step :: Task -> [[Field]] -> Maybe ([[Field]], Bool)
step Task {rows, cols} fs
  | isNull $ find (elem O) fs = return (fs, True)
  | otherwise = do
    f' <- forM (zip rows fs) $ uncurry update
    f'' <- forM (zip cols $ transpose f') $ uncurry update
    return (transpose f'', isNull $ find (elem O) f'')

update :: [Int] -> [Field] -> Maybe [Field]
update task f = merge . knowing f $ possibilities task (length f)

possibilities :: [Int] -> Int -> [[Field]]
possibilities [] n = [replicate n X]
possibilities [x] n | x == n = [replicate x P]
possibilities (x : xs) n
  | x <= n =
    (possibilities xs (n - x - 1) >>= \s -> [replicate x P ++ X : s])
      ++ ((X :) <$> possibilities (x : xs) (n - 1))
possibilities _ _ = []

numPossibilities :: [Int] -> Int -> Int
numPossibilities hints width = run (length hints) $ width - sum (intersperse 1 hints)
  where
    run _ 0 = 1
    run 1 b = b + 1
    run a b = run (a - 1) b + run a (b - 1)

knowing :: [Field] -> [[Field]] -> [[Field]]
knowing fs = filter (and . zipWith (\a b -> a == b || isOpen a) fs)

merge :: [[Field]] -> Maybe [Field]
merge [] = Nothing
merge fs = return $ sel . nub <$> transpose fs
  where
    sel [a] = a
    sel _ = O

printer :: [[Field]] -> [String]
printer = map (unwords . map (\case P -> "#"; _ -> " "))
