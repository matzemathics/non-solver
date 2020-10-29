module Main where

import Control.Monad
import Data.Functor
import NonParser
  ( NonFile (nf_cols, nf_height, nf_rows, nf_width),
    parseNonFile,
  )
import Nonogram
import System.Environment (getArgs)
import UI.NCurses

main :: IO ()
main = do
  (fileName : _) <- getArgs
  file <- readFile fileName
  let task = parseNonFile fileName file
  case task of
    Left e -> print e
    Right (Left e) -> print e
    Right (Right n) -> do
      let t = Task (map fst <$> nf_rows n) (map fst <$> nf_cols n)
      renderSolving t (nf_height n, nf_width n)

nonoWindow w h = newWindow (fromIntegral h + 3) width 0 0
  where
    width = max 50 $ fromIntegral w * 3 + 3

renderSolving :: Task -> (Int, Int) -> IO ()
renderSolving t (h, w) = do
  runCurses $ do
    setEcho False
    wnd <- nonoWindow w h
    printStep wnd t (h, w) $ emptyBoard w h
  return ()

printStep wnd t (h, w) c =
  case step t c of
    Nothing -> noSolution wnd
    Just (f, b) -> do
      updateWindow wnd $ do
        forM_
          (zip [0 ..] $ printer f)
          ( \(i, str) -> do
              moveCursor i 1
              drawString str
          )
        moveCursor (fromIntegral h) 3
        drawString $
          "(press "
            <> (if b then "" else "n to step or ")
            <> "q to quit..)"
      render
      cmd <- handleKeys wnd b
      case cmd of
        Quit -> return ()
        Step -> printStep wnd t (h, w) f

noSolution :: Window -> Curses ()
noSolution w = do
  updateWindow w $ do
    (h, _) <- windowSize
    moveCursor 1 3
    drawString "Couldn't find a Solution :("
    moveCursor h 3
    drawString "(press q to quit...)"
  render
  handleKeys w True $> ()

data Cmd = Step | Quit

handleKeys :: Window -> Bool -> Curses Cmd
handleKeys w fin = do
  ev <- getEvent w Nothing
  case ev of
    Nothing -> handleKeys w fin
    Just k -> case (fin, cmd k) of
      (_, Nothing) -> handleKeys w fin
      (_, Just Quit) -> return Quit
      (True, _) -> handleKeys w fin
      (_, Just s) -> return s

cmd :: Event -> Maybe Cmd
cmd k
  | k == EventCharacter 'q'
      || k == EventCharacter 'Q' =
    Just Quit
  | k == EventSpecialKey KeyRightArrow
      || k == EventSpecialKey KeyEnter
      || k == EventCharacter 'n'
      || k == EventCharacter 'N' =
    Just Step
cmd _ = Nothing