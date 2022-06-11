module Main where

import qualified Data.Text as T
import Shelly
import System.Environment
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

import Lib

main :: IO ()
main = do
    istty <- queryTerminal stdInput
    args <- getArgs
    if istty then withPasteboard args else withStdin args
    where withStdin args = getContents >>= flip entry (unwords args)
          withPasteboard args = shelly $ silently $ do
                pb <- T.unpack <$> run "pbpaste" []
                liftIO $ entry (unwords args) pb
