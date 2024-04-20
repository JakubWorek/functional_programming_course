import System.Environment
import System.IO.Error
import Control.Exception

riskyAction :: IO ()
riskyAction = do (fileName:_) <- getArgs
                 contents <- readFile fileName
                 putStrLn contents

exHdlr :: IOError -> IO ()
exHdlr = \ex -> if isDoesNotExistError ex
                then putStrLn "The file doesn't exist!"
                else ioError ex

main :: IO ()
main = do
  result <- try riskyAction
  case result of
    Left ex -> exHdlr ex
    Right _ -> putStrLn "Operation completed"