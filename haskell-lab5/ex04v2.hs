import System.Environment

main :: IO ()
main = do
  (inFileName:outFileName:_) <- getArgs
  inpStr <- readFile inFileName
  writeFile outFileName inpStr