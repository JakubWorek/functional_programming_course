import System.Environment
import qualified Data.ByteString as BStr

main :: IO ()
main = do
  (inFileName:outFileName:_) <- getArgs
  inpBStr <- BStr.readFile inFileName
  BStr.writeFile outFileName inpBStr