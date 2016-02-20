import System.Environment
import System.IO

main :: IO ()
main = do
    hGetChar stdin
    print "hello\n"
