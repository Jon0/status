import System.Environment

data TestEnum = Foo | Bar | Baz
xs = [1, 2, 3, 4]

double_func :: Int -> Int
double_func x = x + x

some_func :: Int -> Int -> Int -> Int
some_func a b c = a * 2 + b * 3 + c


-- | 'main' runs the main program
main :: IO ()
main = getArgs >>= print . test_str_func . head

test_str_func :: [Char] -> [Char]
test_str_func [] = "Empty"
test_str_func s = "Test " ++ take 1 s

-- let m = 3 + 3


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs


test_print :: [Char] -> IO ()
test_print s = do
    let r = "Test " ++ s
    putStrLn (r)
