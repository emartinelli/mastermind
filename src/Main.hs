module Main where
    import Data.List

    genLsts:: [[Integer]]
    genLsts = [[w, x, y, z] | w <- [1..7], x <- [1..7], y <- [1..7], z <- [1..7], w /= x, w /= y, w /= z, x /= y, x /= z, y /= z]

    getTheSecret :: Int -> [Integer]
    getTheSecret index = genLsts !! index

    nLstOcurrences :: [Integer] -> [Integer] -> Integer
    aux :: [Integer] -> [Integer] -> Integer -> Integer
    nLstOcurrences lst lst2 = aux lst lst2 0
    aux _ [] ac = ac
    aux lst (a:x) ac
                    |elem a lst = 1+ac
                    |otherwise = aux lst x ac

    nLstOcurrencesAtSamePos :: [Integer] -> [Integer] -> Integer
    aux2 :: [Integer] -> [Integer] -> Integer -> Integer
    nLstOcurrencesAtSamePos lst lst2 = aux2 lst lst2 0
    aux2 _ [] ac = ac
    aux2 lst lst2@(a:x) ac
                    |elemIndex a lst == elemIndex a lst2 = 1+ac
                    |otherwise = aux2 lst x a

    returnPins :: Integer -> Integer -> String
    returnPins 0 0 = "vvvv"
    returnPins blacks blacksPlusWhites = createString blacks 'p' ++
                                         createString (blacksPlusWhites - blacks) 'b' ++
                                         createString (4 - blacksPlusWhites) 'v'

    createString :: Integer -> Char -> String
    createString 0 _  = []
    createString n _ |n < 0 = []
    createString n s = s : createString (n - 1) s

    putPins :: [Integer] -> [Integer] -> String
    putPins _ [] = []
    putPins secret input
                        |input == secret = "pppp"
                        |otherwise = returnPins (nLstOcurrencesAtSamePos secret input) (nLstOcurrences secret input)

    maybeRead :: Read a => String -> Maybe a
    maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing

    getListFromString :: String -> Maybe [Integer]
    getListFromString str = maybeRead $ "[" ++ str ++ "]"

    welcome :: String
    welcome = "Let's play a game!"

    instructions :: String
    instructions = "Enter with a sequence of 4 distinct numbers from 1 to 7 (separated by comma). Let's try!"

    formatError :: String
    formatError = "Bad format. Good Bye."

    game :: IO ()
    game = do
        input <- getLine
        gameInput input

    gameTest :: [Integer] -> [Integer] -> IO ()
    gameTest 1 0 = print 1

    gameInput :: String -> IO ()
    gameInput input =
        let maybeList = getListFromString input in
            case maybeList of
                Just l  -> gameTest (getTheSecret 5) input
                Nothing -> error formatError

    main :: IO ()
    main = do
        putStrLn welcome
        putStrLn instructions
        game