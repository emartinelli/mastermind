module Main where
    import Data.List
    import Data.Char (digitToInt)

    genLsts:: [[Int]]
    genLsts = [[w, x, y, z] | w <- [1..7], x <- [1..7], y <- [1..7], z <- [1..7], w /= x, w /= y, w /= z, x /= y, x /= z, y /= z]

    getTheSecret :: Int -> [Int]
    getTheSecret index = genLsts !! index

    nLstOcurrences :: [Int] -> [Int] -> Int
    aux :: [Int] -> [Int] -> Int -> Int
    nLstOcurrences lst lst2 = aux lst lst2 0
    aux _ [] ac = ac
    aux lst (a:x) ac
                    |elem a lst = 1+ac
                    |otherwise = aux lst x ac

    nLstOcurrencesAtSamePos :: [Int] -> [Int] -> Int
    aux2 :: [Int] -> [Int] -> Int -> Int
    nLstOcurrencesAtSamePos lst lst2 = aux2 lst lst2 0
    aux2 _ [] ac = ac
    aux2 lst lst2@(a:x) ac
                    |elemIndex a lst == elemIndex a lst2 = 1+ac
                    |otherwise = aux2 lst x a

    returnPins :: Int -> Int -> String
    returnPins 0 0 = "vvvv"
    returnPins blacks blacksPlusWhites = createString blacks 'p' ++
                                         createString (blacksPlusWhites - blacks) 'b' ++
                                         createString (4 - blacksPlusWhites) 'v'

    createString :: Int -> Char -> String
    createString 0 _  = []
    createString n _ |n < 0 = []
    createString n s = s : createString (n - 1) s

    putPins :: [Int] -> [Int] -> String
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

    gameTest :: [Int] -> [Int] -> IO ()
    gameTest secret input =
                            do
                                print secret
                                print input
                                print (putPins secret input)

    gameInput :: String -> IO ()
    gameInput input =
        let maybeList = getListFromString input in
            case maybeList of
                Just l  -> gameTest (getTheSecret 5) (map digitToInt input)
                Nothing -> error formatError

    main :: IO ()
    main = do
        putStrLn welcome
        putStrLn instructions
        game