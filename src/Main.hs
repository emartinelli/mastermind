module Main where
    import Data.List
    import Data.Char
    import System.IO.Unsafe
    import System.Random

    genLsts:: [[Int]]
    genLsts = [[w, x, y, z] | w <- [1..7], x <- [1..7], y <- [1..7], z <- [1..7], w /= x, w /= y, w /= z, x /= y, x /= z, y /= z]

    getTheSecret :: Int -> [Int]
    getTheSecret index = genLsts !! index

    nLstOcurrences :: [Int] -> [Int] -> Int
    aux :: [Int] -> [Int] -> Int -> Int
    nLstOcurrences lst lst2 = aux lst lst2 0
    aux _ [] ac = ac
    aux lst (a:x) ac
                    |elem a lst = aux lst x (ac + 1)
                    |otherwise = aux lst x ac

    nLstOcurrencesAtSamePos :: [Int] -> [Int] -> Int
    aux2 :: [Int] -> [Int] -> [Int] -> Int -> Int
    nLstOcurrencesAtSamePos lst lst2 = aux2 lst lst2 lst2 0
    aux2 _ _ [] ac = ac
    aux2 lst lst2 (a:x) ac
                    |elemIndices a lst == elemIndices a lst2 = aux2 lst lst2 x (ac + 1)
                    |otherwise = aux2 lst lst2 x ac

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

    getListFromString :: String -> Maybe [Int]
    getListFromString str = maybeRead $ "[" ++ str ++ "]"

    welcome :: String
    welcome = "Let's play a game!"

    instructions :: String
    instructions = "Enter with a sequence of 4 distinct numbers from 1 to 7 (separated by comma). Let's try!"

    formatError :: String
    formatError = "Bad format. Good Bye."

    game :: [Int] -> IO ()
    game ran = do
            gameInput ran 10

    gameTest :: [Int] -> [Int] -> Int -> IO ()
    gameTest secret input count =
                            let pins = putPins secret input in
                                if count < 1 then
                                    print "You lost! :/"
                                else if pins == "pppp" then
                                    print "You win!"
                                else
                                    do
                                        print ("Try again! You have more "++ [intToDigit count] ++ " tries. Tip: "++ pins)
                                        print (secret)
                                        gameInput secret (count-1)

    gameInput :: [Int] -> Int -> IO ()
    gameInput rand count = do
        input <- getLine
        let maybeList = getListFromString input in
            case maybeList of
                Just l  -> (gameTest rand l count)
                Nothing -> error formatError

    main :: IO ()
    main = do
        putStrLn welcome
        putStrLn instructions
        game (getTheSecret (unsafePerformIO (getStdRandom (randomR (0, length genLsts)))))