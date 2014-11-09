module Main where
    import Data.List

    genLsts:: [[Integer]]
    genLsts = [[w, x, y, z] | w <- [1..7], x <- [1..7], y <- [1..7], z <- [1..7], w /= x, w /= y, w /= z, x /= y, x /= z, y /= z]

    getTheSecret :: Int -> [Integer]
    getTheSecret index = genLsts !! index

    nLstOcurrences :: [Integer] -> [Integer] -> Integer
    nLstOcurrences lst lst2 = aux lst lst2 0
    aux _ [] ac = ac
    aux lst (a:x) ac
                    |a elem lst = 1+ac

    nLstOcurrencesAtSamePos :: [Integer] -> [Integer] -> Integer
    nLstOcurrencesAtSamePos lst lst2 = aux2 lst lst2 0
    aux2 _ [] ac = ac
    aux2 lst (a:x) ac
                    |elemIndex a lst = 1+ac

    putPins :: [Integer] -> [Integer] -> [Char]
    putPins _ [] = []
    putPins secret input
                        |input == secret = ['p','p','p','p']
                        |otherwise = ['v','v','v','v']

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

    gameInput :: String -> IO ()
    gameInput input = do
        let maybeList = getListFromString input in
            case maybeList of
                Just l  -> print (sum l)
                Nothing -> error formatError

    main :: IO ()
    main = do
        putStrLn welcome
        putStrLn instructions
        game