module Main where

    genLsts:: [[Integer]]
    genLsts = [[w, x, y, z] | w <- [1..7], x <- [1..7], y <- [1..7], z <- [1..7], w /= x, w /= y, w /= z, x /= y, x /= z, y /= z]

    getTheSecret :: Int -> [Integer]
    getTheSecret index = genLsts !! index

    isMyAnswerCorrect :: [Int] -> [Int] -> [Char]
    isMyAnswerCorrect _ [] = []
    isMyAnswerCorrect secret input
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