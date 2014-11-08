module Main where
    import System.Random
    import Data.Maybe

    genLst = [[w, x, y, z] | w <- [1..7], x <- [1..7], y <- [1..7], z <- [1..7], w /= x, w /= y, w /= z, x /= y, x /= z, y /= z]
    lenLst = length genLst
    list = genLst !! 5

    maybeRead :: Read a => String -> Maybe a
    maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing

    getListFromString :: String -> Maybe [Integer]
    getListFromString str = maybeRead $ "[" ++ str ++ "]"

    welcome :: String
    welcome = "Let's play a game!"

    instructions :: String
    instructions = "Enter with a sequence of 4 distinct numbers form 1 to 7 (separated by comma). Let's try!"

    formatError :: String
    formatError = "Bad format. Good Bye."

    game :: IO ()
    game = do
        gameInput

    gameInput :: IO ()
    gameInput = do
        input <- getLine
        let maybeList = getListFromString input in
            case maybeList of
                Just l  -> print (sum l)
                Nothing -> error formatError

    main :: IO ()
    main = do
        putStrLn welcome
        putStrLn instructions
        game