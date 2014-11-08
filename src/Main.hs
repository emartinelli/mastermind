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

    decreaseTries tries = tries-1

    getListFromString :: String -> Maybe [Integer]
    getListFromString str = maybeRead $ "[" ++ str ++ "]"

    initialPrint :: String
    initialPrint = "Let's play a game!"

    inMsg :: String
    inMsg = "Enter with a sequence of 4 distinct numbers form 1 to 7 (separated by comma). Let's try!"

    main = do
        putStrLn initialPrint
        putStrLn inMsg
        input <- getLine
        let maybeList = getListFromString input in
            case maybeList of
            Just l  -> print (sum l)
            Nothing -> error "Bad format. Good Bye."
