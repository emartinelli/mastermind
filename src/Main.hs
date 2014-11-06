module Main where
    import System.Random

    gen_lst = [[w, x, y, z] | w <- [1..7], x <- [1..7], y <- [1..7], z <- [1..7], w /= x, w /= y, w /= z, x /= y, x /= z, y /= z]
    len_lst = length gen_lst

    randomNumber = random (mkStdGen 100) :: (Int, StdGen)
    list = gen_lst !! (fst randomNumber)
    main = print(list)

