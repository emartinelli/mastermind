--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
-- Authors: Elvio Martinelli
--          Segio Candeas

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Main where
    import Data.List
    import Data.Char
    import System.IO.Unsafe
    import System.Random

-- Cria uma lista de listas que possuem todas as combinações possiveis de 4 elementos de 1 à 7 sem repetição
    genLsts:: [[Int]]
    genLsts = [[w, x, y, z] | w <- [1..7], x <- [1..7], y <- [1..7], z <- [1..7], w /= x, w /= y, w /= z, x /= y, x /= z, y /= z]

-- Função que pega uma combinação
    getTheSecret :: Int -> [Int]
    getTheSecret index = genLsts !! index

-- Função que retorna o numero de pinos que o usuario acertou nao importando se foi na posicao correta
    nLstOcurrences :: [Int] -> [Int] -> Int
    aux :: [Int] -> [Int] -> Int -> Int
    nLstOcurrences lst lst2 = aux lst lst2 0
    aux _ [] ac = ac
    aux lst (a:x) ac
                    |elem a lst = aux lst x (ac + 1)
                    |otherwise = aux lst x ac

-- Função que retorna o numero de pinos que o usuario acertou importando se foi na posicao correta
    nLstOcurrencesAtSamePos :: [Int] -> [Int] -> Int
    aux2 :: [Int] -> [Int] -> [Int] -> Int -> Int
    nLstOcurrencesAtSamePos lst lst2 = aux2 lst lst2 lst2 0
    aux2 _ _ [] ac = ac
    aux2 lst lst2 (a:x) ac
                    |elemIndices a lst == elemIndices a lst2 = aux2 lst lst2 x (ac + 1)
                    |otherwise = aux2 lst lst2 x ac

-- Retorna uma string com os pinos pretos, brancos e vazios, nao mostra na ordem que estao os pinos na senha correta
    returnPins :: Int -> Int -> String
    returnPins 0 0 = "vvvv"
    returnPins blacks blacksPlusWhites = createString blacks 'p' ++
                                         createString (blacksPlusWhites - blacks) 'b' ++
                                         createString (4 - blacksPlusWhites) 'v'

-- Cria uma String com caracteres iguais
    createString :: Int -> Char -> String
    createString 0 _  = []
    createString n _ |n < 0 = []
    createString n s = s : createString (n - 1) s

-- Compara o segredo e a entrada do usuário. Retorna os pinos correspondentes
    putPins :: [Int] -> [Int] -> String
    putPins _ [] = []
    putPins secret input
                        |input == secret = "pppp"
                        |otherwise = returnPins (nLstOcurrencesAtSamePos secret input) (nLstOcurrences secret input)

-- Tratamento de entrada
    maybeRead :: Read a => String -> Maybe a
    maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing

-- Tratamento de entrada
    getListFromString :: String -> Maybe [Int]
    getListFromString str = maybeRead $ "[" ++ str ++ "]"

    welcome :: String
    welcome = "Let's play a game!"

    instructions :: String
    instructions = "Enter with a sequence of 4 distinct numbers from 1 to 7 (separated by comma). Let's try!"

    formatError :: String
    formatError = "Bad format. Good Bye."

    loserMessage :: String
    loserMessage = "You lose! :/"

    winnerMessage :: String
    winnerMessage = "You win! \\o/"

    game :: [Int] -> IO ()
    game ran = do
            gameInput ran 10
            
-- Verifica se o usuario acertou a combinação ou não, e se não acertou mostra quantas tentativas faltam
    gameTest :: [Int] -> [Int] -> Int -> IO ()
    gameTest secret input count =
                            let pins = putPins secret input in
                                if count < 2 then
                                    print loserMessage
                                else if pins == "pppp" then
                                    print winnerMessage
                                else
                                    do
                                        print ("Try again! You have more "++ [intToDigit (count-1)] ++ " tries. Tip: "++ pins)
                                        gameInput secret (count-1)

-- Tratamento de entrada
    gameInput :: [Int] -> Int -> IO ()
    gameInput rand count = do
        input <- getLine
        let maybeList = getListFromString input in
            case maybeList of
                Just l  -> (gameTest rand l count)
                Nothing -> error formatError

-- Função Principal
    main :: IO ()
    main = do
        putStrLn welcome
        putStrLn instructions
        game (getTheSecret (unsafePerformIO (getStdRandom (randomR (0, length genLsts)))))
