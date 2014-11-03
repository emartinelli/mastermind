gerar n = gen 4
    where gen 0 = [[]]
          gen k = [a: x | a <- [1..7], x <- gen (k-1)]


main = print(gerar 2)

