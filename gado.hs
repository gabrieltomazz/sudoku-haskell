rainhaDiagonal :: Int -> (Int,Int) -> Bool
rainhaDiagonal col (x,y) = abs (col - y) == x

--coluna da linha atual
--cols colunas preenchidas
rainhaDiagonais :: Int -> [Int] -> Bool
rainhaDiagonais x cols = 
  let pontos = zip [1..] cols 
  in any (rainhaDiagonal x) pontos


--Já possui rainha na coluna atual?
rainhaColuna :: Int -> [Int] -> Bool
rainhaColuna x col = x `elem` col

--Verifica se a coluna atual é válida,
--A linha é dada pelo tamanho das colunas já ocupadas
posicaoValida :: Int -> [Int] -> Bool
posicaoValida x xs = not (rainhaColuna x xs || rainhaDiagonais x xs)


--Retorna lista de rainhas no tabuleiro, 
--onde a posição é a linha e o valor é a coluna
--	n é o tamanho do tabuleiro
--	k é a linha atual
nRainhas' :: Int -> Int -> [[Int]]
nRainhas' _ 0 = [[]]
nRainhas' n k = [x:xs | xs <- nRainhas' n (k-1), x <- [1..n], posicaoValida x xs]

nRainhas :: Int -> [[Int]]
nRainhas n =  nRainhas' n n

main = do
  print (nRainhas 15)