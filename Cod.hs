suma :: Int->Int->Int
suma x y = x + y

resta :: Int->Int->Int
resta x y = x - y

multi :: Int->Int->Int
multi x y = if x == 1 then y
            else suma y (multi (resta x 1) y)

division :: Int->Int->Int
division x y = if x < x then 0
               else suma 1 (division (resta x y) y)

modulo :: Int->Int->Int
modulo x y = if x < y then x
             else modulo (resta x y) y

binario :: Int->Int
binario x = if (division x 2) == 0 then x
            else suma (modulo x 2) (multi 10 (binario (division x 2)))

decimal :: Int->Int
decimal x = if (division x 10) == 0 then x
            else suma (modulo x 10) (multi 2 (decimal (division x 10)))
            
palindromo::Int->Bool
palindromo x = comparar x (invertir x)

comparar::Int->Int->Bool
comparar x y = (if x == y then True else False)			

contar :: Int -> Int
contar x | x < 10 = 1
         | otherwise = 1 + contar (division x 10)
		 
invertir::Int->Int
invertir x | x < 10 = x
           | otherwise = ((modulo x 10)*10^((contar x)-1) + (invertir (division x 10)))
