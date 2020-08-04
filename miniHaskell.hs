doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

--usando if
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
locardi x1 y1 = [x * y | x <- x1, y <- y1] 

boomBang xs = [ if x `rem` 2 == 0 then "raka" else "taka"| x <- xs]  

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

sayMe :: (Integral a) => a -> String
sayMe 1 = "Uno!"
sayMe 2 = "Dos!"
sayMe 3 = "Tres!"
sayMe 4 = "Cuatro!"
sayMe 5 = "Cinco!"
sayMe x = "No entre uno 1 y 5"

--Ejemplo de recursión
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


--pattern machine
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName x = "No hay nombres para esta letra"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "¡Hey, no puedes utilizar head con una lista vacía!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell []       = "La lista está vacía"
tell (x:[])   = "La lista tiene un elemento: " ++ show x
tell (x:y:[]) = "La lista tiene dos elementos: " ++ show x ++ " y " ++ show y
tell (x:y:_)  = "La lista es larga. Los primeros dos elementos son: " ++ show x ++ " y " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "¡Una cadena vacía!"
capital all@(x:_) = "La primera letra de " ++ all ++ " es " ++ [x]

--Guardas en Haskell
{-bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Tienes infrapeso ¿Eres emo?"
    | bmi <= 25.0 = "Supuestamente eres normal... Espero que seas feo."
    | bmi <= 30.0 = "¡Estás gordo! Pierde algo de peso gordito."
    | otherwise   = "¡Enhorabuena, eres una ballena!"
-}
--usando where
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Tienes infrapeso ¿Eres emo?"
    | bmi <= normal = "Supuestamente eres normal... Espero que seas feo."
    | bmi <= fat    = "¡Estás gordo! Pierde algo de peso gordito."
    | otherwise     = "¡Enhorabuena, eres una ballena!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

--Mumuki--
{-seCompara :: (Eq a, Num a) => a -> Bool
seCompara x = (x==0)
esMultiploDe = seCompara.rem-}

{-calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2-}

--usando let in
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]


