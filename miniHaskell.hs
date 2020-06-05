doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
locardi x1 y1 = [x * y | x <- x1, y <- y1] 
boomBang xs = [ if x `rem` 2 == 0 then "raka" else "taka"| x <- xs]    
length' xs = sum [1 | _ <- xs]
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
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)