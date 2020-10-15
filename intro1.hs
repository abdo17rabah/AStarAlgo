f :: Int->Int
f x = x+2

g :: Int->Int
g x = x-5

carre :: Int->Int
carre x = x*x

sumSquares :: Int->Int->Int
sumSquares x y = carre x + carre y

maxi :: Int->Int->Int
maxi x y = if(x>=y) then x else y

sumSquaresMax :: Int->Int->Int->Int
sumSquaresMax x y z = if(maxi (maxi x y) z ==x) then sumSquares  x (maxi y z) else if (maxi (maxi x y) z ==y) then sumSquares  y (maxi x z) else sumSquares  z (maxi x y)


f2c :: Float -> Float
f2c f= 5/9 *(f-32)

travelExpenses :: Float->Float->Float
travelExpenses km nbp = if(nbp >=11) then nbp*(km*(0.52-((0.52*75)/100))) else if (nbp >=5) then nbp*(km*(0.52-((0.52*50)/100))) else if (nbp >=2) then nbp*(km*(0.52-((0.52*25)/100))) else nbp*km*0.52

decode :: Int -> Char
decode n = toEnum n

code :: Char -> Int
code c = fromEnum c

nextUpperCase :: Char -> Char
nextUpperCase c= if (c=='Z') then 'A' else decode(code c +1)

collatz :: Int -> Int
collatz n = if(n<2) then -1 else if(n==2) then 1 else if (mod n 2==0) then div n 2 else 3*n+1

nbCalls :: Int->Int
nbCalls n= if (collatz n == 1) then 1 else 1+ nbCalls (collatz n)

syracuse :: Int -> [Int]
syracuse n = if(n<2) then [] else [n] ++ syracuse (collatz n)


allEven :: [Int]->Bool
allEven l = if (length l ==0) then True else if (mod (head l) 2 ==0 && length l >0) then allEven (tail l) == True else False


laugh :: Int->String
laugh n = concat (replicate n " Ha ")

double :: [String]->String
double l =  if(l==[]) then " " else concat(replicate 2 (head l ++ " ")) ++double (tail l)


myZip :: [a]->[b]->[(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (xx:xs) (yy:ys) = (xx,yy):(myZip xs ys)


sumSquareEven :: Int -> Int
sumSquareEven n = sum[x*x| x<-[1..(2*n)],even x]

internalSplit :: [a] -> Int -> [a] -> [[a]]
internalSplit (first:rest) count firstPart
    | count == 0 = [firstPart, (first:rest)]
    | otherwise  = internalSplit rest (count - 1) (firstPart ++ [first])

split :: [a] -> [[a]]
split myList =
    let listLength = length myList
    in
        if mod listLength 2 == 0 then
            internalSplit myList (div listLength 2) []
        else
            internalSplit myList ((div listLength 2) + 1) []


(x:y:z)=(x:xp):(y:yp) where (xp:yp) = z









