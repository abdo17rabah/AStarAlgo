type Facteur = Int
type Exposant = Int
type Couple = (Facteur,Exposant)
type Decomposition = [Couple]

rep2int :: Decomposition -> Int
rep2int []=1
rep2int [(x,y)]= x^y
rep2int (x:xs) = rep2int [x] * rep2int (tail (x:xs))

estPremier :: [a] -> Bool
estPremier [] = False
estPremier [1] = False
estPremier [2] = True
estPremier [(x,y)] = length [k | k<-[2..(x^y)],  (x^y) `mod` k == 0]== 1
estPremier xs = estPremier ((x^n,1),estPremier[()]





