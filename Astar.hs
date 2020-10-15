
type TilePos = (Int, Int)

type State = [TilePos]


-- Exemples d'états

e, s2, s3, ef :: State

e = [(2,2),(1,1),(2,1),(3,1),(2,3),(3,2),(3,3),(1,3),(1,2)]

s2 = [(2,3),(1,1),(2,1),(3,1),(2,2),(3,2),(3,3),(1,3),(1,2)]

s3 = [(2,3),(1,2),(1,1),(3,1),(3,2),(3,3),(2,2),(1,3),(2,1)]

ef = [(2,2),(1,1),(2,1),(3,1),(3,2),(3,3),(2,3),(1,3),(1,2)]

-- 1. Representation d'un état

whichTileAt :: TilePos -> State -> Int
whichTileAt pos s = if pos == head(s) then 0
				else 1+whichTileAt pos (tail(s))


posTile :: Int -> State -> TilePos
posTile i s = if i == 0 then head(s) 
				else posTile (i-1) (tail(s))
				
posEmpty :: State -> TilePos
posEmpty s = head(s)


-- 2. Visualisation d'un état

toString :: State -> String
toString s = row 1 s ++ row 2 s ++ row 3 s ++ "\n"


row :: Int -> State -> String
row n s = " " ++ t 1 ++ " " ++ t 2 ++ " " ++ t 3 ++ "\n"
   where t m = show (whichTileAt (m,n) s)

toStr :: [State] -> String
toStr x = concat(map toString (x))



-- 3. distance de MANHATTAN

dist :: TilePos -> TilePos -> Int
dist pos poss = abs((fst(poss)-fst(pos)))+abs((snd(poss)-snd(pos)))


-- 4. Fonction h

h :: State -> Int
h e = sum [dist (posTile x e) (posTile x ef) | x <- [0..(length e-1)] ]



-- 5. Fonction successeurs : comprendre et exprimenter

-- Pour visualiser un tat puis lÕensemble de ses successeurs

visu :: State -> IO ()
visu s = putStr (toStr ([s] ++ (successeurs s)))

successeurs :: State -> [State]
successeurs s = [swap i s | i <- [1.. (length s) - 1], valide i s]
   where 
         valide i s = (dist (posEmpty s) (posTile i s) == 1)
         swap i s = [posTile i s] ++ (take (i-1) (tail s)) ++ [posEmpty s] ++ (drop (i+1) s)


bfs ::State -> [State]
bfs s = bfsSolv [s][]

bfsSolv :: [State] -> [State] -> [State]    
bfsSolv (s:ss)visited
	|s == ef =    reverse (s:visited)
	|otherwise    = bfsSolv    (add_Bfs(notIn(successeurs s) visited) ss) (s:visited)
		where    
			notIn xs ys = [x | x<-xs,not (elem x ys)]

add_Bfs xs ys = ys ++ xs


dfs :: State -> [State]
dfs s = dfsSolv [s][]

dfsSolv :: [State] -> [State] -> [State]    
dfsSolv (s:ss)visited
	|s == ef =    reverse (s:visited)
	|otherwise    = dfsSolv    (add_Dfs(notIn(successeurs s) visited) ss) (s:visited)
		where    
			notIn xs ys = [x | x<-xs,not (elem x ys)]

add_Dfs xs ys = xs ++ ys

	
add_Astar :: State -> [State] -> State	
add_Astar s (x:xs) | xs == [] = s
              | h s < h x = add_Astar s xs
              | otherwise = add_Astar x xs

			  
astar :: State -> [State]
astar x = astarSolv x []

astarSolv :: State -> [State] -> [State]
astarSolv x visited 
        | x == ef = reverse (x:visited)
        |otherwise = astarSolv(add_Astar x (successeurs x))(x:visited)
		
		
bfsPath :: State -> [State]
bfsPath s = astar s

estSoluble :: State -> Bool
estSoluble e = if mod (h e) 2 == dist (head e) (head ef) then True else False
