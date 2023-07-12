type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq,Ord)

up :: MyState -> MyState
up (S (0,y) m_list str state) = Null
up (S (x,y) m_list str state) = (S ((x-1),y) m_list "up" (S (x,y) m_list str state))

down :: Int -> MyState -> MyState
down limit (S (x,y) m_list str state) | limit == x = Null
	| otherwise = (S ((x+1),y) m_list "down" (S (x,y) m_list str state))

left :: MyState -> MyState
left (S (x,0) m_list str state) = Null
left (S (x,y) m_list str state) = (S (x,(y-1)) m_list "left" (S (x,y) m_list str state))

right :: Int -> MyState -> MyState
right limit (S (x,y) m_list str state) | limit == y = Null
	| otherwise = (S (x,(y+1)) m_list "right" (S (x,y) m_list str state))

removeHelp f [] = []
removeHelp f (h:t)
	| f==h = t
	| otherwise = [h]++removeHelp f t

collect :: MyState -> MyState
collect (S mine m_list d state)
	| (removeHelp mine m_list) == m_list = Null
	| otherwise = (S mine (removeHelp mine m_list) "collect" (S mine m_list d state))

removeNull [] = []
removeNull (h:t) 
	| h == Null = removeNull t 
	| otherwise = [h] ++ removeNull t

manhattan :: Cell -> MyState -> Cell
manhattan (xIn,yIn) (S (x,y) [] str state) = (xIn,yIn)
manhattan (xIn,yIn) (S (x,y) ((mx,my):t) str state)
	| (abs (x-mx)+abs (y-my))<(abs (x-xIn)+abs (y-yIn)) = manhattan (mx,my) (S (x,y) t str state)
	| otherwise = manhattan (xIn,yIn) (S (x,y) t str state)

nextMyStates :: Int -> Int -> MyState -> [MyState]
nextMyStates xMax yMax (S (x,y) m_list str state) 
	| xMin < x = removeNull [(up (S (x,y) m_list str state))]
	| xMin > x = removeNull [(down xMax (S (x,y) m_list str state))]
	| yMin < y = removeNull [(left (S (x,y) m_list str state))]
	| yMin > y = removeNull [(right yMax (S (x,y) m_list str state))]
	| otherwise = removeNull [(collect (S (x,y) m_list str state))] where (xMin,yMin) = manhattan ((x*x),(y*y)) (S (x,y) m_list str state)


isGoal :: MyState -> Bool
isGoal (S loc m_list str state)
	| m_list == [] = True
	| otherwise = False
	
search :: Int -> Int -> [MyState] -> MyState
search x y (state:t)
	| isGoal state == True = state
	| otherwise = search x y (t ++ (nextMyStates x y state))
	
constructSolution :: MyState -> [String]
constructSolution (S loc m_list str state)
	| str == "" = []
	|otherwise = (constructSolution state) ++ [str]

maxX x [] = x
maxX x ((x1,y1):t) = if x>x1 then maxX x t else maxX x1 t

maxY y [] = y
maxY y ((x1,y1):t) = if y>y1 then maxY y t else maxY y1 t

solve :: Cell -> [Cell] -> [String]
solve loc locs = constructSolution (search (maxX 0 locs) (maxY 0 locs) [(S loc locs "" Null)])



