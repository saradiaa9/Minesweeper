type Cell = (Int,Int) 
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq) 

up:: MyState -> MyState
up (S (x,y)  l st ls) = if x==0 then Null else S (x-1,y)  l  "up"  (S (x,y)  l st ls)

down:: MyState -> MyState
down (S (x,y)  l st ls) = if x==3 then Null else S (x+1,y)  l  "down"  (S (x,y)  l st ls)

left:: MyState -> MyState
left (S (x,y)  l st ls) = if y==0 then Null else S (x,y-1)  l  "left"  (S (x,y)  l st ls)

right:: MyState -> MyState
right (S (x,y)  l st ls) = if y==3 then Null else S (x,y+1)  l  "right"  (S (x,y)  l st ls)

collect:: MyState -> MyState
collect (S x xs st ls) = if elem x xs then (S x (filter (/=x) xs) "collect" (S x xs st ls)) else Null


nextMyStates::MyState->[MyState]
nextMyStates (S (x,y)  l st ls) = help( helperu (S (x,y)  l st ls) )


helperu (S (x,y)  l st ls) = (up (S (x,y)  l st ls):helperd (S (x,y)  l st ls))
helperd (S (x,y)  l st ls) = (down (S (x,y)  l st ls):helperl (S (x,y)  l st ls))
helperl (S (x,y)  l st ls) = (left (S (x,y)  l st ls):helperr (S (x,y)  l st ls))
helperr (S (x,y)  l st ls) = [collect (S (x,y)  l st ls)] ++ [right (S (x,y)  l st ls)]

help [] = []
help (h:t) = if  h /= Null then h : (help t)
			else help t


					
isGoal::MyState->Bool
isGoal (S (x,y)  l st ls) = if length l == 0 then True else False

search::[MyState]->MyState
search (h : t)= if isGoal h then h else search (t ++ nextMyStates h)

constructSolution:: MyState ->[String]
constructSolution (S (x,y)  l st ls) = rstring (S (x,y)  l st ls) []

rstring (S (x,y)  l "" ls) res1 = res1
rstring (S (x,y)  l st ls) res1 = rstring ls res1 ++ [st]

solve :: Cell->[Cell]->[String]
solve c ac = constructSolution( (search ([S c ac "" Null])) )