removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (a:as) = a:removeDuplicates (filter (/=a) as)

-- POINT -- understood as a vector (for translation) or a position of a block inside polyomino.
newtype Point = Point (Int,Int)

instance Eq Point where
  (Point (x,y)) == (Point (a,b)) = (x==a) && (y==b) 
  (Point (x,y)) /= (Point (a,b)) = (x/=a) || (y/=b) 

instance Show Point where
  show (Point (x,y)) = "(Point ("++show x++","++show y++"))"

minPoint :: Point -> Point -> Point
minPoint (Point (a,b)) (Point(x,y)) = (Point(min a x, min b y))

minusPoint :: Point -> Point
minusPoint (Point(a,b)) = (Point(-a,-b))

-- POLYOMINO -- A simple list of points (blocks) that form a polyomino.
newtype Polyomino = Polyomino [Point]

instance Eq Polyomino where
  (Polyomino a) == (Polyomino b) = filter (\x -> not (x `elem` a)) b == [] && filter (\x -> not (x `elem` b)) a == []
  (Polyomino a) /= (Polyomino b) = not ((Polyomino a) == (Polyomino b)) 

instance Show Polyomino where
  show (Polyomino []) = ""
  show (Polyomino xs) = "(Polyomino("++show xs++")"

-- Normalize, so no blocks are on the left or below (0,0). 
-- Finds the smallest vetor that have to be applied (minima of coordinates) and translates polyomino by that vector.
normalForm :: Polyomino -> Polyomino
normalForm (Polyomino a) = translate (Polyomino a) (minusPoint (lowLeft(Polyomino a)))

-- Finds vector that needs to be applied to polyomino to be normlized.
lowLeft :: Polyomino -> Point
lowLeft (Polyomino (x:xs)) = foldr (minPoint) x xs

-- If normal forms are the same then Polyominoes are the same.
translatableSimilarity :: Polyomino -> Polyomino -> Bool
translatableSimilarity (Polyomino a) (Polyomino b) = (normalForm (Polyomino a)) == (normalForm (Polyomino b))

-- TRANSLATABLE - class for all objects that can be translated by a vector (represented as Point).
class Translatable a where
  translate :: a -> Point -> a 

instance Translatable Point where
  translate (Point (x,y)) (Point(a,b)) = (Point (x+a, y+b))

instance Translatable (Polyomino) where
  translate (Polyomino a) (Point(x,y)) = Polyomino (map (translate (Point(x,y))) a)

-- NEIGHBOURABLE - class for all objects that can generate a list of neighbour Points.
class Neighbourable a where
  neighbours :: a -> [Point]

instance Neighbourable Point where
  neighbours (Point (x,y)) = [Point (x+1,y), Point (x-1,y), Point (x,y+1), Point (x,y-1)]

basicNeighbours (Polyomino []) = []
basicNeighbours (Polyomino (p:ps)) = neighbours(p)++neighbours(Polyomino ps) 

-- Polyomino neighbours generate all points that are adjacent to any point inside polyomino and are not already in the polyomino.
instance Neighbourable Polyomino where
  neighbours (Polyomino p) =  (filter (\x -> not (x `elem` p))) (removeDuplicates (basicNeighbours (Polyomino p)))

--GENERATION - returns a list of generated polyominoes of given size.
--Recursive function: appends new blocks to neighbour points of previous step polyominoes, normalizes them and removes duplicates.
generatePolyominos :: Int -> [Polyomino]
generatePolyominos 1 = [(Polyomino [(Point (0,0))])]
generatePolyominos n = removeDuplicates ((map (normalForm)) [(Polyomino (b:a)) | (Polyomino a) <- (generatePolyominos (n-1)), b<-(neighbours (Polyomino a))])

-- ENUMERATION - returns a number of polyomioes of given size.
enumeratePolyominos = length.generatePolyominos

-- Returns a list of numbers of polyominoes.
enumerateAllPolyominos count = [enumeratePolyominos n | n<-[1..count]]