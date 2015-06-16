import Euterpea
import Data.Ratio

f1 :: Int -> [Pitch] -> [Pitch]
f1 i ps = map (trans i) ps

f2 :: [Dur] -> [Music a]
f2 = map (rest)

f3 :: [Music Pitch] -> [Music Pitch]
f3 = let chop (Prim (Note d p)) = (note ((numerator d) % (2 * denominator d)) p) :+: (rest ((numerator d) % (2 * denominator d)))
     in map chop

doubleEach :: [Integer] -> [Integer]
doubleEach = map (* 2)

pairAndOne :: [Integer] -> [(Integer,Integer)]
pairAndOne = let f n = (n, n + 1)
             in map f

addEachPair :: [(Integer,Integer)] -> [Integer]
addEachPair = let f (a,b) = a + b
              in map f

addPairsPointwise :: [(Integer,Integer)] -> (Integer,Integer)
addPairsPointwise = let addPairs (a1,a2) (b1,b2) = (a1 + b1, a2 + b2)
                    in foldr addPairs (0,0)

fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse ds fs = map (\(d,f) -> f d) (zip ds fs)

maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch [] = error "Empty input list"
maxAbsPitch (x:xs) = foldr (max) x xs

minAbsPitch :: [AbsPitch] -> AbsPitch
minAbsPitch [] = error "Empty input list"
minAbsPitch (x:xs) = foldr (min) x xs

chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 
  | absPitch p1 == absPitch p2 = note qn p1
  | absPitch p1 < absPitch p2 = line (map (\p -> note qn (pitch p)) [(absPitch p1)..(absPitch p2)])
  | otherwise = line (map (\p -> note qn (pitch p)) [(absPitch p2)..(absPitch p1)])

mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p is = line (map (note qn) (reverse (foldl (\ps i -> (trans i (head ps)) : ps) [p] is)))

data ScaleMode = Ionian | Dorian | Phrygian | Lydian | Myxolydian | Aeolian | Locrian deriving (Enum)

majorIntervals = cycle [2,2,1,2,2,2,1]

genScale :: Pitch -> ScaleMode -> Music Pitch
genScale p m = mkScale p $ take 6 $ drop (fromEnum m) majorIntervals

fjm :: Pitch -> Music Pitch
fjm p = let fj = line $ map (\(t,d) -> note d $ trans t p) [(0,en),(2,en),(4,en),(0,en)]
            dv = line $ map (\(t,d) -> note d $ trans t p) [(4,en),(5,en),(7,qn)]
            mb = line $ map (\(t,d) -> note d $ trans t p) [(7,sn),(9,sn),(7,sn),(5,sn),(4,en),(0,en)]
            dd = line $ map (\(t,d) -> note d $ trans t p) [(0,en),(-5,en),(0,qn)]
        in line [fj, fj, dv, dv, mb, mb, dd, dd]

fjmRound :: Pitch -> Music Pitch
fjmRound p = chord [fjm p, 
                    Modify (Instrument BrightAcousticPiano) (line [rest wn, fjm p]), 
                    Modify (Instrument ElectricGrandPiano)  (line [rest wn, rest wn, fjm p]), 
                    Modify (Instrument HonkyTonkPiano)      (line [rest wn, rest wn, rest wn, fjm p])
                   ]
--fjmRound p = foldl (\m (i,fjm) -> chord [m, line [rest (i), fjm]]) (rest 0) $ zip [0..] [fjm p , fjm p, fjm p, fjm p]
