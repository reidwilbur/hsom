import Euterpea

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn 
       in dMinor :+: gMajor :+: cMajor

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = let minorTwo = note d (trans 2 p) :=: note d (trans 2 (trans 3 p)) :=: note d (trans 2 (trans 7 p))
                     five     = note d (trans 7 p) :=: note d (trans 7 (trans 4 p)) :=: note d (trans 7 (trans 7 p))
                     one      = note (d*2) p :=: note (d*2) (trans 4 p) :=: note (d*2) (trans 7 p)
                 in minorTwo :+: five :+: one

data BluesPitchClass = Ro | MT | Fo | Fi | MS deriving(Show, Eq, Ord, Read, Enum, Bounded)

type BluesPitch = (BluesPitchClass, Octave)

ro,mt,fo,fi,ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o)
mt o d = note d (MT, o)
fo o d = note d (Fo, o)
fi o d = note d (Fi, o)
ms o d = note d (MS, o)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d (Ro,o))) = note d (C,o)
fromBlues (Prim (Note d (MT,o))) = note d (Ef,o)
fromBlues (Prim (Note d (Fo,o))) = note d (F,o)
fromBlues (Prim (Note d (Fi,o))) = note d (G,o)
fromBlues (Prim (Note d (MS,o))) = note d (Bf,o)
fromBlues (Prim (Rest d)) = rest d
fromBlues (m1 :+: m2) = (fromBlues m1) :+: (fromBlues m2)
fromBlues (m1 :=: m2) = (fromBlues m1) :=: (fromBlues m2)
--fromBlues (Modify (Control (Music a))) = Modify (Control (fromBlues a))

bluesMel1  = (ro 5 qn :=: fi 5 qn) :+: (ms 4 en :=: fi 5 en) :+: (ro 5 en :=: fi 5 en) :+: (mt 5 en :=: fi 5 en) :+: (ro 5 en :=: fi 5 en)

transM :: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Note d p)) = note d (trans ap p)
transM ap (Prim (Rest d)) = rest d
transM ap (m1 :+: m2) = (transM ap m1) :+: (transM ap m2)
transM ap (m1 :=: m2) = (transM ap m1) :=: (transM ap m2)

