import Data.List
import Data.Maybe
maandgetal = [0,3,3,6,1,4,6,2,5,0,3,5]
jaargetal  = [0,1,2,3,5,6,0,1,3,4,5,6,1,2,3,4,6,0,1,2,4,5,6,0,2,3,4,5]
eeuwgetal  = [ (15,0),(19,0),(23,0),(16,6),(20,6),(24,6),(17,4),(21,4),
               (25,4),(18,2),(22,2),(26,2)]
weekdagen  = ["zondag","maandag","dinsdag","woensdag","donderdag","vrijdag","zaterdag"]
-- Geef het maandgetal  terug uit de tabel
zoekMaandgetal m = maandgetal!!(m-1)
-- Geef het jaargetal terug uit de tabel
zoekJaarGetal n = jaargetal !! mod (mod n 100) 28
-- Geef het eeuwgetal terug uit de tabel
zoekEeuwGetal n = snd (fromJust (find (\g -> fst g == div n 100) eeuwgetal))
-- Pas op voor negatieve indexen
zoekWeekdag x = weekdagen !! mod x 7
-- Geef terug of een jaar een schrikkeljaar is of niet
schrikkeljaar j = ((mod j 4 == 0) && (mod j 100 /= 0)) || (mod j 400 == 0)

-- Bereken de weekdag
weekdag dag maand jaar =
  do
    let getal = dag + zoekMaandgetal maand + zoekJaarGetal jaar + zoekEeuwGetal jaar
    if schrikkeljaar jaar
      then zoekWeekdag (getal - 1)
      else zoekWeekdag getal

-- Gegeven de eeuw en het jaar geef de
-- weekdag waarop valentijn valt dat jaar.
valentijn = weekdag 14 02

main = do
  --let m = zoekEeuwGetal 1582
  let v = valentijn 2017
  print v
