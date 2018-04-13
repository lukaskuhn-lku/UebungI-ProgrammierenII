-- ============================================================================
-- Grundlagen der Programmierung 2 
-- Aufgabenblatt 1
-- Loesungsvorschl"age
-- ============================================================================

import Data.List

-- =============================================================================
-- =
-- = Aufgabe 1
-- =

note vorgetragen1 vorgetragen2 bonuspkt klausurpkt =  if vorgetragen1 && vorgetragen2 
                                                      then if klausurpkt >= 40 then auswertung (bonuspkt + klausurpkt)
                                                           else auswertung klausurpkt
                                                      else auswertung klausurpkt

auswertung pkt = if pkt >= 86 then 1.0
                 else if pkt >= 82 then 1.3 
                    else if pkt >= 78 then 1.7
                        else if pkt >= 74 then 2.0
                           else if pkt >= 70 then 2.3
                                else if pkt >= 66 then 2.7
                                    else if pkt >= 62 then 3.0
                                        else if pkt >= 58 then 3.3
                                           else if pkt >= 54 then 3.7
                                              else if pkt >= 50 then 4.0
                                                   else 5.0 
    

         --       if pkt >= 50 
         --       then 4.0-(((rundePkt pkt)*3)/10)
         --       else 5.0

        -- rundePkt pkt = fromIntegral(floor((pkt-50)/4))

-- =
-- =
-- =============================================================================

-- =============================================================================
-- =
-- = Aufgabe 2
-- =
-- = ---------------------------------------------------------------------------
-- = - zeigeFeld und Beispielmatrizen.
-- = -

zeigeFeld feld = putStrLn $ unlines [[feld i (5-j) | i <- [1..5]] | j <- [1..4]]

-- Start
feldA 1 1 = 's'
feldA 1 2 = 's'
feldA 1 3 = 's'
feldA 1 4 = 's'
feldA 5 1 = 'w'
feldA 5 2 = 'w'
feldA 5 3 = 'w'
feldA 5 4 = 'w'
feldA _ _ = ' '

-- Fast erreichtes Ziel.
feldB 1 1 = 'w'
feldB 1 2 = 'w'
feldB 1 3 = 'w'
feldB 1 4 = 'w'
feldB 4 3 = 's'
feldB 5 1 = 's'
feldB 5 3 = 's'
feldB 5 4 = 's'
feldB _ _ = ' '

-- = -
-- = -
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe a)
-- = -

istZugDiagonal xAlt yAlt xNeu yNeu = if xAlt /= xNeu || yAlt /= yNeu 
                                     then if getDifferenz xAlt xNeu == getDifferenz yAlt yNeu then True
                                          else False
                                     else False -- Figur hat sich auf mindestens einer Achse nicht bewegt
                                    
getDifferenz neu alt = if neu - alt >= 0 then neu -alt 
                       else (neu - alt)*(-1)


-- = -
-- = -
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe b)
-- = -

bedrohtRichtung x y farbe richtung feld = if richtung == 0 then checkeBedroht (x-1) (y+1) farbe feld (0) richtung
                                     else if richtung==1 then checkeBedroht (x+1) (y+1) farbe feld (0) richtung
                                        else if richtung==2 then checkeBedroht (x+1) (y-1) farbe feld (0) richtung 
                                          else if richtung==3 then checkeBedroht (x-1) (y-1) farbe feld (0) richtung
                                            else False
                                     


checkeBedroht x y farbe feld counter richtung = if feld x y /= farbe && feld x y /= ' ' then True 
    else if counter <= 4 
    then if richtung==0 then checkeBedroht (x-1) (y+1) farbe feld (counter+1) richtung
         else if richtung==1 then checkeBedroht (x+1) (y+1) farbe feld (counter+1) richtung
            else if richtung==2 then checkeBedroht (x+1) (y-1) farbe feld (counter+1) richtung 
                else if richtung==3 then checkeBedroht (x-1) (y-1) farbe feld (counter+1) richtung
                    else False
        else False
                                        

-- = - Test 
-- = -
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe c)
-- = -

bedroht x y farbe feld = if bedrohtRichtung x y farbe (0) feld then True 
                         else if bedrohtRichtung x y farbe (1) feld then True
                         else if bedrohtRichtung x y farbe (2) feld then True
                         else if bedrohtRichtung x y farbe (3) feld then True
                              else False
                              
-- = -
-- = -
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe d)
-- = -

istZugGueltig = undefined -- zu implementieren!

-- = -
-- = -
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe e)
-- = -

zieheWennGueltig = undefined -- zu implementieren!

-- = -
-- = -
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe f)
-- = -

geloest = undefined -- zu implementieren!

-- = -
-- = -
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe g)
-- = -

loesung = undefined -- zu implementieren!