-- | Conway's Game of Life in Haskell.
-- Das bekannte Game of Life in Haskell programmiert. 
module GoL where

-- Datentypen für Zellen
type Cell = ((Int,Int),Bool)
type LoC = [Cell]

-- Datentyp für eine Welt
data World = World { locs :: [LoC] }

-- | Es wird die nächste Welt zu der angegbenen berechnet.
nextWorld :: World -> World
nextWorld world = World l
  where l = map (\x -> nextLine x world) (locs world)
          where 
            nextLine [] _ = []
            nextLine (y:ys) w = [(nextGen (neighbors y w) y w)] ++ nextLine ys w
        
-- | Berechnung der nächsten Generation.
-- Hier steckt der eigentliche Algorithmus drinnen. Es wird berechnet ob eine         
-- Zelle am Leben bleibt oder stirbt oder ob eine tote Zelle lebendig wird.        
nextGen :: Int -> Cell -> World -> Cell
nextGen n ((x,y),s) world
  | n < 2 || n > 3 = ((x,y),False)           -- Zelle stirbt
  | n == 3 = ((x,y),True)                    -- tote Zelle wird lebendig
  | n == 2 = ((x,y), state ((x,y),s) world)  -- Zelle bleibt wie sie ist
  | otherwise = ((x,y),False)
                
-- | Überprüft ob eine Zelle in der Welt vorhanden ist.
-- Es wird überprüft ob eine Zelle in einer angegebenen Welt vorhanden ist.
celem :: Cell -> World -> Bool
celem ((x,y),_) world = any (==(x,y)) $ map (fst) $ foldl (++) [] (locs world)

-- | Vergleich von zwei Zellen
-- Normalerweise kann man zwei Zellen mit '==' vergleichen. Hier wird aber auch
-- der Status verglichen. Wenn man nun eine Zelle finden möchte ohne auf den
-- Status der Zelle rücksicht zu nehmen muß man diese Funktion verwenden.
eq :: Cell -> Cell -> Bool
eq c1 c2 = fst c1 == fst c2

-- | Suche eine Zelle.
-- Es wird eine Zelle in der angegebenen Welt gesucht.
findCell :: Cell -> World -> Cell
findCell ((x,y),s) world = head $ filter (`eq`((x,y),s)) $ foldl (++) [] (locs world)

-- | Status der Zelle.
-- Es wird ermittelt welchen Status die Zelle in der angegebenen Welt hat.
-- Diese Funktion wird dazu verwendet um nachzusehen welchen Status die
-- Zelle in der früherern Welt hatte.
state :: Cell -> World -> Bool
state ((x,y),s) world
  | ((x,y),s) `celem` world == True = snd $ findCell ((x,y),s) world
  | otherwise = False
                
-- | Es wird ermittelt ob eine Zelle vorhanden ist oder nicht.
-- Wenn z.B. eine Zelle außerhalb des Randes ist, ist dies keine
-- gültige Zelle und es wird 'False' zurück gegeben.
extract :: Cell -> World -> Bool
extract ((row,col),s) world
  | row < 0 || col < 0 = False
  | row > length (locs world) || col > length (locs world) = False
  | otherwise = state ((row,col),s) world
                
-- | Es werden die Anzahl der am lebenden benachbarten Zellen ermittelt.
-- 
--  +----+----+----+
--  | n1 | n2 | n3 |  Es wird zunächst ermittelt ob die Nachbaren am
--  +----+----+----+  Leben sind oder nicht und alle die am Leben sind
--  | n4 | xx | n5 |  werden in eine Liste gespeichert. Danach wird die
--  +----+----+----+  Größe der Liste ermittelt und dadurch erhält man
--  | n6 | n7 | n8 |  die Anzahl der am Leben befindlicher Nachbarn.
--  +----+----+----+
--
neighbors :: Cell -> World -> Int
neighbors ((x,y),s) world = length (filter (\x -> x == True) [n1,n2,n3,n4,n5,n6,n7,n8])
  where n1 = extract ((x-1,y-1),s) world
        n2 = extract ((x-1,y),s) world
        n3 = extract ((x-1,y+1),s) world
        n4 = extract ((x,y-1),s) world
        n5 = extract ((x,y+1),s) world
        n6 = extract ((x+1,y-1),s) world
        n7 = extract ((x+1,y),s) world
        n8 = extract ((x+1,y+1),s) world
        
-- IO Stuff
        
-- | Schönere Darstellung der einzelnen Zellen.
-- Wenn eine Zelle tot ist dann wird '_' ausgegeben und wenn sie am
-- leben ist dann wird '@' ausgegeben.        
prettyPrintCell :: Bool -> String
prettyPrintCell False = "_"
prettyPrintCell True = "@"

-- | Ausgabe der Welt.
-- Es wird eine Welt auf der Kommandozeile oder in ghci ausgegeben.
printWorld :: World -> IO ()
printWorld w = do
  mapM_ printLoC $ locs w
  where printLoC loc = do
          mapM_ putStr $ map (++ "|") $ map prettyPrintCell $ map snd loc
          putStrLn ""
  
-- Test Stuff

-- | Startet hintereinander einige Welten.
-- Es werden nacheinander die Welten dargestellt bis man mit 'q' abbricht.
interactive :: World -> IO ()  
interactive world = do
  printWorld world
  c <- getChar
  if c == 'q' then 
    return ()
    else 
    interactive $ nextWorld world

-- | Eine Funktion zum Testen der Applikation.
-- Hier wird einfach eine 'World' generiert zum Testen.
generateWorld :: World
generateWorld = World lines 
  where lines = [ [((0,0),False),((0,1),False),((0,2),False),((0,3),False)]
                , [((1,0),False),((1,1),True),((1,2),True),((1,3),False)]
                , [((2,0),True),((2,1),True),((2,2),False),((2,3),False)]
                , [((3,0),True),((3,1),False),((3,2),False),((3,3),False)]
                ]