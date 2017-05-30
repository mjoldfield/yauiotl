{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


module Lib
    ( design
    , pdfDesigns
    ) where

import Diagrams.Prelude

import Diagrams.TwoD.Arc
import Diagrams.TwoD.Text

import Diagrams.Segment

import Data.Char
import Data.Maybe
import Data.List.Split
import qualified Data.List as L

annotate :: ((Renderable (Text Double) b), (Renderable (Path V2 Double) b)) => Int -> (QDiagram b V2 Double Any, Material) -> (QDiagram b V2 Double Any, Material)
annotate i (d,m) = (d', m)
  where d'    = mconcat [ d, (text' label 1.6) # translate p ]
        p     = r2 (- 36, 55)
        label = show i ++ ":" ++ show m

inNs :: Int -> [a] -> [[a]]
inNs n [] = []
inNs n xs = take n xs : inNs n (drop n xs)

design :: ((Renderable (Text Double) b), (Renderable (Path V2 Double) b)) => QDiagram b V2 Double Any
design = vsep 5 [ hsep 5 (map fst xs) | xs <- chunksOf 3 panelStack ]
           # lw veryThin

pdfDesigns :: ((Renderable (Text Double) b), (Renderable (Path V2 Double) b)) => [(String,[QDiagram b V2 Double Any])]
pdfDesigns = map pagesIn [(minBound :: Material) .. ]
   where pagesIn m  = (matName m, [ d # lw 0.0762 | (d,m') <- panelStack, m' == m ])
         matName    = map toLower . show

------------------------------------------------------------------------

rRect :: (InSpace V2 n t, TrailLike t, RealFloat n) => n -> n -> n -> t
rRect w h r = roundedShape (w/2) (r - h/2)
              [ RELine   0 (h - 2 * r)
              , RECorner 0 0 r
              , RELine   (2 * r - w) 0
              , RECorner 0 1 r
              , RELine   0 (2 * r - h)
              , RECorner 0 2 r
              , RELine   (w - 2 * r) 0
              , RECorner 0 3 r
              ]
          
data RoundedElt a = RELine a a | RECorner Int Int a
   deriving (Show, Eq)

roundedShape :: (InSpace V2 n t, TrailLike t, RealFloat n) => n -> n -> [RoundedElt n] -> t 
roundedShape  = roundedShape' id  --  shapeAt x y . roundedPath

roundedShape' f x y = shapeAt x y . f . roundedPath

shapeAt x y = trailLike
              . (`at` p2 (x,y))
              . wrapTrail
              . glueLine

roundedPath = mconcat . map f                      
     where f (RELine   x y)   = lineFromOffsets . (:[]) . r2 $ (x,y)
           f (RECorner j k r) = arc' r (xDir & _theta <>~ ((fromIntegral k) / 4) @@ turn) (j' @@ turn)
                where j' = if even j then (1/4) else (-1/4)
             
rotRoundedPath :: (RealFloat n) => Int -> [RoundedElt n] -> [RoundedElt n]
rotRoundedPath n = map (rotRounded n)

rotRounded :: (RealFloat a) => Int -> RoundedElt a -> RoundedElt a
rotRounded 0 r = r
rotRounded n (RELine   dx dy) = rotRounded (n-1) (RELine (- dy) dx)
rotRounded n (RECorner j i r)  = RECorner j ((i + n) `mod` 4) r
             
              
text'                t s = text t               # fc black # fontSize (local s) # font "Arial" <> strutY (s * 1.2)
text''               t s = text t               # fc black # fontSize (local s) # font "Arial" 
alignedText' (px,py) t s = alignedText px py  t # fc black # fontSize (local s) # font "Arial" <> strutY (s * 1.2)

scaleToHeight h x = scale s x
    where s = h / (height x)


tetrad d = mconcat [ d, d # rotateBy (1/4), d # rotateBy (2/4), d # rotateBy (3/4) ]

------------------------------------------------------------------------

data Material = WhiteAcrylic |  Ply3mm | Veneer | Perspex | MDF3mm | MDF4mm | MDF6mm | Generic
  deriving (Show, Eq, Enum, Bounded);

panelStack :: ((Renderable (Text Double) b), (Renderable (Path V2 Double) b)) => [ (QDiagram b V2 Double Any, Material) ]
panelStack = map (\(d,m) -> (d # lc red, m)) $
             [
               (twoScreens,               WhiteAcrylic)
             , (twoScreens,               WhiteAcrylic)
--             , (holyScreen,               WhiteAcrylic)
--             , (holyBack,                 WhiteAcrylic)
--             , (halfHolyBack,             WhiteAcrylic)
             , (fourOf subScr,            WhiteAcrylic)
             , (topplate,                 Ply3mm)
             , (baseplate <> fourHsPBs,   Ply3mm)
             , (bottomplate,              Ply3mm)
             , (fourOf heatsinkF,         WhiteAcrylic)
             , (bottomSidesA,             Ply3mm)
             , (bottomSidesB,             Ply3mm)
             ]

fourHsPBs = centerXY $ vsep 0.5 $ map centerXY [ topThree, bottomOne ]
    where topThree  = nOf 3 heatsinkPB
          bottomOne = rotateBy (1/4) heatsinkPB

twoScreens = nOf 2 $ rotateBy (1/4) screen          

nOf n = hsep 0.5 . replicate n             

fourOf = nOf 4

screenWidth    = 160 
screenHeight   = 100 
screenRounding = 10  

screenTenonNumber  = 7 
screenTenonWidth   = 10
screenTenonHeight  = 2.5
screenTenonRound   = 0.5

screenMorticeWidth   = screenTenonWidth
screenMorticeHeight  = 3
screenMorticeRound   = 0.5
screenMorticeOffset  = 82.5 -- 85

-- subscreen
subScrTenonWidth   = 6
subScrMorticeWidth = 6
subScrOffset = 105
subScrWidth  =  12
subScrRound  =   1
subScrTenonNumber =  1

-- heatsink pad
hspWidth        = 25
hspHeight       = 10
hspRounding     = 5

-- heatsink back - between fins
hsbWidth        = hspWidth
hsbHeight       = hspHeight * 2 + 50
hsbRounding     = hspRounding

-- heatsink front - flat surface
hsfWidth        = 40
hsfHeight       = screenHeight
hsfRounding     = 2

hsfTenonNumber  = 3 

hsfTenonWidth   = 5
hsfTenonHeight  = 2.5
hsfTenonRound   = 0.5

hsfMorticeWidth   = hsfTenonWidth
hsfMorticeHeight  = 3
hsfMorticeRound   = 0.5
hsfMorticeOffset  = 55

hsfLEDradius      = 15

hsDrillOffset    = 5
hsDrillRadius    = 1.6

baseWidth       = 175 -- 180
baseHole        =  96
baseRounding    =  10
baseMountingOffset = 62
baseFootOffset     = 55
baseCableOffset    = 62

bottomWidth     = 160
bottomHeight    =  36
bottomRounding  =  10
bottomInset     =  10

bottomTenonNumber  = 5

bottomTenonWidth   = 10
bottomTenonSpace   = 10
bottomTenonHeight  = 4
bottomTenonRound   = 0.25

bottomMorticeWidth  = bottomTenonWidth
bottomMorticeHeight = 3.5
bottomMorticeRound  = 0.25


---
--
-- return paths for tenons and pads between them
--
tenon tw th tr = (tenonP, tw + 2 * tr, spaceP, tw')
  where tenonP = [ RECorner 1 3 tr
                 , RELine   0 th'
                 , RECorner 0 0 tr
                 , RELine   (- tw') 0
                 , RECorner 0 1 tr
                 , RELine   0 (- th')
                 , RECorner 1 4 tr
                 ]
        spaceP = [ RELine (- tw') 0 ]
                 
        th' = th - 2.0 * tr
        tw' = tw - 2.0 * tr

rectWithTenons sh sw sr th tw tr tn = centerXY $ roundedShape 0.0 0.0 path
   where (tenonP, tenonWidth, spaceP, spaceWidth) = tenon tw th tr

         tsWidth = (fromIntegral  tn     ) * tenonWidth
                 + (fromIntegral (tn - 1)) * spaceWidth

         tenonPad   = RELine (- pad) 0
             where pad = 0.5 * (sw' - tsWidth)

         tenonList = L.replicate tn tenonP

         tenons = L.intercalate spaceP tenonList

         tenonSide = [tenonPad] ++ tenons ++ [tenonPad]

         halfPath = [ RELine 0 sh', RECorner 0 0 sr ]
                    ++ tenonSide 
                    ++ [ RECorner 0 1 sr ]

         halfPath' = [ RELine 0 sh', RECorner 0 0 sr, RELine (- sw') 0, RECorner 0 1 sr ]

         path = halfPath ++ (rotRoundedPath 2 halfPath)

         sh' = sh - 2.0 * sr
         sw' = sw - 2.0 * sr

mortices = mortices' 0.25

mortices' slop w h r o n = tetrad morticeRow
   where mortice     = rRect (w + slop) (h + slop) r
         morticeAt p = mortice # translate (r2 p)
         morticeRow  = translate (r2 (0,o))
                       $ centerXY
                       $ mconcat [ morticeAt (x,0) | i <- [1..n] :: [Int]
                                                  , let x = (w + w) * fromIntegral i ]        

screen = rectWithTenons screenHeight screenWidth screenRounding screenTenonHeight screenTenonWidth screenTenonRound screenTenonNumber

smoothScreen = rRect screenWidth screenHeight screenRounding

screenBorder = 15

blessScreen s = s <> hole <> screenDrills
  where hole   = rRect (screenWidth - 2 * border) (screenHeight - 2 * border) screenRounding
        border = screenBorder

screenDrills = mconcat $ drillGrid 1.6 [ -dx,0,dx ] [ -dy,dy ]
  where dx     = 0.5 * (screenWidth  - border)
        dy     = 0.5 * (screenHeight - border)
        border = screenBorder

holyScreen = blessScreen screen

holyBack   = blessScreen smoothScreen

halfHolyBack = holes <> smoothScreen <> screenDrills
   where holes    = centerXY $ vsep holeSpace $ replicate nUp $ centerXY $ hsep holeSpace $ replicate nAcross hole
         hole     = rRect holeSize holeSize holeRound
         holeSize = (screenHeight - 2 * screenBorder - (fromIntegral nUp - 1) * holeSpace) / (fromIntegral nUp)
         holeSpace = 5
         holeRound = 3
         nUp      =  5 :: Int
         nAcross  =  9 :: Int

subScr = rectWithTenons screenHeight subScrWidth subScrRound    screenTenonHeight subScrTenonWidth screenTenonRound subScrTenonNumber

genericplate = centerXY $ mconcat [ outline, screenMorts, subScrMorts, hsfMorts, fixings, debug screenOL ]
  where outline = rRect baseWidth baseWidth baseRounding
        sMort       = mortices screenMorticeWidth screenMorticeHeight screenMorticeRound     
        screenMorts = sMort screenMorticeOffset screenTenonNumber
        hsfMorts = mortices hsfMorticeWidth hsfMorticeHeight hsfMorticeRound hsfMorticeOffset hsfTenonNumber
        screenOL    = tetrad $ rRect screenWidth 3 0.1 # translateY screenMorticeOffset
        ssMort       = mortices subScrMorticeWidth screenMorticeHeight screenMorticeRound     
        subScrMorts = rotateBy (1/8) $ ssMort subScrOffset 1 <> debug (rRect subScrWidth 3 0.1 # translateY subScrOffset)
        fixings   = mconcat $ drillGrid 3.3 [ -foffset,foffset] [-foffset,foffset]
        foffset   = 55 
        debug x   = if debugging then (x # lc black) else mempty
        debugging = False

-- topplate = genericplate <> hsSlots
--   where hsSlots = tetrad $ centerX $ hsep 3 [slot, slot]
--         slot    = rRect 30 18 5 # translate (r2 (0,40)) 

topplate = genericplate <> cooling
  where cooling = mconcat $ drillGrid 1.5 [-45,-40..45] [-45,-40..45] 

mountingHoles = drillGrid 1.6 [-m,m] [-m,m]
  where m = baseMountingOffset

footHoles = drillGrid 1.6 [-m,m] [-m,m]
  where m = baseFootOffset

bottomMorts = mortices bottomMorticeWidth bottomMorticeHeight bottomMorticeRound offset bottomTenonNumber
   where offset = bottomWidth / 2 - bottomInset

baseplate = centerXY $ mconcat $ [ genericplate, hole, bottomMorts ] ++ mountingHoles ++ cableHoles
  where  hole = rRect baseHole baseHole baseRounding 
         cableHoles = map (drillAt 2) [ (-l,0),(l,0),(0,-l),(0,l) ]
         l = baseCableOffset

drillGrid r xs ys = [ drillAt r (x,y) | x <- xs, y <- ys ]

drillAt r p = circle r # translate (r2 p)

bottomplate = centerXY $ mconcat $ mountingHoles ++ footHoles
              ++ [ outline
                 , bottomMorts
                 , picobuck                  # translate (r2 ( 35, 35))
                 , picobuck                  # translate (r2 ( 35,-35))
                 , (rotateBy (-1/4) piA)     # translate (r2 (-25,  0))
                 , dcTodc                    # translate (r2 ( 35,  0))
                 , cooling
                 ]
  where outline = rRect bottomWidth bottomWidth bottomRounding
        cooling = tetrad $ mconcat $ drillGrid 1.5 [-45,-40..45] [57.5,62.5]

picobuck = centerXY $ rotateBy (1/4) $ mconcat $
           [ rRect 35.6 30.5 0.5 # lc black # lw 1
           , drillAt 1.8 (-15.25, 12.7)
           , drillAt 1.8 ( 15.25,-12.7)
           ]

piZero = centerXY $ mconcat $
         (rRect 65 30 2.5 # lc black # lw 1)
         : (drillGrid 1.5 [-29,29] [-11.5,11.5])

piA    = centerXY $ mconcat $
         [ rRect 85 56 2.5 # lc black # lw 1
         , drillAt 1.5 ( 37.5,  15.5)
         , drillAt 1.5 (-17.0, -10.0)
         ]

dcTodc =  centerXY $ mconcat $
           [ rRect 48 23 0.5 # lc black # lw 1
           , drillAt 1.8 (-17, 8.5)
           , drillAt 1.8 ( 17,-8.5)
           ]

bottomSides  = rotateBy (1/4) $ vsep 1 [ frontBottomSide, backBottomSide, shortBottomSide, shortBottomSide ]

bottomSidesA = rotateBy (1/4) $ vsep 1 [ frontBottomSide, backBottomSide                                   ]

bottomSidesB = rotateBy (1/4) $ vsep 1 [                                  shortBottomSide, shortBottomSide ]

bottomSide x = rectWithTenons bottomHeight width bottomTenonRound bottomTenonHeight bottomTenonWidth bottomTenonRound bottomTenonNumber
   where width = bottomWidth - 2 * bottomInset + x * 3

shortBottomSide = bottomSide (-1)

frontBottomSide = mconcat [ bottomSide 1
                          , drillAt 4.5 (0,0)
                          , drillAt 2.5 (-50,0)
                          , translate (r2 (40,0)) $ text'' "YAUIoTL" 8 ]

backBottomSide  = mconcat [ bottomSide 1
                          , drillAt 3.0 (-50,0)
                          , drillAt 4.0 (-30,0)
                          , centrePos # translateX (-20)
                          , text' "12-25V,40W" 4    # translate (r2 (-20,-8))
                          , text'' "M.J.Oldfield" 4      # translate (r2 ( 30,  4))
                          , text'' "m@mjoldfield.com" 4  # translate (r2 ( 30, -4))
                          ]

centrePos = fc black $ lc black $
            hsep 2 [ rect 2 0.4
                   , rect 2 0.2 ||| (rings <> rect 4 0.2 # translateX 2)
                   , rect 2 0.4 `atop` (rect 2 0.4 # rotateBy (1/4))
                   ]
  where rings = ringA <> ringB
        ringA = circle 1
        ringB = arc (rotateBy alpha xDir) ((1 - 2 * alpha) @@ turn) # scale 2 # lw 1
        alpha = 3/32

heatsinkF = drill <> outline <> circle hsfLEDradius
  where  outline = rectWithTenons hsfHeight  hsfWidth hsfRounding hsfTenonHeight hsfTenonWidth hsfTenonRound hsfTenonNumber
         drill  = mconcat $ drillGrid  hsDrillRadius [-dx,dx] [-dy,dy]
         dx = 0.5 * hsbWidth  - hsDrillOffset
         dy = 0.5 * hsbHeight - hsDrillOffset

heatsinkPB = (rotateBy (1/4) $ heatsinkP) <> heatsinkB

heatsinkB = centerXY $ hole <> drill4 <> rRect hsbWidth hsbHeight hsbRounding
  where  drill r xs ys = mconcat $ drillGrid r xs ys
         drill4   = drill hsDrillRadius [-dx,dx] [-dy,dy]
         dx = 0.5 * hsbWidth  - hsDrillOffset
         dy = 0.5 * hsbHeight - hsDrillOffset
         hole = rRect (hsbWidth - 10) (hsbHeight - 17.5) hsbRounding

heatsinkP = centerXY $ hsep 1 [p,p]
  where  p  =         drill2 <> rRect hspWidth hspHeight hspRounding
         drill r xs ys = mconcat $ drillGrid r xs ys
         drill2   = drill hsDrillRadius [-dx,dx] [0]
         dx = 0.5 * hsbWidth  - hsDrillOffset
         dy = 0.5 * hsbHeight - hsDrillOffset
         hole = rRect (hsbWidth - 10) (hsbHeight - 20) hsbRounding


heatsinkDrillTemplate = centerXY $ mconcat $ (rRect 65 50 0.5) : (drillGrid 1.2 [0] [10,-10])

cutoff = rRect 1 400 0.5
