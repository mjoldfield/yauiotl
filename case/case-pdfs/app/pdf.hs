{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Lib

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Text.Printf

import Diagrams.Backend.Cairo

-- all sizes in mm

newtype Width  = Width  Double
newtype Height = Height Double

newtype Paper  = Paper (Width, Height)

a4 :: Paper
a4 = Paper (Width 297, Height 210)

inPoints mm = mm / 25.4 * 72.0

onPaper (Paper (Width w, Height h)) d = center d # rectEnvelope (p2 (- 0.5 * w, - 0.5 * h)) (r2 (w,h))

renderOnPaper :: Paper -> FilePath -> Diagram B -> IO ()
renderOnPaper p f = renderCairo f (mkSizeSpec2D (Just wp) (Just hp)) . onPaper p
   where hp = inPoints h
         wp = inPoints w
         (Paper (Width w, Height h)) = p

renderOnPapers :: Paper -> String -> [Diagram B] -> IO ()
renderOnPapers p f = mapM_ doPage . zip [0..]
  where doPage :: (Int, Diagram B) -> IO ()
        doPage (i, d) = renderOnPaper p (printf f  i) d

renderMaterial :: Paper -> (String, [Diagram B]) -> IO ()
renderMaterial p (tag, ds) = renderOnPapers p ("out-" ++ tag ++ "-%03d.pdf") ds

main = sequence_ $ map (renderMaterial a4) Lib.pdfDesigns

