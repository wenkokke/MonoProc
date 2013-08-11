module Main where

import PROC
import PROC.MF.Analysis.CP
import PROC.Testing (testAnalysis)
import PROC.Parsing (pCP)

import Data.Foldable (forM_)
import Text.Printf (printf)
import qualified Data.Set as S (toList)
import Text.ParserCombinators.UU.Utils (runParser)

-- because CP analysis is non-distributive, I'm testing the MFP against the MOP
-- solution (for a known program where MOP works) to see that at each program
-- point the result of MFP refines that of MOP.
-- this is different than with the other analyses where I'm testing that the MOP
-- and the MFP solutions both give the exact same results.

main :: IO ()
main = do testAnalysis mop mfCP progCP reslCP1
          testAnalysis mfp mfCP progCP reslCP2
          
          -- this one is kinda superfluous, since if we know that the
          -- results are reslCP1 and reslCP2, and we know that the refinement
          -- relation holds between these... well, then this will always succeed...
          
          let mopCP = analyse mop mfCP progCP
          let mfpCP = analyse mfp mfCP progCP
          let (<:)  = refines (getL $ mfCP progCP)
          forM_ (S.toList $ labels progCP) $ \l -> do            
            let mop = mopCP l
            let mfp = mfpCP l
            if mfp <: mop
              then return ()
              else fail (printf "expected refinement of %s, found %s (at %d)" (show mop) (show mfp) l)
    
progCP :: Prog
progCP = mkProg
  [ "x = 2;"
  , "y = 4;"
  , "x = 1;"
  , "if (y > x) {"
  , "  z = y;"
  , "}"
  , "else {"
  , "  z = y * y;"
  , "}"
  , "x = z;"
  ]

-- |The result of a MOP CP analysis of @progCP@.
reslCP1 :: [(Label,CP)]
reslCP1 = cps
  [ "{(x,2)}"
  , "{(x,2),(y,4)}"
  , "{(x,T),(y,4)}"
  , "{(x,T),(y,4)}"
  , "{(x,T),(y,4),(z,4)}"
  , "{(x,T),(y,4),(z,16)}"
  , "{(x,T),(y,4),(z,T)}"
  ]
  
-- |The result of a MFP CP analysis of @progCP@.
reslCP2 :: [(Label,CP)]
reslCP2 = cps
  [ "{(x,2)}"
  , "Bottom"
  , "Bottom"
  , "Bottom"
  , "Bottom"
  , "Bottom"
  , "Bottom"
  ]

cps :: [String] -> [(Label,CP)]
cps = zip [1..] . map (runParser "stdin" pCP)
  