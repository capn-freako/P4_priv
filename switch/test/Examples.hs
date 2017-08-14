-- To run:
--
--   stack build :switch-examples
--
-- You might also want to use stack's --file-watch flag for automatic recompilation.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main where

import qualified Data.Map as Map
import Language.P4.Interp

main :: IO ()
main = sequence_
  [ putChar '\n' -- return ()
  -- , testP4 "dummy" dummyRprt (mkInterp dummyScript) [] initState
  , testP4 "simple" simpleRprt (mkInterp simpleScript) simplePkts initState
  ]

{--------------------------------------------------------------------
    Testing utilities
--------------------------------------------------------------------}

testP4 :: String -> (String -> ([Pkt], SwitchState) -> IO ()) ->
          P4Interp -> [Pkt] -> SwitchState -> IO ()
testP4 nm rprt dut pkts st = rprt nm $ runP4 dut pkts st

{--------------------------------------------------------------------
    Misc.
--------------------------------------------------------------------}

-- Initial switch states.
initState = SwitchState
  { pktsLost    = 0
  , pktsDropped = 0
  }

-- Reports.
dummyRprt nm _ = do
  putStrLn $ "Testing " ++ nm ++ "..."
  putStrLn "\tHello, World!"

simpleRprt nm res =
  let outPkts = fst res
  in do putStrLn $ "Testing " ++ nm ++ "..."
        putStrLn $ "Output packets (" ++ show (length outPkts) ++ "):"
        _ <- sequence $ map (putStrLn . show) (fst res)
        putStrLn $ "Final switch state:"
        putStrLn $ show (snd res)

-- P4 Scripts.
dummyScript = P4Script
  { ingress = []
  , egress  = Nothing
  }

simpleScript = P4Script
  { ingress =
      [ Apply tblDropNMB [] []
      ]
  , egress  = Nothing
  }

-- Tables.
tblDropNMB = Table
  { tableID = 0
  , rows =
      [ TableRow
          { fields = Map.fromList
                       [ (FeType,   Exact $ Etype NMB)
                       , (FsrcAddr, NoMatch)
                       , (FdstAddr, NoMatch)
                       ]
          , params = Map.empty
          , actions =
              [ Drop
              ]
          }
      ]
  }

-- Packet lists.
simplePkts =
  [ Pkt
      { _inPort   = VInt  1
      , _outPort  = VInt  0
      , _vlanId   = VInt  0
      , _dropped  = VBool False
      , _srcAddr  = Addr  82
      , _dstAddr  = Addr  83
      , _eType    = Etype IP
      , _pyldSize = VInt  10
      }
  , Pkt
      { _inPort   = VInt  1
      , _outPort  = VInt  0
      , _vlanId   = VInt  0
      , _dropped  = VBool False
      , _srcAddr  = Addr  82
      , _dstAddr  = Addr  83
      , _eType    = Etype NMB
      , _pyldSize = VInt  10
      }
  ]

