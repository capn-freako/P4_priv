-- To run:
--
--   stack build :switch-examples
--
-- You might also want to use stack's --file-watch flag for automatic recompilation.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main where

import Data.Map.Strict (fromList)
import Text.Printf
import Language.P4.Interp

main :: IO ()
main = sequence_
  [ putChar '\n' -- return ()
  , testP4 "dummy"  dummyRprt  Nothing                    (mkInterp dummyScript)  []         initState
  , testP4 "simple" simpleRprt Nothing                    (mkInterp simpleScript) simplePkts initState
  , testP4 "test"   testRprt   (Just (refPkts, refState)) (mkInterp simpleScript) simplePkts initState
  ]

{--------------------------------------------------------------------
    Testing utilities
--------------------------------------------------------------------}

testP4 :: String ->                      -- test name
          ( String ->                      -- test name
            Maybe ([Pkt], SwitchState) ->  -- optional validation reference
            ([Pkt], SwitchState) ->        -- processed packets & final switch state
            IO ()
          ) ->                           -- reporting/validation function
          Maybe ([Pkt], SwitchState) ->  -- optional validation reference
          P4Interp ->                    -- DUT
          [Pkt] -> SwitchState ->        -- test packets & initial switch state
          IO ()
testP4 nm rprt ref dut pkts st = rprt nm ref $ runP4 dut pkts st

{--------------------------------------------------------------------
    Misc.
--------------------------------------------------------------------}

-- Switch states.
initState = initSwitchState
  { _pktsLost    = 0
  , _pktsDropped = 0
  }

refState = initSwitchState
  { _pktsLost    = 0
  , _pktsDropped = 1
  , _pktsMatched = 1
  , _tblHits     = fromList [(0::Int,1::Int)]
  , _portAddrMap = fromList [(VInt 1, Addr 82),(VInt 2, Addr 83)]
  , _addrPortMap = fromList [(Addr 82, VInt 1),(Addr 83, VInt 2)]
  }

-- Reports.
dummyRprt nm _ _ = do
  putStrLn $ "Testing " ++ nm ++ "..."
  putStrLn "\tHello, World!"

simpleRprt nm _ res =
  let outPkts = fst res
  in do putStrLn $ "Testing " ++ nm ++ "..."
        putStrLn $ "Output packets (" ++ show (length outPkts) ++ "):"
        _ <- mapM print (fst res)
        putStrLn "Final switch state:"
        print (snd res)

testRprt _ Nothing _ = error "testRprt called without test reference!"
testRprt nm (Just (rPkts, rState)) (outPkts, outState) =
  do putStrLn $ "Testing " ++ nm ++ "..."
     putStrLn $ "Final switch state: " ++
       if outState == rState then "PASS" else "FAIL"
     putStrLn $ "Packet comparison results: " ++
       if outPkts == rPkts then "All match."
                          else partialPktMatchStr
  where partialPktMatchStr = printf "%3d %% match." $ ((round $ 100 * matchRatio)::Int)
        matchRatio         = ( fromIntegral (length (filter (uncurry (==)) $ zip rPkts outPkts)) /
                               (fromIntegral $ length outPkts)
                             ) :: Float

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
tblDropNMB = mkTable
  ( 0                                        -- table ID
  , [ FeType ]                               -- fields to match
  , []                                       -- parameters to set
  , [ ( 0, [Exact $ Etype NMB], [], [Drop] ) -- rows ([(ID,[Match],[Value],[Action])])
    ]
  )

-- Packet lists.
simplePkts = map mkPkt
  --  inP    srcAd     dstAd      eT       pyldSz
  [ (  1,       82,       83,     IP,          10 )
  , (  2,       83,       82,    NMB,          10 )
  ]

refPkts = map mkRefPkt
  --  inP    outP  vlanID dropped           srcAd           dstAd      eT  pyldSz
  [ (   1,      0,      0,  False,             82,             83,     IP,     10 )
  , (   2,      1,      0,   True,             83,             82,    NMB,     10 )
  ]

