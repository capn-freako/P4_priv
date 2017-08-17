-- To run:
--
--   stack build :switch-examples
--
-- You might also want to use stack's --file-watch flag for automatic recompilation.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main where

import Language.P4.Interp

main :: IO ()
main = sequence_
  [ putChar '\n' -- return ()
  , testP4 "dummy" dummyRprt (mkInterp dummyScript) [] initState
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
  { _pktsLost    = 0
  , _pktsDropped = 0
  }

-- Reports.
dummyRprt nm _ = do
  putStrLn $ "Testing " ++ nm ++ "..."
  putStrLn "\tHello, World!"

simpleRprt nm res =
  let outPkts = fst res
  in do putStrLn $ "Testing " ++ nm ++ "..."
        putStrLn $ "Output packets (" ++ show (length outPkts) ++ "):"
        _ <- mapM print (fst res)
        putStrLn "Final switch state:"
        print (snd res)

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
  , (  1,       82,       83,    NMB,          10 )
  ]

