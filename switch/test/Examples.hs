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
  ]

{--------------------------------------------------------------------
    Testing utilities
--------------------------------------------------------------------}

-- testP4 :: String -> P4Interp -> [Pkt] -> SwitchState ->
--   (String -> ([Pkt], SwitchState) -> IO ()) -> IO ()
-- testP4 nm dut pkts st rprt = rprt nm $ runP4 dut pkts st
testP4 :: String -> (String -> ([Pkt], SwitchState) -> IO ()) ->
          P4Interp -> [Pkt] -> SwitchState -> IO ()
testP4 nm rprt dut pkts st = rprt nm $ runP4 dut pkts st

{--------------------------------------------------------------------
    Misc.
--------------------------------------------------------------------}

initState = SwitchState
  { pktsLost    = 0
  , pktsDropped = 0
  }

dummyRprt nm _ = do
  putStrLn $ "Testing " ++ nm ++ "..."
  putStrLn "\tHello, World!"

dummyScript = P4Script
  { ingress = []
  , egress  = Nothing
  }

