{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}

{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -ddump-splices #-}

----------------------------------------------------------------------
-- |
-- Module      :  Language.P4.Arch
-- Copyright   :  (c) 2017 David Banas
-- License     :  BSD3
--
-- Maintainer  :  capn.freako@gmail.com
-- Stability   :  experimental
--
-- Architectural modeling of P4-programmable switches.
----------------------------------------------------------------------

module Language.P4.Arch
  ( mkArch, mkTable, mkPkt, mkRefPkt, initSwitchState
  , P4Arch(runP4), P4Script(..), Table, Pkt, SwitchState(..)
  , Statement(..), Field(..), Match(..), Value(..), EthType(..), Action(..)
  , Param(..)
  ) where

import Control.Lens hiding (Empty)
import Control.Monad.State
import Data.Array hiding ((!))
import qualified Data.Array as A

import Language.P4.Types
import Language.P4.Util

-- | Architectural P4 interpreter.
--
-- Note: Parser modeling has been omitted. Therefore, test packet data
--       fields must coincide with table reads definition fields.
--       (See *test/Examples.hs*, for packet construction guidance.)
--
-- Note: We're modeling only the data plane, here; not the control
--       plane. Therefore, tables are static, read-only entities, in
--       this context.
--       There is no provision in this module for creating tables, nor
--       for inserting/deleting rows to/from them. Instead, tables are
--       expected to be created by the testing infrastructure, and enter
--       the simulation flow, only by virtue of the *Apply* constructor
--       for the *Statement* type.
--       (See *test/Examples.hs*, for table construction guidance.)
--
-- Note: The *P4Arch* data structure should *not* be constructed
--       manually by the user. Instead, the *mkArch()* helper function
--       should be called with a *P4Script*, and the necessary
--       architectural parameterization.
--       (See *test/Examples.hs*.)
newtype P4Arch = P4Arch { runP4 :: Switch }

-- | Type definition of a configured switch.
type Switch = [Pkt] -> SwitchState -> ([Maybe Pkt], SwitchState)

-- | Converts a P4 script into an architectural model of a programmed switch.
mkArch :: P4Script -> P4Arch
mkArch script = P4Arch $ runState . traverse (stepSwitch (ingress script, egStmts))
  where egStmts | Just outStmts <- egress script = outStmts
                | otherwise                      = []

-- | Step the switch through one "cycle".
--
-- Currently, a cycle is equivalent to pulling one packet from the input
-- packet list.
-- TODO: Change this, to support simultaneous arrival of packets at
--       multiple ports.
stepSwitch :: ([Statement], [Statement]) -> Pkt -> State SwitchState (Maybe Pkt)
stepSwitch (ingStmts, egStmts) pkt =
  do s <- get
         -- Push the next input packet into its associated port's ingress FIFO.
     let s'  = over ingressBufs ( \ar -> ar // [(inPortNum, push pkt (ar A.! inPortNum))] ) s
         -- Attempt to pop next available input packet, for ingress MAU processing.
         res = nextInPkt s'
         -- If successful then process that packet using the ingress statements,
         -- and move to traffic memory (TM).
         s'' | Just (p, ss) <- res = let (p', ss') = runState (procPkt ingStmts p) ss
                                      in over trafficMem (push p') ss'
             | otherwise           = s'
         -- Attempt to pop a packet from TM for processing by egress MAU.
         res' = nextPkt s''
         -- If successful then process that packet using the egress statements,
         -- and move to the appropriate port's egress buffer.
         s''' | Just (p, ss) <- res' =
                   let (p', ss')     = runState (procPkt egStmts p) ss
                       outPortNum    = getVInt $ view outPort p'
                       outBufArray   = _egressBufs ss'
                    in ss' { _egressBufs =
                               outBufArray //
                                 [(outPortNum, push p' (outBufArray A.! outPortNum))] }
                    -- in if outPortNum /= 0
                    --       then ss' { _egressBufs =
                    --                    outBufArray //
                    --                      [(outPortNum, push p' (outBufArray A.! outPortNum))] }
                    --       else ss'
              | otherwise            = s''
         -- Attempt to pop next outgoing packet from an egress FIFO.
         res'' = nextOutPkt s'''
         -- If successful then return the popped packet.
         (pkt', s'''') | Just (p, ss) <- res'' = (Just p,  ss)
                       | otherwise             = (Nothing, s''')
     put s''''
     return pkt'


 where inPortNum  = getVInt $ view inPort  pkt

-- | Scan the ports for new input packets, in order, returning the first one found.
nextInPkt :: SwitchState -> Maybe (Pkt, SwitchState)
nextInPkt s =
  do let loop nxt = do
           let (p, f) = pop $ _ingressBufs s A.! nxt
           case p of
             Just p' -> Just ( p', s { _ingressBufs = _ingressBufs s // [(nxt, f)]
                                     , _nextInPort  = bumpPortNum nxt } )
             _       -> if nxt == prev then Nothing
                                       else loop (nxt + 1)
     loop next
       where first         = fst inBufAryBnds
             lst           = snd inBufAryBnds
             inBufAryBnds  = (bounds . _ingressBufs) s
             next          = _nextInPort s
             prev          = if next == first then lst
                                              else next - 1
             bumpPortNum n = if n == lst then first
                                         else n + 1

-- | Scan the ports for an output packet waiting to exit.
nextOutPkt :: SwitchState -> Maybe (Pkt, SwitchState)
nextOutPkt s =
  do let loop nxt = do
           let (p, f) = pop $ _egressBufs s A.! nxt
           case p of
             Just p' -> Just ( p', s { _egressBufs = _egressBufs s // [(nxt, f)] } )
             _       -> if nxt == lst then Nothing
                                      else loop (nxt + 1)
     loop first
       where first       = fst arrayBounds
             lst         = snd arrayBounds
             arrayBounds = (bounds . _egressBufs) s

-- | Get the next packet waiting in traffic memory, if available.
--
-- TODO: Add priority selection.
nextPkt :: SwitchState -> Maybe (Pkt, SwitchState)
nextPkt s =
  do let (p, f) = pop (_trafficMem s)
     case p of
       Just p' -> Just (p', s { _trafficMem = f })
       _       -> Nothing

