{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}

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
  ( mkArch, mkTable, mkPkt, mkRefPkt, mkRefPkt2, initSwitchState
  , P4Arch(runP4), P4Script(..), Table, Pkt, SwitchState(..)
  , Statement(..), Field(..), Match(..), Value(..), EthType(..), Action(..)
  , Param(..)
  ) where

import Control.Lens hiding (Empty)
import Control.Monad.State
import Data.Array hiding ((!))
import qualified Data.Array as A
import Data.Map.Strict (Map, member, adjust, fromList)
import qualified Data.Map.Strict as Map
import Data.Sequence hiding (zip, fromList, adjust, sort, filter)
import Data.Tuple (swap)

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

data SwitchState = SwitchState
  { _pktsLost    :: Integer          -- Lost, due to buffer overflow.
  , _pktsDropped :: Integer          -- Dropped, due to MAU action.
  , _pktsMatched :: Integer
  , _tblHits     :: Map Int   Int    -- table ID -> # hits
  , _portAddrMap :: Map Value Value  -- port # -> MAC address
  , _addrPortMap :: Map Value Value  -- MAC address -> port #
  , _ingressBufs :: Array Int (Fifo Pkt)   -- one ingress buffer per port
  , _egressBufs  :: Array Int (Fifo Pkt)   -- one egress buffer per port
  , _cycleCount  :: Int              -- Currently, one "cycle" per packet.
  , _nextInPort  :: Int              -- Round robin arbitration for ingress MAU.
  , _trafficMem  :: Fifo Pkt
  } deriving (Eq)

instance Show SwitchState where
  show ss = unlines
    [ "Packets lost:\t\t"     ++ show (_pktsLost    ss)
    , "Packets dropped:\t"    ++ show (_pktsDropped ss)
    , "Packets matched:\t"    ++ show (_pktsMatched ss)
    , "Table hits:\t\t"       ++ show (_tblHits     ss)
    , "Port MAC addresses:\t" ++ show (_portAddrMap ss)
    , "Cycles run:\t\t"       ++ show (_cycleCount  ss)
    , "Required memory:"
    , "\tIngress buffers:\t"    ++ show (_maxLen <$> _ingressBufs ss)
    , "\tEgress buffers:\t\t"   ++ show (_maxLen <$> _egressBufs  ss)
    , "\tTraffic memory:\t\t"   ++ show ((_maxLen . _trafficMem) ss)
    ]

$(makeLenses ''SwitchState)

-- | Default initial switch state.
--
-- Users can invoke this, modifying it as necessary using record syntax.
-- And this will be less fragile than having user code do the full
-- initial state creation.
gNumPorts = 16
ns        = [1..gNumPorts]
initMapList = [ ( VInt n, Addr (80 + fromIntegral n) ) | n <- ns ]
initBuf   = array (1, gNumPorts) [(n, Fifo Empty 0) | n <- [1..gNumPorts]]
initSwitchState = SwitchState
  { _pktsLost    = 0
  , _pktsDropped = 0
  , _pktsMatched = 0
  , _tblHits     = Map.empty
  , _portAddrMap = fromList initMapList
  , _addrPortMap = fromList $ map swap initMapList
  , _ingressBufs = initBuf
  , _egressBufs  = initBuf
  , _cycleCount  = 0
  , _nextInPort  = 1
  , _trafficMem  = Fifo Empty 0
  }

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
--
-- Note: The apparently backwards nature of the monadic processing,
--       below, is necessary to correctly model the flow of packets
--       through the switch, with proper cycle counting. If we did this
--       in the more intuitive straightforward fashion, a packet could
--       get through the switch with zero latency, which isn't
--       realistic.
stepSwitch :: ([Statement], [Statement]) -> Pkt -> State SwitchState (Maybe Pkt)
stepSwitch (ingStmts, egStmts) pkt =
  do s <- get
         -- Attempt to pop next outgoing packet from an egress FIFO.
     let res'' = nextPktAry s egressBufs
         -- Setup return values, base on our popping attempt.
         (pkt', s') | Just (p, ss) <- res'' = (Just (set outTime (VInt $ view cycleCount ss) p),  ss)
                    | otherwise             = (Nothing, s)
         -- Attempt to pop a packet from TM for processing by egress MAU.
         res' = nextPkt s' trafficMem
         -- If successful then process that packet using the egress statements,
         -- and move to the appropriate port's egress buffer.
         s'' | Just (p, ss) <- res' =
                 let (p', ss')     = runState (procPkt egStmts p) ss
                     outPortNum    = getVInt $ view outPort p'
                     outBufArray   = _egressBufs ss'
                  in ss' { _egressBufs =
                             outBufArray //
                               [(outPortNum, push p' (outBufArray A.! outPortNum))] }
             | otherwise            = s'
         -- Attempt to pop next available input packet, for ingress MAU processing.
         res = nextPktAry s'' ingressBufs
         -- If successful then process that packet using the ingress statements,
         -- and move to traffic memory (TM).
         s''' | Just (p, ss) <- res = let (p', ss') = runState (procPkt ingStmts p) ss
                                       in over trafficMem (push p') ss'
              | otherwise           = s''
         -- Push the next input packet into its associated port's ingress FIFO.
         pkt'' = set inTime (VInt $ view cycleCount s) pkt
         s'''' = over ingressBufs ( \ar -> ar // [(inPortNum, push pkt'' (ar A.! inPortNum))] ) s'''
     put $ over cycleCount (+ 1) s''''
     return pkt'
 where inPortNum = getVInt $ view inPort pkt

-- | Try to pop a packet from an array of FIFOs.
--
-- Return immediately upon success.
-- Attempt each FIFO in the array, at most, once.
nextPktAry :: SwitchState -> Lens' SwitchState (Array Int (Fifo Pkt)) -> Maybe (Pkt, SwitchState)
nextPktAry s l =
  do let loop nxt = do
           let (p, f) = pop ((s ^. l) A.! nxt)
           case p of
             Just p' -> Just ( p', s & l %~ (// [(nxt, f)]))
             _       -> if nxt == lst then Nothing
                                      else loop (nxt + 1)
     loop first
 where first       = fst arrayBounds
       lst         = snd arrayBounds
       arrayBounds = bounds $ s ^. l


-- | Attempt to pop a packet from a FIFO, using a lens into SwitchState.
--
-- TODO: Add priority selection.
nextPkt :: SwitchState -> Lens' SwitchState (Fifo Pkt) -> Maybe (Pkt, SwitchState)
nextPkt s l =
  do let (p, f) = pop (s ^. l)
     case p of
       Just p' -> Just ( p', s & l .~ f )
       _       -> Nothing

-- | Stateful processing of a single packet over a sequence of statements.
procPkt :: [Statement] -> Pkt -> State SwitchState Pkt
procPkt stmts pkt = foldl (>>=) (return pkt) (map app $ concatMap mkOps stmts)  -- TODO: combine map & concatMap?
  where app f p =
          do s <- get
             let (p', s') = f (p, s)
             put s'
             return p'

mkOps :: Statement -> [Unop (Pkt, SwitchState)]
mkOps (Apply tbl hit miss) = [applyTbl tbl hit miss]
mkOps (If e s1 s2)         = mkOps $ if evalExpr e then s1 else s2
mkOps (User stmts)         = concatMap mkOps stmts

-- | Apply a single table to a packet.
--
-- Note: The learning of port # to MAC addr. mapping has been removed.
--       This mapping will be included in the pre-simulation switch
--       configuration, going forward.
--
-- TODO: Flag changes in MAC addresses of already mapped ports.
--       Flag duplicate MAC addresses from different ports.
applyTbl :: Table -> [Action] -> [Action] -> Unop (Pkt, SwitchState)
applyTbl tbl hit miss (pkt, st) = (pkt', st')
  where pkt' = foldl (.) id (map actionToFunc allActions) pkt''
        pkt'' | Just v <- Map.lookup (_dstAddr pkt) (_addrPortMap st) = set outPort v        pkt'''
              | otherwise                                             = set outPort (VInt 0) pkt'''
        st'         = if Drop `elem` allActions then over pktsDropped (+ 1) st''
                                           else st''
        st''        = if matched           then over pktsMatched (+ 1) st'''
                                           else st'''
        st'''       = if matched           then over tblHits (bump $ tableID tbl) st
                                           else st
        allActions                  = mActions ++ extras
        (mActions, matched, pkt''') = match tbl pkt
        extras | matched            = hit
               | otherwise          = miss
        bump k m                    = if k `member` m then adjust     (+ 1) k m
                                                      else Map.insert k     1 m

