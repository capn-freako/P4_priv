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
-- Module      :  Language.P4.Interp
-- Copyright   :  (c) 2017 David Banas
-- License     :  BSD3
--
-- Maintainer  :  capn.freako@gmail.com
-- Stability   :  experimental
--
-- Behavioral modeling of P4-programmable switches.
----------------------------------------------------------------------

module Language.P4.Interp
  ( mkInterp, mkTable, mkPkt, mkRefPkt, initSwitchState
  , P4Interp(runP4), P4Script(..), Table, Pkt, SwitchState(..)
  , Statement(..), Field(..), Match(..), Value(..), EthType(..), Action(..)
  , Param(..)
  , module Language.P4.TH
  ) where

import Control.Lens hiding (Empty, (<|))
import Control.Monad.State
import Data.Map.Strict (Map, member, adjust)
import qualified Data.Map.Strict as Map

import Language.P4.Types
import Language.P4.Util
import Language.P4.TH

-- | Behavioral P4 interpreter.
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
-- Note: The *P4Interp* data structure should *not* be constructed
--       manually by the user. Instead, the *mkInterp()* helper function
--       should be called with a *P4Script*, constructed using the
--       various data structures and helper functions provided, below.
--       (See *test/Examples.hs*.)
newtype P4Interp = P4Interp { runP4 :: Switch }

-- | Type definition of a configured switch.
type Switch = [Pkt] -> SwitchState -> ([Pkt], SwitchState)

data SwitchState = SwitchState
  { _pktsLost    :: Integer
  , _pktsDropped :: Integer
  , _pktsMatched :: Integer
  , _tblHits     :: Map Int   Int    -- table ID -> # hits
  , _portAddrMap :: Map Value Value  -- port # -> MAC address
  , _addrPortMap :: Map Value Value  -- MAC address -> port #
  } deriving (Eq)

instance Show SwitchState where
  show ss = unlines
    [ "Packets lost:\t\t"     ++ show (_pktsLost    ss)
    , "Packets dropped:\t"    ++ show (_pktsDropped ss)
    , "Packets matched:\t"    ++ show (_pktsMatched ss)
    , "Table hits:\t\t"       ++ show (_tblHits     ss)
    , "Port MAC addresses:\t" ++ show (_portAddrMap ss)
    ]

--- | Default initial switch state.
---
--- Users can invoke this, modifying it as necessary using record syntax.
--- And this will be less fragile than having user code do the full
--- initial state creation.
initSwitchState = SwitchState
  { _pktsLost    = 0
  , _pktsDropped = 0
  , _pktsMatched = 0
  , _tblHits     = Map.empty
  , _portAddrMap = Map.empty
  , _addrPortMap = Map.empty
  }

$(makeLenses ''SwitchState)

-- | Converts a P4 script into a behavioral model of a programmed switch.
mkInterp :: P4Script -> P4Interp
mkInterp script = P4Interp $ runState . traverse (procPkt (ingress script ++ xtras))
  where xtras | Just outStmts <- egress script = outStmts
              | otherwise                      = []

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

--- | Apply a single table to a packet.
---
--- TODO: Flag changes in MAC addresses of already mapped ports.
---       Flag duplicate MAC addresses from different ports.
applyTbl :: Table -> [Action] -> [Action] -> Unop (Pkt, SwitchState)
applyTbl tbl hit miss (pkt, st) = (pkt', st')
  where pkt' = foldl (.) id (map actionToFunc allActions) pkt''
        pkt'' | Just v <- Map.lookup (_dstAddr pkt) (_addrPortMap st) = set outPort v pkt
              | otherwise                                             = pkt
        st'         = if Drop `elem` allActions then over pktsDropped (+ 1) st''
                                           else st''
        st''        = if matched           then over pktsMatched (+ 1) st'''
                                           else st'''
        st'''       = if matched           then over tblHits (bump $ tableID tbl) st''''
                                           else st''''
        st''''      = over portAddrMap (Map.insert (_inPort  pkt) (_srcAddr pkt)) st'''''
        st'''''     = over addrPortMap (Map.insert (_srcAddr pkt) (_inPort pkt))  st
        allActions             = mActions ++ extras
        (mActions, matched, _) = match tbl pkt
        extras | matched       = hit
               | otherwise     = miss
        bump k m               = if k `member` m then adjust     (+ 1) k m
                                                 else Map.insert k     1 m

