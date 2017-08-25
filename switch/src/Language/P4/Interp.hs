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

import Control.Monad.State

import Language.P4.Types
import Language.P4.Util
import Language.P4.TH (mkTests)

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

-- | Converts a P4 script into a behavioral model of a programmed switch.
mkInterp :: P4Script -> P4Interp
mkInterp script = P4Interp $ runState . traverse (procPkt (ingress script ++ xtras))
  where xtras | Just outStmts <- egress script = outStmts
              | otherwise                      = []

