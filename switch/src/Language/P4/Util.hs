{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}

----------------------------------------------------------------------
-- |
-- Module      :  Language.P4.Util
-- Copyright   :  (c) 2017 David Banas
-- License     :  BSD3
--
-- Maintainer  :  capn.freako@gmail.com
-- Stability   :  experimental
--
-- General utilities used by various members of the p4-switch package.
----------------------------------------------------------------------

module Language.P4.Util where

import Control.Lens hiding (Empty)
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.List
import Data.Map.Strict ((!), fromList, member, adjust)
import qualified Data.Map.Strict as Map

import Language.P4.Types

-- | Misc. utilities.
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

procPkt :: [Statement] -> Pkt -> State SwitchState Pkt
procPkt stmts pkt = foldl (>>=) (return pkt) (map app $ concatMap mkOps stmts)  -- TODO: combine map & concatMap?
  where app f = \p ->
          do s <- get
             let (p', s') = f (p, s)
             put s'
             return p'

mkOps :: Statement -> [Unop (Pkt, SwitchState)]
mkOps (Apply tbl hit miss) = [applyTbl tbl hit miss]
mkOps (If e s1 s2)         = mkOps $ if evalExpr e then s1 else s2
mkOps (User stmts)         = concatMap mkOps stmts

-- | Evaluate an Boolean expression.
--
-- TODO: Complete this.
evalExpr :: Expr -> Bool
evalExpr _ = True

-- | Apply a single table to a packet.
--
-- TODO: Flag changes in MAC addresses of already mapped ports.
--       Flag duplicate MAC addresses from different ports.
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
        allActions          = mActions ++ extras
        (mActions, matched) = match tbl pkt
        extras | matched    = hit
               | otherwise  = miss
        bump k m            = if k `member` m then adjust     (+ 1) k m
                                              else Map.insert k     1 m

actionToFunc :: Action -> Unop Pkt
actionToFunc a = case a of
  Noop           -> id
  Drop           -> set dropped $ VBool True
  Modify fld val -> set (fieldToLens fld) val

-- | Attempt to match a packet, using a single table.
match :: Table -> Pkt -> ([Action], Bool)
match tbl pkt | Just row <- safeHead (sort matchingRows) = (actions row, True)
              | otherwise                                = ([],          False)
  where matchingRows = filter (rowMatch pkt) $ rows tbl

-- | Exported packet builder.
--
-- To be used by clients to build packets, as opposed to constructing
-- them manually, in order to protect them from changes in
-- implementation details.
mkPkt :: (Int, Integer , Integer, EthType, Int) -> Pkt
mkPkt (inP, srcA, dstA, eT, pyldSz) =
  mkRefPkt (inP, 0, 0, False, 0, 0, srcA, dstA, eT, pyldSz)

mkRefPkt :: (Int, Int, Int, Bool, Int, Int, Integer , Integer, EthType, Int) -> Pkt
mkRefPkt (inP, outP, vID, drpd, inT, outT, srcA, dstA, eT, pyldSz) =
  Pkt
    { _inPort   = VInt inP
    , _outPort  = VInt outP
    , _vlanId   = VInt vID
    , _dropped  = VBool drpd
    , _inTime   = VInt inT
    , _outTime  = VInt outT
    , _srcAddr  = Addr srcA
    , _dstAddr  = Addr dstA
    , _eType    = Etype eT
    , _pyldSize = VInt pyldSz
    }

-- | Exported table builder.
mkTable :: (Int, [Field], [Param], [(Int,[Match],[Value],[Action])]) -> Table
mkTable (tblID, flds, pNames, tblRows) =
  Table
    { tableID = tblID
    , rows    = map (mkRow flds pNames) tblRows
    }

mkRow :: [Field] -> [Param] -> (Int,[Match],[Value],[Action]) -> TableRow
mkRow flds parms (rID, matches, pVals, acts) =
  TableRow
    { rowID   = rID
    , fields  = fillMissingFields $ fromList $ zip flds   matches
    , params  = fromList $ zip parms  pVals
    , actions = acts
    }
  where fillMissingFields m = foldl addIfMissing m hdrFields
        addIfMissing m f    = if f `member` m then m else Map.insert f NoMatch m

-- | Row matching
rowMatch :: Pkt -> TableRow -> Bool
rowMatch pkt row = and
  [matchVal (fields row ! fld) (acc pkt) | (fld, acc) <- zip hdrFields $ map (view . fieldToLens) hdrFields]

matchVal :: Match -> Value -> Bool
matchVal NoMatch       _                = True
matchVal (Exact val)   v                = v == val
matchVal (Ternary n m) v | Addr n' <- v = (n' .&. m) == (n .&. m)
                         | otherwise    = False  -- error?

-- TODO: There has to be a way to use TH to eliminate this inelegance.
--       Note that, in every case, the associated terms differ only
--       by "F".
fieldToLens :: Field -> Lens' Pkt Value
fieldToLens fld = case fld of
  FinPort  -> inPort
  FoutPort -> outPort
  FvlanId  -> vlanId
  FsrcAddr -> srcAddr
  FdstAddr -> dstAddr
  FeType   -> eType

