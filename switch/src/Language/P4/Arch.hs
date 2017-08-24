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
import Data.Bits
import Data.Function (on)
import Data.List
import Data.Sequence hiding (zip, fromList, adjust, sort, filter)

import Data.Map.Strict ((!), Map, fromList, member, adjust)  -- , keys)
import qualified Data.Map.Strict as Map

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

-- | Packet abstraction.
--
-- Note: The "_" prefix in the field names is required by *Control.Lens*,
--       to make the Template Haskell splice, below, work.
data Pkt = Pkt
  { -- meta-data
    _inPort  :: Value
  , _outPort :: Value
  , _vlanId  :: Value
  , _dropped :: Value
    -- header
  , _srcAddr :: Value
  , _dstAddr :: Value
  , _eType   :: Value
    -- payload
  , _pyldSize :: Value     -- For now, just note the size of the payload.
  } deriving (Eq)

instance Show Pkt where
  show p = unlines
    [
      "Packet:"
    , "\tIn port:\t\t"            ++ show (_inPort p)
    , "\tOut port:\t\t"           ++ show (_outPort p)
    , "\tVLAN ID:\t\t"            ++ show (_vlanId p)
    , "\tDropped:\t\t"            ++ show (_dropped p)
    , "\tSource MAC Addr:\t"      ++ show (_srcAddr p)
    , "\tDestination MAC Addr:\t" ++ show (_dstAddr p)
    , "\tEthernet type:\t\t"      ++ show (_eType p)
    , "\tPayload size:\t\t"       ++ show (_pyldSize p)
    ]

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
    ]

-- | Default initial switch state.
--
-- Users can invoke this, modifying it as necessary using record syntax.
-- And this will be less fragile than having user code do the full
-- initial state creation.
initSwitchState = SwitchState
  { _pktsLost    = 0
  , _pktsDropped = 0
  , _pktsMatched = 0
  , _tblHits     = Map.empty
  , _portAddrMap = Map.empty
  , _addrPortMap = Map.empty
  , _ingressBufs = array (1, 16) [(n, Empty) | n <- [1..16]]
  , _egressBufs  = array (1, 16) [(n, Empty) | n <- [1..16]]
  , _cycleCount  = 0
  , _nextInPort  = 1
  , _trafficMem  = Empty
  }

-- | FIFO abstraction.
type Fifo a = Seq a

push :: a -> Fifo a -> Fifo a
push = (:<|)

pop :: Fifo a -> (Maybe a, Fifo a)
pop f | f' :|> x <- f = (Just x,  f')
      | otherwise     = (Nothing, f)

-- | Polymorphic value type, used throughout.
--
-- Note: Experimentation reveals that this code must remain early on in
--       the source, presumably due to the TH splice used to create its
--       *Show* instance.
data Value    = Addr  Integer
              | Etype EthType
              | VBool Bool
              | VInt  Int
  deriving (Eq, Ord)

data EthType  = IP
              | NMB
  deriving (Show, Eq, Ord)

$(mkShow ''Value)

getVInt :: Value -> Int
getVInt (VInt n) = n
getVInt v        = error $ "Language.P4.Arch.getVInt called with: " ++ show v

$(mkFieldTypes)  -- Makes the *Field* and *Param* types.

-- | Match/action table abstraction.
data Table = Table
  { tableID :: Int
  , rows    :: [TableRow]
  }

data TableRow = TableRow
  { rowID   :: Int
  , fields  :: Map Field Match
  , params  :: Map Param Value
  , actions :: [Action]
  }

instance Eq TableRow where
  (==) = (==) `on` fields

-- | Table row *Ord* instance.
--
-- This works, because it relies on the derived *Ord* instance for
-- *Match*, and derived *Ord* instances for sum types assume that the
-- constructors are given in ascending order. That is, we are able to
-- express our preference for exact matches, simply by listing the
-- *Exact* constructor before the *Ternary* constructor in the
-- definition of the *Match* data type, below.
instance Ord TableRow where
  compare = compare `on` fields

-- | Actions
data Action = Noop
            | Drop
            | Modify Field Value
  deriving (Eq)

data Match    = Exact Value
              | Ternary Integer Integer  -- value and mask
              | NoMatch
  deriving (Show, Eq, Ord)

-- | Template Haskell based automatic lens building.
--
-- Note: This only works, because the field accessor names for both
--       types: *Pkt* and *SwitchState*, have been prefixed with an
--       underscore.
$(makeLenses ''Pkt)
$(makeLenses ''SwitchState)

hdrFields = [FsrcAddr, FdstAddr, FeType]  -- Which fields to match on.

-- | Exported packet builder.
--
-- To be used by clients to build packets, as opposed to constructing
-- them manually, in order to protect them from changes in
-- implementation details.
mkPkt :: (Int, Integer , Integer, EthType, Int) -> Pkt
mkPkt (inP, srcA, dstA, eT, pyldSz) =
  mkRefPkt (inP, 0, 0, False, srcA, dstA, eT, pyldSz)

mkRefPkt :: (Int, Int, Int, Bool, Integer , Integer, EthType, Int) -> Pkt
mkRefPkt (inP, outP, vID, drpd, srcA, dstA, eT, pyldSz) =
  Pkt
    { _inPort   = VInt inP
    , _outPort  = VInt outP
    , _vlanId   = VInt vID
    , _dropped  = VBool drpd
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
     let s'  = over ingressBufs ( \ar -> ar // [(inPortNum, push pkt (ar A.! inPortNum))] ) s
         res = nextInPkt s'
         -- s'' | Just (p, ss) <- res = ss' {_trafficMem = push p' $ _trafficMem ss'}
         s'' | Just (p, ss) <- res = let (p', ss') = runState (procPkt ingStmts p) ss
                                      in over trafficMem (push p') ss'
             | otherwise           = s'
         res' = nextPkt s''
         s''' | Just (p, ss) <- res' = let (p', ss')   = runState (procPkt egStmts p) ss
                                           outPortNum  = getVInt $ view outPort p'
                                           outBufArray = _egressBufs ss'
                                        in ss' { _egressBufs = outBufArray // [(outPortNum, push p' (outBufArray A.! outPortNum))] }
              | otherwise            = s''
         res'' = nextOutPkt s'''
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
nextPkt :: SwitchState -> Maybe (Pkt, SwitchState)
nextPkt s =
  do let (p, f) = pop (_trafficMem s)
     case p of
       Just p' -> Just (p', s { _trafficMem = f })
       _       -> Nothing

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

-- | Attempt to match a packet, using a single table.
match :: Table -> Pkt -> ([Action], Bool)
match tbl pkt | Just row <- safeHead (sort matchingRows) = (actions row, True)
              | otherwise                                = ([],          False)
  where matchingRows = filter (rowMatch pkt) $ rows tbl

-- | P4 scripting abstraction.
data P4Script = P4Script
  { ingress :: [Statement]        -- Must exist; P4 programs start here. ("main()" equiv.)
  , egress  :: Maybe [Statement]  -- Optional.
  }

-- | Control flow abstraction.
data Statement = Apply Table       [Action]    [Action]  -- optional hit/miss processing
               | If    Expr        Statement   Statement
               | User  [Statement]                       -- Just allows for modular code writing.

actionToFunc :: Action -> Unop Pkt
actionToFunc a = case a of
  Noop           -> id
  Drop           -> set dropped $ VBool True
  Modify fld val -> set (fieldToLens fld) val

-- | Result
data MatchResult = Hit
                 | Miss

-- | Stateful object abstraction.
-- *Direct* counters are automatically associated w/ each row of a match/action table,
-- and can be referenced in expressions.
-- *Indirect* counters offer the user the power of association.
data Stateful = Counter
              | Meter
              | Register

-- | Conditional expressions.
type Expr = Bool

