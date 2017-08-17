{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
  ( mkInterp, mkTable, mkPkt, mkRefPkt
  , P4Interp(runP4), P4Script(..), Table, Pkt, SwitchState(..)
  , Statement(..), Field(..), Match(..), Value(..), EthType(..), Action(..)
  , initSwitchState
  ) where

import Control.Lens
import Control.Monad.State
import Data.Bits
import Data.Function (on)
import Data.List
import Data.Map.Strict ((!), Map, fromList, member, adjust)  -- , keys)
import qualified Data.Map.Strict as Map

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
  }

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

data Field    = FinPort
              | FoutPort
              | FvlanId
              | FsrcAddr
              | FdstAddr
              | FeType
  deriving (Show, Eq, Ord)

data Param    = PinPort
              | PoutPort
              | PvlanId
              | PsrcAddr
              | PdstAddr
              | PeType
  deriving (Show, Eq, Ord)

data Match    = Exact Value
              | Ternary Integer Integer  -- value and mask
              | NoMatch
  deriving (Show, Eq, Ord)

data Value    = Addr  Integer
              | Etype EthType
              | VBool Bool
              | VInt  Int
  deriving (Eq, Ord)

data EthType  = IP
              | NMB
  deriving (Show, Eq, Ord)

-- TODO: Can TH be used to eliminate this? We just want to avoid
--       printing the constructor, in each case.
instance Show Value where
  show (Addr n)   = show n
  show (Etype et) = show et
  show (VBool q)  = show q
  show (VInt n)   = show n

-- | Value accessors.
getAddr :: Value -> Maybe Integer
getAddr (Addr n) = Just n
getAddr _        = Nothing

getEtype :: Value -> Maybe EthType
getEtype (Etype et) = Just et
getEtype _          = Nothing

getVBool :: Value -> Maybe Bool
getVBool (VBool q) = Just q
getVBool _         = Nothing

getVInt :: Value -> Maybe Int
getVInt (VInt n) = Just n
getVInt _        = Nothing

-- | Actions
--
-- TODO: Understand why this had to be moved above the *Pkt* definition,
--       when I introduced lenses and their associated TH splice, below.
--       (Moving the code back to its original location, below the TH
--       splice, yields and "undefined constructor: Action" error, at
--       line 74(ish).)
data Action = Noop
            | Drop
            | Modify Field Value
  deriving (Eq)

-- | Packet abstraction.
--
-- Note: The "_" prefix in the field names is required by *Control.Lens*,
--       to make the Template Haskell splice, below, work.
--
-- TODO: There's an awkward 1:1 correspondence between the fields of
--       the *Pkt* type and the value constructors of the *Field* type.
--       Can anything be done to make this more elegant? (TH?)
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

-- This TH splice builds lenses for all fields in *Pkt* automatically.
$(makeLenses ''Pkt)
$(makeLenses ''SwitchState)

instance Show Pkt where
  show p =
    "Packet:\n" ++
    "\tIn port:\t\t"            ++ show (_inPort p)   ++ "\n" ++
    "\tOut port:\t\t"           ++ show (_outPort p)  ++ "\n" ++
    "\tVLAN ID:\t\t"            ++ show (_vlanId p)   ++ "\n" ++
    "\tDropped:\t\t"            ++ show (_dropped p)  ++ "\n" ++
    "\tSource MAC Addr:\t"      ++ show (_srcAddr p)  ++ "\n" ++
    "\tDestination MAC Addr:\t" ++ show (_dstAddr p)  ++ "\n" ++
    "\tEthernet type:\t\t"      ++ show (_eType p)    ++ "\n" ++
    "\tPayload size:\t\t"       ++ show (_pyldSize p) ++ "\n"

hdrFields = [FsrcAddr, FdstAddr, FeType]  -- Which fields to match on.

-- | Exported packet builder.
--
-- To be used by clients to build packets, as opposed to constructing
-- them manually, in order to protect them from changes in
-- implementation details.
mkPkt :: (Int, Integer , Integer, EthType, Int) -> Pkt
mkPkt (inP, srcA, dstA, eT, pyldSz) =
  Pkt
    { _inPort   = VInt inP
    , _outPort  = VInt 0  -- undefined
    , _vlanId   = VInt 0  -- undefined
    , _dropped  = VBool False
    , _srcAddr  = Addr srcA
    , _dstAddr  = Addr dstA
    , _eType    = Etype eT
    , _pyldSize = VInt pyldSz
    }

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

-- | Converts a P4 script into a behavioral model of a programmed switch.
mkInterp :: P4Script -> P4Interp
mkInterp script = P4Interp $ runState . traverse (procPkt (ingress script ++ xtras))
  where xtras | Just outStmts <- egress script = outStmts
              | otherwise                      = []

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
              | otherwise                                         = pkt
        st'         = if Drop `elem` allActions then over pktsDropped (+ 1) st''
                                           else st''
        st''        = if matched           then over pktsMatched (+ 1) st'''
                                           else st'''
        st'''       = if matched           then over tblHits (bump $ tableID tbl) st''''
                                           else st''''
        st''''      = over portAddrMap (Map.insert (_inPort pkt) (_srcAddr pkt)) st'''''
        st'''''     = over addrPortMap (Map.insert (_srcAddr pkt) (_inPort pkt)) st
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

-- | Misc. utilities.
type Unop a = a -> a

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

