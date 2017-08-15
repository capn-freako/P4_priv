{-# LANGUAGE RankNTypes #-}

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

module Language.P4.Interp where

import Control.Lens
import Control.Monad.State
import Data.Bits
import Data.Function (on)
import Data.List
import Data.Map.Strict ((!), keys, Map)

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
  }

instance Show SwitchState where
  show ss = unlines
    [ "Packets lost:\t\t" ++ show (_pktsLost ss)
    , "Packets dropped:\t" ++ show (_pktsDropped ss)
    ]

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
  compare rx ry = foldl mappend EQ [compare (x!k) (y!k) | k <- keys x]
    where x = fields rx
          y = fields ry

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
  }

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
applyTbl :: Table -> [Action] -> [Action] -> Unop (Pkt, SwitchState)
applyTbl tbl hit miss (pkt, st) = (pkt', st')
  where pkt' = foldl (.) id (map actionToFunc allActions) pkt
        st'  = if Drop `elem` allActions then over pktsDropped (+ 1) st
                                    else st
        allActions          = mActions ++ extras
        (mActions, matched) = match tbl pkt
        extras | matched    = hit
               | otherwise  = miss

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

