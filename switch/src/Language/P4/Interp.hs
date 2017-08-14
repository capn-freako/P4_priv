{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}

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
import Data.List
import qualified Data.Map.Strict as Map

-- | Behavioral P4 interpreter.
--
-- Should return its match/action table depth, after construction.
--
-- Note: Parser modeling has been omitted. Therefore, test packet data
--       fields must coincide with table reads definition fields.
--       A helper function, mkPkt(), has been provided, to facillitate
--       this.
--
-- Note: We're modeling only the data plane, here; not the control
--       plane. Therefore, tables are static, read-only entities, in
--       this context, and must be passed into the *runP4* function.
--       There is no provision in this module for creating tables, nor
--       for inserting/deleting rows to/from them.
--
-- Note: The *P4Interp* data structure should *not* be constructed
--       manually by the user. Instead, the *mkInterp()* helper function
--       should be called with a *P4Script*, constructed using the
--       various data structures and helper functions provided, below.
--       (See *test/Examples.hs*.)
newtype P4Interp = P4Interp { runP4 :: Switch }

-- | Type definition of a configured switch.
--
-- Input packet stream + initial switch state and pre-configured match/action tables,
-- to output packet stream + final switch state.
type Switch = [Pkt] -> SwitchState -> ([Pkt], SwitchState)
data SwitchState = SwitchState
  { pktsLost    :: Integer
  , pktsDropped :: Integer
  }

-- | Match/action table abstraction.
--
-- TODO: The *Ord* instance needs work. Because table (i.e. - Map)
--       construction sorts the keys as they're inserted, we can't use
--       insertion order to indicate priority. And, what really needs
--       to happen is that exact matches take priority over ternary
--       matches.
data Table = Table
  { tableID :: Int
  , rows    :: [TableRow]
  }

data TableRow = TableRow
  { fields  :: Map.Map Field Match
  , params  :: Map.Map Param Value
  , actions :: [Action]
  }

instance Eq TableRow where
  rx == ry = fields rx == fields ry

instance Ord TableRow where
  rx <= ry = if xType == yType then x' <= y' else xType <= yType
    where xType = x Map.! fld
          yType = y Map.! fld
          x'    = Map.delete fld x
          y'    = Map.delete fld y
          fld   = head $ Map.keys x
          x     = fields rx
          y     = fields ry

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
  deriving (Show, Eq, Ord)

data EthType  = IP
              | NMB
  deriving (Show, Eq, Ord)

-- | Value accessors.
--
-- TODO: This approach is reminiscent of the *Visitor* class, used to
--       implement polymorphic recursion in C++/Boost. It seems like
--       Haskell, with its accomodation of existential type
--       quantification, ought to be able to do this in a more elegant
--       fashion, but I lack experience in its use. Talk to Conal.
--       (See, also, *Packet abstraction*, below.)
getAddr :: Value -> Maybe Integer
getAddr val = case val of
  Addr n -> Just n
  _      -> Nothing

getEtype :: Value -> Maybe EthType
getEtype val = case val of
  Etype et -> Just et
  _        -> Nothing

getVBool :: Value -> Maybe Bool
getVBool val = case val of
  VBool q -> Just q
  _       -> Nothing

getVInt :: Value -> Maybe Int
getVInt val = case val of
  VInt n -> Just n
  _      -> Nothing

-- | Actions
--
-- TODO: Understand why this had to be moved above the *Pkt* definition,
--       when I introduced lenses and their associated TH splice, below.
--       (Moving the code back to its original location, below the TH
--       splice, yields and "undefined constructor: Action" error, at
--       line 74.)
data Action = Noop
            | Drop
            | Modify Field Value

-- | Packet abstraction.
--
-- Note: The "_" prefix in the field names is required by *Control.Lens*,
--       to make the Template Haskell splice, below, work.
--
-- TODO: Talk to Conal about my choice of making everything a *Value*.
--       I did this, in order to avoid introducing existential type
--       quantification, in my *fieldToLens* function, below. But, I
--       need to make sure I'm thinking about this correctly. Maybe,
--       review the potential use cases and caveats of existential
--       types, in Haskell.
--
-- TODO: There's an awkward 1:1 correspondence between the fields of
--       the *Pkt* type and the value constructors of the *Field* type.
--       Can anything be done to make this more elegant?
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

hdrFields = [FsrcAddr, FdstAddr, FeType]  -- Which fields to match on.

-- | Row matching
rowMatch :: Pkt -> TableRow -> Bool
rowMatch pkt row = and
  [matchVal (fields row Map.! fld) (acc pkt) | (fld, acc) <- zip hdrFields $ map (view . fieldToLens) hdrFields]

matchVal :: Match -> Value -> Bool
matchVal mtch v = case mtch of
  NoMatch   -> True
  Exact val -> v == val
  Ternary n m | Addr n' <- v -> (n' .&. m) == (n .&. m)
              | otherwise    -> False  -- error?

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
mkInterp script = P4Interp $ \pkts -> runState (traverse (procPkt stmts) pkts)
  where stmts | Just outStmts <- egress script = inStmts ++ outStmts
              | otherwise                      = inStmts
        inStmts = ingress script

procPkt :: [Statement] -> Pkt -> State SwitchState Pkt
procPkt stmts pkt = foldl (>>=) (return pkt) (map app $ concatMap mkOps stmts)
  where app f = \p ->
          do s <- get
             let (p', s') = f (p, s)
             put s'
             return p'

mkOps :: Statement -> [Unop (Pkt, SwitchState)]
mkOps stmt = case stmt of
  Apply tbl hit miss -> [applyTbl tbl hit miss]
  If e s1 s2         -> if evalExpr e then mkOps s1 else mkOps s2
  User stmts         -> concatMap mkOps stmts

-- | Evaluate an Boolean expression.
evalExpr :: Expr -> Bool
evalExpr _ = True

-- | Apply a single table to a packet.
--
-- TODO: Incorporate the switch state into this processing.
applyTbl :: Table -> [Action] -> [Action] -> Unop (Pkt, SwitchState)
applyTbl tbl hit miss (pkt, st) = let pkt' = foldl (.) id (map actionToFunc (mActions ++ extras)) pkt
                                 in (pkt', st)
  where extras | matched    = hit
               | otherwise  = miss
        (mActions, matched) = match tbl pkt

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

