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
type Switch = [Table] -> SwitchState -> [Pkt] -> ([Pkt], SwitchState)
data SwitchState = SwitchState
  { pktsLost :: Integer
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

getVal :: Value -> a
getVal v = case v of
  Addr  n  -> n
  Etype et -> et
  VBool q  -> q
  VInt  n  -> n

data EthType  = IP
              | NMB
  deriving (Show, Eq, Ord)

-- | Packet abstraction.
--
-- TODO: Talk to Conal about my choice of making everything a *Value*.
--       I did this, in order to avoid introducing existential type
--       quantification, in my fieldToPktAcc function, below. But, I
--       need to make sure I'm thinking about this correctly.
data Pkt = Pkt
  { -- meta-data
    inPort  :: Value
  , outPort :: Value
  , vlanId  :: Value
  , dropped :: Value
    -- header
  , srcAddr :: Value
  , dstAddr :: Value
  , eType   :: Value
    -- payload
  , pyldSize :: Value     -- For now, just note the size of the payload.
  }
hdrFields = [FsrcAddr, FdstAddr, FeType]

-- | Field matching
--
-- TODO: Will Haskell let us map over all possible value constructors for a type?
--       This manual matching of the function code, below, to the data structures,
--       above, seems clunky and inelegant.
{-
allFieldsMatch :: Map.Map Field Match -> Pkt -> Bool
allFieldsMatch mp pkt = and $ concat [
  [matchVal (mp Map.! fld) (acc (meta   pkt)) | (fld, acc) <- zip [FinPort,  FoutPort, FvlanId] [inPort,  outPort, vlanId]],
  [matchVal (mp Map.! fld) (acc (header pkt)) | (fld, acc) <- zip [FsrcAddr, FdstAddr, FeType]  [srcAddr, dstAddr, eType]] ]
-}

rowMatch :: Pkt -> TableRow -> Bool
rowMatch pkt row = and
  [matchVal (fields row Map.! fld) (acc pkt) | (fld, acc) <- zip hdrFields $ map fieldToPktAcc hdrFields]

matchVal :: Match -> Value -> Bool
matchVal mtch v = case mtch of
  NoMatch   -> True
  Exact val -> v == val
  Ternary n m | Addr n' <- v -> (n' .&. m) == (n .&. m)
              | otherwise    -> False  -- error?

fieldToPktAcc :: Field -> Pkt -> Value
fieldToPktAcc fld = case fld of
  FinPort  -> inPort
  FoutPort -> outPort
  FvlanId  -> vlanId
  FsrcAddr -> srcAddr
  FdstAddr -> dstAddr
  FeType   -> eType

-- | Converts a P4 script into a behavioral model of a programmed switch.
mkInterp :: P4Script -> P4Interp
mkInterp = undefined
-- sequence :: [m a] -> m [a]
-- stmts               :: [Statement]
-- traverse            :: (a -> f b) -> t a -> f (t b)
-- traverse mkOp       :: t Statement -> State SwitchState (t (Unop Pkt))
-- traverse mkOp stmts :: State SwitchState [Unop Pkt] = StateT SwitchState [Unop Pkt] Identity
-- runState (traverse mkOp stmts) :: SwitchState -> ([Unop Pkt], SwitchState)
--
-- wanted :: SwitchState -> [Pkt] -> ([Pkt], SwitchState)
--
-- try for :: SwitchState -> Pkt -> (Pkt, SwitchState)
-- if f :: Pkt -> SwitchState -> (Pkt, SwitchState) = Pkt -> runState (State SwitchState Pkt)
-- then (flip f) has the type we want.
-- mkInterp script = P4Interp $ \ tbls -> runState (traverse (foldM mkOp [] stmts))
--   where stmts | egress script == Just outStmts = inStmts ++ outStmts
--               | otherwise                     = inStmts
--         inStmts = ingress script
--   where procSteps | egress script == Just outScript = inProc ++ fmap mkOp outScript
--                   | otherwise                      = inProc
--         inProc = fmap mkOp $ ingress script

mkOp :: Statement -> [Unop (Pkt, SwitchState)]
-- mkOp :: Statement -> State SwitchState (Unop Pkt)
mkOp stmt = case stmt of
  Apply tbl hit miss -> [procPkt tbl hit miss]

  If e s1 s2         -> if evalExpr e == True then mkOp s1 else mkOp s2

  User stmts         -> concat $ map mkOp stmts

-- | Evaluate an Boolean expression.
evalExpr :: Expr -> Bool
evalExpr _ = True

-- | Process a single packet, via a single table.
--
-- TODO: Incorporate the switch state into this processing.
procPkt :: Table -> [Action] -> [Action] -> Unop (Pkt, SwitchState)
procPkt tbl hit miss (pkt, st) = let pkt' = foldl (.) id (map actionToFunc (mActions ++ extras)) pkt
                                 in (pkt', st)
  where extras | matched == True = hit
               | otherwise      = miss
        (mActions, matched)     = match tbl pkt

-- | Attempt to match a packet, using a single table.
match :: Table -> Pkt -> ([Action], Bool)
match tbl pkt | Just row <- safeHead (sort matchingRows) = (actions row, True)
              | otherwise                                = ([],          False)
  where matchingRows = filter (rowMatch pkt) $ rows tbl

-- | P4 scripting abstraction.
data P4Script = P4Script { ingress :: [Statement]        -- Must exist; P4 programs start here. ("main()" equiv.)
                         , egress  :: Maybe [Statement]  -- Optional.
                         }

-- | Control flow abstraction.
data Statement = Apply Table       [Action]    [Action]  -- optional hit/miss processing
               | If    Expr        Statement   Statement
               | User  [Statement]                       -- Just allows for modular code writing.

-- | Actions
data Action = Noop
            | Drop
            | Modify Field Value

actionToFunc :: Action -> Unop Pkt
actionToFunc a = case a of
  Noop           -> id
  Drop           -> set dropped True
  Modify fld val -> set (fieldToPktAcc fld) (getVal val)

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

