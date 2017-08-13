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

import Control.Monad.State
import Data.Bits
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
data Table = Table
  { tableID :: Int
  , rows    :: [TableRow]
  }
data TableRow = TableRow
  { fields  :: Map.Map Field Match
  , params  :: Map.Map Param Value
  , actions :: [Action]
  }
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
data Value    = Addr Integer
              | Etype EthType
  deriving (Show, Eq, Ord)
data EthType  = IP
              | NMB
  deriving (Show, Eq, Ord)

-- | Field matching
--
-- TODO: Will Haskell let us map over all possible value constructors for a type?
--       This manual matching of the function code, below, to the data structures,
--       above, seems clunky and inelegant.
allFieldsMatch :: Map.Map Field Match -> Pkt -> Bool
allFieldsMatch mp pkt = and $ concat [
  [matchVal (mp Map.! fld) (acc (meta   pkt)) | (fld, acc) <- zip [FinPort,  FoutPort, FvlanId] [inPort,  outPort, vlanId]],
  [matchVal (mp Map.! fld) (acc (header pkt)) | (fld, acc) <- zip [FsrcAddr, FdstAddr, FeType]  [srcAddr, dstAddr, eType]] ]

matchVal :: Match -> Value -> Bool
matchVal m v = case m of
  Exact val   -> case val of
    Addr n | Addr m <- v -> m == n
           | otherwise   -> False
    Etype et             -> v == Etype et

  Ternary n m | Addr n' <- v -> (n' .&. m) == (n .&. m)
              | otherwise    -> False

  NoMatch     -> True

-- | Packet abstraction.
data Pkt = Pkt
  { meta    :: Meta
  , header  :: Header
  , payload :: Int     -- For now, just note the size of the payload.
  }

data Meta = Meta
  { inPort  :: Value
  , outPort :: Value
  , vlanId  :: Value
  }

data Header = Header
  { srcAddr :: Value
  , dstAddr :: Value
  , eType   :: Value
  }

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
evalExpr e = True

-- | Process a single packet, via a single table.
--
-- TODO: Incorporate the switch state into this processing.
-- procPkt :: Table -> [Statement] -> [Statement] -> (Pkt, SwitchState) -> (Pkt, SwitchState)
procPkt :: Table -> [Action] -> [Action] -> Unop (Pkt, SwitchState)
procPkt tbl hit miss (pkt, st) = let pkt' = applyActions (actions ++ extras) pkt
                                 in (pkt', st)
  where extras | matched == True = hit
               | otherwise      = miss
        (actions, matched)      = match tbl pkt

applyActions :: [Action] -> Pkt -> Pkt
applyActions _ = id

-- | Attempt to match a packet, using a single table.
match :: Table -> Pkt -> ([Action], Bool)
match tbl pkt | Just row <- safeHead (sortRows matchingRows) = (actions row, True)
              | otherwise                                    = ([],          False)
  where matchingRows = filter (\x -> allFieldsMatch (fields x) pkt) $ rows tbl

sortRows :: Unop [TableRow]
sortRows = id

-- | P4 scripting abstraction.
data P4Script = P4Script { ingress :: [Statement]        -- Must exist; P4 programs start here. ("main()" equiv.)
                         , egress  :: Maybe [Statement]  -- Optional.
                         }

-- | Control flow abstraction.
data Statement = Apply Table       [Action]    [Action]  -- optional hit/miss processing
               | If    Expr        Statement   Statement
               | User  [Statement]                          -- Just allows for modular code writing.

-- | Actions
data Action = Noop
            | Drop
            | Modify
            | Compound [Action]

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
safeHead []     = Nothing
safeHead (x:xs) = Just x

