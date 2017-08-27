{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module      :  Language.P4.Types
-- Copyright   :  (c) 2017 David Banas
-- License     :  BSD3
--
-- Maintainer  :  capn.freako@gmail.com
-- Stability   :  experimental
--
-- Common type definitions used in modeling P4-programmable switches.
----------------------------------------------------------------------

module Language.P4.Types where

import Prelude hiding (length)
import Control.Lens hiding (Empty, (<|))
import Data.Array hiding ((!))
import qualified Data.Array as A
import Data.Function (on)
import Data.Map.Strict ((!), Map, fromList, member, adjust)  -- , keys)
import qualified Data.Map.Strict as Map
import Data.Sequence hiding (zip, fromList, adjust, sort, filter)
import Data.Tuple (swap)

import Language.P4.TH

-- | FIFO abstraction.
data Fifo a = Fifo
  { _getData :: Seq a
  , _maxLen  :: Int
  } deriving (Eq)

$(makeLenses ''Fifo)

push :: a -> Fifo a -> Fifo a
push x f = f' & maxLen .~ l
  where f' = f & getData %~ (x <|)
        l  = max (f' ^. maxLen) (length $ f' ^. getData)

pop :: Fifo a -> (Maybe a, Fifo a)
pop f | f' :|> x <- f ^. getData = (Just x,  f & getData .~ f')
      | otherwise                = (Nothing, f)

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
  , _inTime  :: Value
  , _outTime :: Value
  , _tableHits :: [(Int, Int)]  -- Table ID, Row ID
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
    , "\tInput time:\t\t"         ++ show (_inTime p)
    , "\tOutput time:\t\t"        ++ show (_outTime p)
    , "\tTable hits:\t\t"         ++ show (_tableHits p)
    , "\tSource MAC Addr:\t"      ++ show (_srcAddr p)
    , "\tDestination MAC Addr:\t" ++ show (_dstAddr p)
    , "\tEthernet type:\t\t"      ++ show (_eType p)
    , "\tPayload size:\t\t"       ++ show (_pyldSize p)
    ]

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

hdrFields = [FsrcAddr, FdstAddr, FeType]  -- Which fields to match on.

-- | P4 scripting abstraction.
data P4Script = P4Script
  { ingress :: [Statement]        -- Must exist; P4 programs start here. ("main()" equiv.)
  , egress  :: Maybe [Statement]  -- Optional.
  }

-- | Control flow abstraction.
data Statement = Apply Table       [Action]    [Action]  -- optional hit/miss processing
               | If    Expr        Statement   Statement
               | User  [Statement]                       -- Just allows for modular code writing.

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

-- | Misc.
type Unop a = a -> a

