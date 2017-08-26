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

import Control.Lens hiding (Empty)
import Data.Array hiding ((!))
import qualified Data.Array as A
import Data.Function (on)
import Data.Map.Strict ((!), Map, fromList, member, adjust)  -- , keys)
import qualified Data.Map.Strict as Map
import Data.Sequence hiding (zip, fromList, adjust, sort, filter)
import Data.Tuple (swap)

import Language.P4.TH

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
gNumPorts = 16
ns        = [1..gNumPorts]
initMapList = [ ( VInt n, Addr (80 + fromIntegral n) ) | n <- ns ]
initBuf   = array (1, gNumPorts) [(n, Empty) | n <- [1..gNumPorts]]
initSwitchState = SwitchState
  { _pktsLost    = 0
  , _pktsDropped = 0
  , _pktsMatched = 0
  , _tblHits     = Map.empty
  , _portAddrMap = fromList initMapList
  , _addrPortMap = fromList $ map swap initMapList
  , _ingressBufs = initBuf
  , _egressBufs  = initBuf
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

