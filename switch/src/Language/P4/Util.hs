{-# LANGUAGE TemplateHaskell #-}

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

import Control.Monad
import Data.Char
import Language.Haskell.TH

-- | Misc. utilities.
type Unop a = a -> a

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | Template Haskell for test case construction
mkTests :: Int -> Q Exp
mkTests n = listE $ map mkTest [1..n]

mkTest :: Int -> Q Exp
mkTest n = [| testP4 $(stringE $ show name) testRprt (Just ($(varE refPkts), $(varE refState)))
              (mkInterp $(varE testScript)) $(varE testPkts) initState |]
  where [name, testScript, testPkts] = map mkTestName ["", "Script", "Pkts"]
        [refState, refPkts]          = map mkRefName  ["State", "Pkts"]
        mkTestName s  = mkName $ "test" ++ show n ++ s
        mkRefName  s  = mkName $ "ref"  ++ show n ++ s

