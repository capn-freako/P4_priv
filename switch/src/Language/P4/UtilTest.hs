{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module      :  Language.P4.UtilTest
-- Copyright   :  (c) 2017 David Banas
-- License     :  BSD3
--
-- Maintainer  :  capn.freako@gmail.com
-- Stability   :  experimental
--
-- Testing module, for Util.hs.
--
-- Use: stack ghc -- UtilTest.hs -ddump-splices
----------------------------------------------------------------------

module Language.P4.UtilTest where

import Language.P4.Util

test = $(mkTests 2)

