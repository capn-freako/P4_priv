{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module      :  Language.P4.TH
-- Copyright   :  (c) 2017 David Banas
-- License     :  BSD3
--
-- Maintainer  :  capn.freako@gmail.com
-- Stability   :  experimental
--
-- Template Haskell functions.
--
-- It would be preferrable to have these live with the data types they
-- help build, but GHC doesn't allow this.
----------------------------------------------------------------------

module Language.P4.TH where

import Language.Haskell.TH
import Control.Monad

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

-- | (TH) Simple alternative to derived Show instances.
--
-- Just omits the data constructor.
mkShow :: Name -> Q [Dec]
mkShow typName = do
  t@(TyConI (DataD _ _ _ _ constructors _)) <- reify typName  -- Get all the information on the type.
  let func_name = mkName "show"
  let var_name  = mkName "x"
  let clause_decs = map ( \c@(NormalC nm _) ->
                            Clause [ConP (simpleName nm) [VarP var_name]]
                                   (NormalB (AppE (VarE func_name) (VarE var_name)))
                                   []
                        )
                        constructors
  return [InstanceD Nothing [] (AppT (ConT (mkName "Show")) (ConT (simpleName typName))) [FunD func_name clause_decs]]

simpleName :: Name -> Name
simpleName nm =
   let s = nameBase nm
   in case dropWhile (/=':') s of
        []          -> mkName s
        _:[]        -> mkName s
        _:t         -> mkName t

-- | (TH) Builder for types and helper functions, which share a common
-- set of field accessors.
mkFieldTypes :: Q [Dec]
mkFieldTypes = forM ["Field","Param"] $ \typName -> mkFieldType typName

mkFieldType :: String -> Q Dec
mkFieldType typName = dataD (cxt []) (mkName typName) [] Nothing constructors [derivings]
  where constructors = map (flip normalC [] . mkName . (head typName :)) fldNms
        fldNms       = ["inPort", "outPort", "vlanId", "srcAddr", "dstAddr", "eType"]
        derivings    = derivClause Nothing $ map (conT . mkName) ["Show","Eq","Ord"]

