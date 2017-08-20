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

-- | (TH) Simple alternative to derived Show instances.
--
-- Just omits the data constructor.
--
-- Some useful (?) stuff:
--
-- ghci> runQ [d| f a = 1 |]
-- [FunD f [Clause [VarP a_4] (NormalB (LitE (IntegerL 1))) []]]
--
-- In some rare cases you don’t need unique variable name to be generated; instead, you need to specify the exact name of variable which must be generated in output code. For these cases, there is a (pure) function mkName::String->Name. There is also corresponding helper function “dyn s = return (VarE (mkName s))”, which returns Exp representing variable with exact the given name.
--
-- summ n = summ' n [| 0 |]
-- summ' 0 code = code
-- summ' n code = [| \x -> $(summ' (n-1) [|$code+x|] ) |]
-- This definition generates lambda form with n parameters which sums up all its arguments, for example $(summ 3) -> (\x1 -> \x2 -> \x3 -> 0+x1+x2+x3). Please draw attention that
--
-- Only [p| ... |] quotation doesn’t rename variables this pattern introduces. Instead, TH provides function genpat, which generates unique pattern from the given one.
--
-- If you need to use identifiers, available at place of splicing call, use the $(dyn "str") form.
--
-- Also inside quotation brackets you can use local variables of currently executed functions. These compile-time variables are run-time constants, so on translating brackets contents TH just substitute current values of these variables as literals. So, in this case [|... x ...|] is converted to [| ... $(lift x) ... |].
--
-- ghci> :m +Language.Haskell.TH
-- ghci> runQ [| \x _ -> x |] >>= print
-- LamE [VarP x_0,WildP] (VarE x_0)
--
-- InstanceD (Maybe Overlap) Cxt Type [Dec]
--
-- Clause [Pat] Body [Dec]
--
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

