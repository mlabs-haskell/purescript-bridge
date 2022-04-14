module PlutusTx.Aux where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Datatype as TH
import qualified PlutusTx.IsData  as P
import Prelude (Int, Maybe (Just, Nothing), fromIntegral, map, pure, zip, ($), (.), (<$>), (<>))

-- PlutusTx doesn't export this so we need to duplicate it here
defaultIndex :: TH.Name -> TH.Q [(TH.Name, Int)]
defaultIndex name = do
  info <- TH.reifyDatatype name
  pure $ zip (TH.constructorName <$> TH.datatypeCons info) [0 ..]

makeHasConstrIndex :: TH.Name -> [(TH.Name, Int)] -> TH.Q [TH.Dec]
makeHasConstrIndex name indices =
  pure [TH.InstanceD Nothing [] instanceType [getIndices]]
  where
    instanceType = TH.AppT classType (TH.ConT name)

    classType = TH.ConT $ TH.mkName "HasConstrIndices"

    getIndices :: TH.Dec
    getIndices = TH.FunD (TH.mkName "getConstrIndices") [methodClause]

    methodClause :: TH.Clause
    methodClause = TH.Clause [] methodBody []

    methodBody :: TH.Body
    methodBody = TH.NormalB indicesE

    indicesE :: TH.Exp
    indicesE =
      TH.ListE $
        map
          ( \(n, i) ->
              TH.TupE [Just (TH.LitE . TH.IntegerL . fromIntegral $ i), Just (TH.LitE . TH.StringL $ TH.nameBase n)]
          )
          indices

unstableMakeIsData :: TH.Name -> TH.Q [TH.Dec]
unstableMakeIsData name = do
  indices <- defaultIndex name
  hasConstrIndicesInstance <- makeHasConstrIndex name indices
  decs <- P.unstableMakeIsData name
  pure (hasConstrIndicesInstance <> decs)

makeIsDataIndexed :: TH.Name -> [(TH.Name, Int)] -> TH.Q [TH.Dec]
makeIsDataIndexed name indices = do
  hasConstrIndicesInstance <- makeHasConstrIndex name indices
  decs <- P.makeIsDataIndexed name indices
  pure (hasConstrIndicesInstance <> decs)

mkIndicesDefault :: TH.Name -> TH.Q [TH.Dec]
mkIndicesDefault name = do
  indices <- defaultIndex name
  makeHasConstrIndex name indices
