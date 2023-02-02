{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module SearchRegion.Archive.YND where

import SearchRegion.UB
import SearchRegion.Class

import SearchRegion.Archive.XE

import qualified Data.Array.Unboxed as AU
import qualified Data.Array as A
import qualified Data.Set as S
import GHC.Generics
import Control.DeepSeq

{-| Archive containing the results of the optimization over the feasible set and the efficient set (phases 1 and 3)
    i.e over u_-l and over f-1(y*) 
-}


data YMdl = YMdl { _ymdlZone :: ![Double],
                   _ymdlOpt :: !(Maybe Double)}
        deriving (Generic, NFData)

newtype YArchive = YArchive (A.Array Int (S.Set YMdl)) --TODO
        deriving (Generic, NFData)



instance Eq YMdl where
    (YMdl z opt) == (YMdl z' opt') = opt == opt' && z == z'
instance Ord YMdl where
    (YMdl _ Nothing) `compare` (YMdl _ (Just _)) = GT
    (YMdl _ (Just _)) `compare` (YMdl _ Nothing) = LT
    (YMdl z opt) `compare` (YMdl z' opt') = let cmp = opt `compare` opt'
                                            in case cmp of
                                                 EQ -> opt `compare` opt'
                                                 _ -> cmp

mkYArchive :: Int -> YArchive
mkYArchive p = YArchive $ A.array (1,p) $ zip [1..p] (repeat S.empty)
mkYMdl :: ExploredUB -> Maybe Double -> YMdl
mkYMdl zexp@(ExploredUB z) ptM = YMdl (proj pdir zexp) ptM
      where pdir = snd $ _szMaxProj z

--insertYMdl :: YMdl -> YArchive -> YArchive
--insertYMdl ymdl (YArchive l) = YArchive $ ymdl:l

insertYMdl :: ExploredUB -> Maybe Double -> YArchive -> YArchive
insertYMdl zexp ptM (YArchive l) = YArchive l'
    where prevArch = l A.! fromProjDir proj
          proj = snd $ _szMaxProj $ fromExplored zexp
          l' = l A.//[(fromProjDir proj, S.insert (mkYMdl zexp ptM) prevArch)]



checkYMdl :: ExploredUB -> YArchive -> Maybe YMdl
checkYMdl zexp (YArchive l) = safeHead $ concat [checkYMdl' i | i <- [1..p]]-- or $ fmap (\amdl -> zexp `yKnownBy` amdl) l
    -- safeHead [ami | ami <- l, zexp `yKnownBy` ami]
    where p = dimension zexp
          checkYMdl' i = let ubproj = proj (ProjDir i) zexp
                             ui = toBound zexp AU.! i
                             (_, candidates) = S.split (YMdl ubproj (Just ui)) $ l A.! i
                         in [ci | ci <- S.toList $ candidates, and $ zipWith (<=) ubproj (_ymdlZone ci)]   








