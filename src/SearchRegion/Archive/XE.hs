{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module SearchRegion.Archive.XE where

import SearchRegion.UB
import SearchRegion.Class

import Data.Maybe

import qualified Data.Array as A
import qualified Data.Array.Unboxed as AU
import qualified Data.Set as S
import GHC.Generics
import Control.DeepSeq

{-| Archive containing the results of the optimization over the feasible set and the efficient set (phases 1 and 3)
    i.e over u_-l and over f-1(y*) 
-}

data XeMdl = XeMdl { _xemdlZone :: ![Double],
                     _xemdlOpt :: !(Maybe HyperOpt)}
        deriving (Generic, NFData)


instance Eq XeMdl where
    (XeMdl z opt) == (XeMdl z' opt') = opt == opt' && z == z'
instance Ord XeMdl where
    (XeMdl _ Nothing) `compare` (XeMdl _ (Just _)) = GT
    (XeMdl _ (Just _)) `compare` (XeMdl _ Nothing) = LT
    (XeMdl z opt) `compare` (XeMdl z' opt') = let ret = opt `compare` opt'
                                              in case ret of
                                                        EQ -> z `compare` z'
                                                        _ -> ret
newtype XeArchive = XeArchive (A.Array Int (S.Set XeMdl))
        deriving (Generic, NFData)



mkXeArchive :: Int -> XeArchive
mkXeArchive p = XeArchive $ A.array (1,p) $ zip [1..p] (repeat S.empty)

mkXeMdl :: ExploredUB -> Maybe HyperOpt -> XeMdl
mkXeMdl zexp@(ExploredUB z) hopt = XeMdl (proj pdir z) hopt
    where pdir = snd $  _szMaxProj z

insertXeMdl :: ExploredUB -> Maybe HyperOpt -> XeArchive -> XeArchive
insertXeMdl ub hopt (XeArchive l) = XeArchive $ l A.// [(fromProjDir proj, S.insert (mkXeMdl ub hopt) prevArch)]--XeArchive $ mkXeMdl ub hopt:l
    where prevArch = l A.! fromProjDir proj
          proj = snd $ _szMaxProj $ fromExplored ub

--checkXeMdl :: XeMdl -> XeArchive -> Bool
checkXeMdl :: UB -> SubOpt -> XeArchive -> Maybe XeMdl
checkXeMdl ub curopt (XeArchive l) = safeHead $ concat [checkXeMdl' i | i <- [1..p]]-- or $ fmap (\amdl -> xemdl `xeKnownBy` amdl) l
    -- safeHead [xi | xi <- l, xeKnownBy ub curopt xi]
  where p = dimension ub  
        checkXeMdl' i = let ubproj = proj (ProjDir i) ub
                    --        ui = toBound ub AU.! i
                            (_, candidates) = S.split (XeMdl ubproj (Just $ HyperOpt $ fromSubOpt curopt)) $ l A.! i
                        in [ci | ci <- S.toList $ candidates, and $ zipWith (<=) ubproj (_xemdlZone ci)]   







safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing
