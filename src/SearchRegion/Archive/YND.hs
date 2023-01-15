{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module SearchRegion.Archive.YND where

import SearchRegion.UB
import SearchRegion.Class

import SearchRegion.Archive.XE

import qualified Data.Array as A

{-| Archive containing the results of the optimization over the feasible set and the efficient set (phases 1 and 3)
    i.e over u_-l and over f-1(y*) 
-}


data YMdl = YMdl { _ymdlZone :: !ExploredUB,
                   _ymdlOpt :: !(Maybe Point)}

instance Show YMdl where
    show (YMdl z opt) = "optYND z=" ++ show (toBound $ fromExplored z) ++ " opt_" ++ show optproj ++ "=" ++ show opt 
        where optproj = snd $ _szMaxProj $ fromExplored z
newtype YArchive = YArchive [YMdl] --TODO


mkYMdl :: ExploredUB -> Maybe Point -> YMdl
mkYMdl zexp@(ExploredUB z) ptM = YMdl zexp ptM
      where (ProjDir l) = snd $ _szMaxProj z

insertYMdl :: YMdl -> YArchive -> YArchive
insertYMdl ymdl (YArchive l) = YArchive $ ymdl:l

checkYMdl :: ExploredUB -> YArchive -> Maybe YMdl
checkYMdl zexp (YArchive l) = -- or $ fmap (\amdl -> zexp `yKnownBy` amdl) l
    safeHead [ami | ami <- l, zexp `yKnownBy` ami]


_ymdlProj :: YMdl -> ProjDir
_ymdlProj = snd . _szMaxProj . fromExplored . _ymdlZone



yKnownBy :: ExploredUB -> YMdl -> Bool
yKnownBy zexp archivemdl = tpdir == apdir
                        && proj tpdir zexp `domL` proj tpdir aub
                        && (case ptM of 
                                Nothing -> True
                                Just pt -> (toBound zexp A.! l) <= (_ptPerf pt A.! l))
    where 
          (YMdl aub ptM) = archivemdl
          tpdir@(ProjDir l) = snd $ _szMaxProj $ fromExplored zexp
          apdir = _ymdlProj archivemdl



