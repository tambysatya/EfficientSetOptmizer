{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module SearchRegion.Archive.XE where

import SearchRegion.UB
import SearchRegion.Class

import qualified Data.Array as A

{-| Archive containing the results of the optimization over the feasible set and the efficient set (phases 1 and 3)
    i.e over u_-l and over f-1(y*) 
-}


newtype OptVal = OptVal Double
    deriving (Show,Num,Eq,Ord)
newtype OptSum = OptSum Double
    deriving (Show,Num,Eq,Ord)

data XeMdl = XeMdl { _xemdlZone :: !ExploredUB,
                     _xemdlOpt :: !OptVal,
                     _xemdlSum :: !OptSum}

instance Show XeMdl where
    show (XeMdl z opt s) = "optXE z=" ++ show (toBound $ fromExplored z) ++ " opt_" ++ show optproj ++ "=" ++ show opt ++ " sum=" ++ show s
        where optproj = snd $ _szMaxProj $ fromExplored z
newtype XeArchive = XeArchive [XeMdl] --TODO


mkXeMdl :: ExploredUB -> Point -> XeMdl
mkXeMdl zexp@(ExploredUB z) pt = XeMdl zexp (OptVal opt) (OptSum optsum)
    where 
          (_,pdir@(ProjDir l)) = _szMaxProj z
          opt = _ptPerf pt A.! l
          optsum = sum $ A.elems $ _ptPerf pt

insertXeMdl :: XeMdl -> XeArchive -> XeArchive
insertXeMdl xemdl (XeArchive l) = XeArchive $ xemdl:l

--checkXeMdl :: XeMdl -> XeArchive -> Bool
checkXeMdl :: XeMdl -> XeArchive -> Maybe XeMdl
checkXeMdl xemdl (XeArchive l) = -- or $ fmap (\amdl -> xemdl `xeKnownBy` amdl) l
    safeHead [xi | xi <- l, xemdl `xeKnownBy` xi]


_xemdlProj :: XeMdl -> ProjDir
_xemdlProj = snd . _szMaxProj . fromExplored . _xemdlZone



xeKnownBy :: XeMdl -> XeMdl -> Bool
xeKnownBy testmdl archivemdl = tpdir == apdir
                            && proj tpdir tub `domL` proj tpdir aub
                            && topt == aopt
                            && tsum >= asum -- si vrai, alors le point est dominé non ? TODO
    where (XeMdl tub topt tsum) = testmdl
          (XeMdl aub aopt asum) = archivemdl
          tpdir = _xemdlProj testmdl
          apdir = _xemdlProj archivemdl



safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing
