{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module SearchRegion.Archive.XE where

import SearchRegion.UB
import SearchRegion.Class

import Data.Maybe

import qualified Data.Array as A

{-| Archive containing the results of the optimization over the feasible set and the efficient set (phases 1 and 3)
    i.e over u_-l and over f-1(y*) 
-}

data XeMdl = XeMdl { _xemdlZone :: !ExploredUB,
                     _xemdlOpt :: !(Maybe HyperOpt)}

instance Show XeMdl where
    show (XeMdl z opt) = "optXE z=" ++ show (toBound $ fromExplored z) ++ " opt_" ++ show optproj ++ "=" ++ show opt 
        where optproj = snd $ _szMaxProj $ fromExplored z
newtype XeArchive = XeArchive [XeMdl] --TODO


mkXeMdl :: ExploredUB -> Maybe HyperOpt -> XeMdl
mkXeMdl zexp@(ExploredUB z) hopt = XeMdl zexp hopt

insertXeMdl :: ExploredUB -> Maybe HyperOpt -> XeArchive -> XeArchive
insertXeMdl ub hopt (XeArchive l) = XeArchive $ mkXeMdl ub hopt:l

--checkXeMdl :: XeMdl -> XeArchive -> Bool
checkXeMdl :: UB -> SubOpt -> XeArchive -> Maybe XeMdl
checkXeMdl ub curopt (XeArchive l) = -- or $ fmap (\amdl -> xemdl `xeKnownBy` amdl) l
    safeHead [xi | xi <- l, xeKnownBy ub curopt xi]


_xemdlProj :: XeMdl -> ProjDir
_xemdlProj = snd . _szMaxProj . fromExplored . _xemdlZone



-- TODO TOCHECK
xeKnownBy :: UB -> SubOpt -> XeMdl -> Bool
xeKnownBy test (SubOpt estimation) archivemdl = proj apdir test `domL` proj apdir aub
                                    && if isJust aopt 
                                        then (HyperOpt estimation) <= fromJust aopt
                                        else True
    where 
          (XeMdl aub aopt) = archivemdl
          apdir = _xemdlProj archivemdl



safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing
