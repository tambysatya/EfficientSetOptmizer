module MOIP.Domain where
{-| TODO : export / import domains using JSON or .lp format -}

import qualified Data.Array as A

-- | ([ObjectivesFunction], [Lower bound], [Constraints], [UpperBound])
type Domain = ([[Double]], [Double], [[Double]], [Double])

nbDomVars :: Domain -> Int
nbDomVars ((f:_),_,_,_) = length f
nbDomVars _ = error "invalid domain: no objective coefficients"
nbObjVars (f,_,_,_) = length f
nbDomCtrs (_,lb,_,_) = length lb


readKS :: String -> IO Domain
readKS str = do
    ls <- lines <$> readFile str
    let (strp:strn:wmaxstr:costlist:weightlist:[]) = ls
        p = read strp :: Int
        n = read strn :: Int
        wmaxs = read wmaxstr
        costs = fmap negate <$> read costlist
        weights = read weightlist
        m = length weights
--    print costs
    pure (costs, take m (repeat 0), weights, wmaxs)

read1KS :: String -> IO Domain
read1KS str = do
    ls <- lines <$> readFile str
    let (strp:strn:wmaxstr:costlist:weightlist:[]) = ls
        p = read strp :: Int
        n = read strn :: Int
        wmax = read wmaxstr
        costs = fmap negate <$> read costlist
        weights = read weightlist
--    print costs
    pure (costs, [0] , [weights], [wmax])

getNbCrits :: Domain -> Int
getNbCrits (crits, _,_,_) = length crits


buildVC :: Int -> Int -> [A.Array (Int,Int) Double] -> Domain
buildVC p nbObj w = (weights,initLine, matToList ctrs, initLine)
  where
        weights = map (\ci ->  [ ci A.! (i,j) | i <- [1..nbObj], j <- [1..nbObj] ]) w
        initLine = take (fromIntegral nbObj*2) $  repeat 1
        initMat = A.listArray ((1,1),(nbObj*2, nbObj*nbObj)) $ repeat 0
        matToList mat = [[mat A.! (i,j) | j <- [1..nbObj*nbObj]] | i <- [1..nbObj*2]]
        ctrsL = [((i+1, i*nbObj +j),1) | i <- [0..nbObj-1], j<- [1..nbObj]]
        ctrsR = [((nbObj+j, (i-1)*nbObj+j) ,1) | j <- [1..nbObj], i <- [1..nbObj] ]
        ctrs = initMat A.// (ctrsL ++ ctrsR)
readVC :: String -> IO Domain
readVC fname = do x <- readFile fname
                  let (p1:nObj:rest) = lines x
                      costs = matFromLines (read nObj, read nObj) <$> (read (concat rest) :: [[[Double]]])
                  pure $ buildVC (read p1) (read nObj) costs

matFromLines (n,p) l = A.array ((1,1),(n,p)) $ concat $ [[((i,j),vi) | (j,vi) <- zip [1..] li] | (i,li) <- zip [1..] l ]

