{-# OPTIONS_GHC -O2 -fllvm -funbox-strict-fields -mavx -mavx2 -mavx512f #-}
        
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}


import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Criterion.Config
import Criterion.Main
import Data.Params
import Data.Primitive.ByteArray 
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import GHC.Float
import GHC.Int
import GHC.Base (Int (..))
import GHC.Prim

import Data.SIMD

-------------------------------------------------------------------------------
-- config

veclen = 16000

critConfig = defaultConfig 
    { cfgPerformGC   = ljust True
    , cfgSamples     = ljust 1000
    , cfgSummaryFile = ljust $ "summary"++show veclen++".csv"
    , cfgReport      = ljust $ "report"++show veclen++".html"
    }

-------------------------------------------------------------------------------
-- main

main = do
    
    -----------------------------------
    -- initialize single vectors

    putStrLn "constructing vectors"

    let lf1 :: [Float] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 1)
        lf2 :: [Float] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 2)

        ld1 :: [Double] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 1)
        ld2 :: [Double] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 2)

    let vuf1 = VU.fromList lf1 :: VU.Vector Float
        vuf2 = VU.fromList lf2 :: VU.Vector Float
    deepseq vuf1 $ deepseq vuf2 $ return () 

    let vud1 = VU.fromList ld2 :: VU.Vector Double
        vud2 = VU.fromList ld2 :: VU.Vector Double
    deepseq vud1 $ deepseq vud2 $ return () 

    let vsf1 = VS.fromList lf1 :: VS.Vector Float
        vsf2 = VS.fromList lf2 :: VS.Vector Float
    deepseq vsf1 $ deepseq vsf2 $ return () 

    let vsd1 = VS.fromList ld2 :: VS.Vector Double
        vsd2 = VS.fromList ld2 :: VS.Vector Double
    deepseq vsd1 $ deepseq vsd2 $ return () 

    -----------------------------------
    -- tests

    putStrLn "starting criterion"

    defaultMainWith critConfig (return ())
        [ bgroup "VU"
            [ bgroup "Float"
                [ bench "diff1"                 $ nf (distance_diff1                vuf1) vuf2
                , bench "diff4"                 $ nf (distance_diff4                vuf1) vuf2
                , bench "simd4"                 $ nf (distance_unbox_simd4          vuf1) vuf2
                , bench "simd8"                 $ nf (distance_unbox_simd8          vuf1) vuf2
                , bench "simd16"                $ nf (distance_unbox_simd16         vuf1) vuf2
                , bench "hof"                   $ nf (distance_hof                  vuf1) vuf2
                , bench "hof_simd4"             $ nf (distance_unbox_hof_simd4      vuf1) vuf2
                , bench "hof_simd8"             $ nf (distance_unbox_hof_simd8      vuf1) vuf2
                , bench "hof_simd16"            $ nf (distance_unbox_hof_simd16     vuf1) vuf2
                ]
            , bgroup "Double"
                [ bench "diff1"                 $ nf (distance_diff1                vud1) vud2
                , bench "diff4"                 $ nf (distance_diff4                vud1) vud2
                , bench "simd4"                 $ nf (distance_unbox_simd4          vud1) vud2
                , bench "simd8"                 $ nf (distance_unbox_simd8          vud1) vud2
                , bench "hof"                   $ nf (distance_hof                  vud1) vud2
                , bench "hof_simd4"             $ nf (distance_unbox_hof_simd4      vud1) vud2
                , bench "hof_simd8"             $ nf (distance_unbox_hof_simd8      vud1) vud2
                ]
            ]
        , bgroup "VS"
            [ bgroup "Float"
                [ bench "diff1"                 $ nf (distance_diff1                vsf1) vsf2
                , bench "diff4"                 $ nf (distance_diff4                vsf1) vsf2
                , bench "simd4"                 $ nf (distance_storable_simd4       vsf1) vsf2
                , bench "simd8"                 $ nf (distance_storable_simd8       vsf1) vsf2
                , bench "simd16"                $ nf (distance_storable_simd16      vsf1) vsf2
                , bench "hof"                   $ nf (distance_hof                  vsf1) vsf2
                , bench "hof_simd4"             $ nf (distance_storable_hof_simd4   vsf1) vsf2
                , bench "hof_simd8"             $ nf (distance_storable_hof_simd8   vsf1) vsf2
                , bench "hof_simd16"            $ nf (distance_storable_hof_simd16  vsf1) vsf2
                ]
            , bgroup "Double"
                [ bench "diff1"                 $ nf (distance_diff1                vsd1) vsd2
                , bench "diff4"                 $ nf (distance_diff4                vsd1) vsd2
                , bench "simd4"                 $ nf (distance_storable_simd4       vsd1) vsd2
                , bench "simd8"                 $ nf (distance_storable_simd8       vsd1) vsd2
                , bench "hof"                   $ nf (distance_hof                  vsd1) vsd2
                , bench "hof_simd4"             $ nf (distance_storable_hof_simd4   vsd1) vsd2
                , bench "hof_simd8"             $ nf (distance_storable_hof_simd8   vsd1) vsd2
                ]
            ]
        ]

-------------------------------------------------------------------------------
-- distance functions


distance_diff1 :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_diff1 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where 
                tot' = tot+diff1*diff1
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i


distance_diff4 :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_diff4 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go tot' (i-4)
            where 
                tot' = tot+diff1*diff1
                          +diff2*diff2
                          +diff3*diff3
                          +diff4*diff4
                      
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i
                diff2 = v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1)
                diff3 = v1 `VG.unsafeIndex` (i-2)-v2 `VG.unsafeIndex` (i-2)
                diff4 = v1 `VG.unsafeIndex` (i-3)-v2 `VG.unsafeIndex` (i-3)


distance_unbox_simd4 :: 
    ( SIMD4 f
    , Floating f
    , VU.Unbox f
    , VU.Unbox (X4 f)
    ) => VU.Vector f -> VU.Vector f -> f
distance_unbox_simd4 v1 v2 = sqrt $ plusHorizontalX4 $ go 0 (VG.length v1'-1)
    where
        v1' = unsafeVectorizeUnboxedX4 v1
        v2' = unsafeVectorizeUnboxedX4 v2
        
        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = tot+diff*diff
                diff = v1' `VG.unsafeIndex` i - v2' `VG.unsafeIndex` i


distance_unbox_simd8 :: 
    ( SIMD8 f
    , Floating f
    , VU.Unbox f
    , VU.Unbox (X8 f)
    ) => VU.Vector f -> VU.Vector f -> f
distance_unbox_simd8 v1 v2 = sqrt $ plusHorizontalX8 $ go 0 (VG.length v1'-1)
    where
        v1' = unsafeVectorizeUnboxedX8 v1
        v2' = unsafeVectorizeUnboxedX8 v2
        
        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = tot+diff*diff
                diff = v1' `VG.unsafeIndex` i - v2' `VG.unsafeIndex` i


distance_unbox_simd16 :: 
    ( SIMD16 f
    , Floating f
    , VU.Unbox f
    , VU.Unbox (X16 f)
    ) => VU.Vector f -> VU.Vector f -> f
distance_unbox_simd16 v1 v2 = sqrt $ plusHorizontalX16 $ go 0 (VG.length v1'-1)
    where
        v1' = unsafeVectorizeUnboxedX16 v1
        v2' = unsafeVectorizeUnboxedX16 v2
        
        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = tot+diff*diff
                diff = v1' `VG.unsafeIndex` i - v2' `VG.unsafeIndex` i


distance_storable_simd4 :: 
    ( SIMD4 f
    , Floating f
    , VS.Storable f
    , VS.Storable (X4 f)
    ) => VS.Vector f -> VS.Vector f -> f
distance_storable_simd4 v1 v2 = sqrt $ plusHorizontalX4 $ go 0 (VG.length v1'-1)
    where
        v1' = vectorizeStorableX4 v1
        v2' = vectorizeStorableX4 v2
        
        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = tot+diff*diff
                diff = v1' `VG.unsafeIndex` i - v2' `VG.unsafeIndex` i


distance_storable_simd8 :: 
    ( SIMD8 f
    , Floating f
    , VS.Storable f
    , VS.Storable (X8 f)
    ) => VS.Vector f -> VS.Vector f -> f
distance_storable_simd8 v1 v2 = sqrt $ plusHorizontalX8 $ go 0 (VG.length v1'-1)
    where
        v1' = vectorizeStorableX8 v1
        v2' = vectorizeStorableX8 v2
        
        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = tot+diff*diff
                diff = v1' `VG.unsafeIndex` i - v2' `VG.unsafeIndex` i


distance_storable_simd16 :: 
    ( SIMD16 f
    , Floating f
    , VS.Storable f
    , VS.Storable (X16 f)
    ) => VS.Vector f -> VS.Vector f -> f
distance_storable_simd16 v1 v2 = sqrt $ plusHorizontalX16 $ go 0 (VG.length v1'-1)
    where
        v1' = vectorizeStorableX16 v1
        v2' = vectorizeStorableX16 v2
        
        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = tot+diff*diff
                diff = v1' `VG.unsafeIndex` i - v2' `VG.unsafeIndex` i


distance_hof :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_hof v1 v2 = sqrt $ VG.foldl1' (+) $ VG.zipWith (\a1 a2 -> (a1-a2)*(a1-a2)) v1 v2


distance_unbox_hof_simd4 :: 
    ( SIMD4 f
    , Floating f
    , VU.Unbox f
    , VU.Unbox (X4 f)
    ) => VU.Vector f -> VU.Vector f -> f
distance_unbox_hof_simd4 v1 v2 
    = sqrt 
    $ plusHorizontalX4 
    $ VG.foldl1' (+) 
    $ (VG.zipWith (\a1 a2 -> (a1-a2)*(a1-a2)) (unsafeVectorizeUnboxedX4 v1) (unsafeVectorizeUnboxedX4 v2))


distance_unbox_hof_simd8 :: 
    ( SIMD8 f
    , Floating f
    , VU.Unbox f
    , VU.Unbox (X8 f)
    ) => VU.Vector f -> VU.Vector f -> f
distance_unbox_hof_simd8 v1 v2 
    = sqrt 
    $ plusHorizontalX8 
    $ VG.foldl1' (+) 
    $ (VG.zipWith (\a1 a2 -> (a1-a2)*(a1-a2)) (unsafeVectorizeUnboxedX8 v1) (unsafeVectorizeUnboxedX8 v2))


distance_unbox_hof_simd16 :: 
    ( SIMD16 f
    , Floating f
    , VU.Unbox f
    , VU.Unbox (X16 f)
    ) => VU.Vector f -> VU.Vector f -> f
distance_unbox_hof_simd16 v1 v2 
    = sqrt 
    $ plusHorizontalX16 
    $ VG.foldl1' (+) 
    $ (VG.zipWith (\a1 a2 -> (a1-a2)*(a1-a2)) (unsafeVectorizeUnboxedX16 v1) (unsafeVectorizeUnboxedX16 v2))


distance_storable_hof_simd4 :: 
    ( SIMD4 f
    , Floating f
    , VS.Storable f
    , VS.Storable (X4 f)
    ) => VS.Vector f -> VS.Vector f -> f
distance_storable_hof_simd4 v1 v2 
    = sqrt 
    $ plusHorizontalX4 
    $ VG.foldl1' (+) 
    $ (VG.zipWith (\a1 a2 -> (a1-a2)*(a1-a2)) (vectorizeStorableX4 v1) (vectorizeStorableX4 v2))


distance_storable_hof_simd8 :: 
    ( SIMD8 f
    , Floating f
    , VS.Storable f
    , VS.Storable (X8 f)
    ) => VS.Vector f -> VS.Vector f -> f
distance_storable_hof_simd8 v1 v2 
    = sqrt 
    $ plusHorizontalX8 
    $ VG.foldl1' (+) 
    $ (VG.zipWith (\a1 a2 -> (a1-a2)*(a1-a2)) (vectorizeStorableX8 v1) (vectorizeStorableX8 v2))


distance_storable_hof_simd16 :: 
    ( SIMD16 f
    , Floating f
    , VS.Storable f
    , VS.Storable (X16 f)
    ) => VS.Vector f -> VS.Vector f -> f
distance_storable_hof_simd16 v1 v2 
    = sqrt 
    $ plusHorizontalX16 
    $ VG.foldl1' (+) 
    $ (VG.zipWith (\a1 a2 -> (a1-a2)*(a1-a2)) (vectorizeStorableX16 v1) (vectorizeStorableX16 v2))
