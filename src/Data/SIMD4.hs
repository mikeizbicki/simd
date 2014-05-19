{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Data.SIMD
    ( 
    
    -- * SIMD classes
      SIMD4 (..)
    , SIMD4Float (..)

    -- * conversion functions
    , unsafeVectorizeUnboxedX4
    , vectorizeUnboxedX4
    , unVectorizeUnboxedX4
    , vectorizeStorableX4
    , unVectorizeStorableX4
    )
    where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive
import Data.Primitive.MachDeps
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as P
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import GHC.Base (Int(..))
import GHC.Float
import GHC.Int
import GHC.Word
import GHC.Prim
import GHC.Ptr

import Unsafe.Coerce

unI# :: Int -> Int#
unI# (I# i#) = i#

-------------------------------------------------------------------------------
-- SIMD4

class SIMD4 a where
    data X4 a
    plusX4 :: X4 a -> X4 a -> X4 a
    minusX4 :: X4 a -> X4 a -> X4 a
    timesX4 :: X4 a -> X4 a -> X4 a
    negateX4 :: X4 a -> X4 a 
    indexArrayAsX4 :: ByteArray -> Int -> X4 a
    indexOffAddrAsX4 :: Addr -> Int -> X4 a
    insertX4 :: X4 a -> a -> Int -> X4 a
    unpackX4 :: X4 a -> (# a, a, a, a #)
    packX4 :: (# a,a,a,a #) -> X4 a
    broadcastX4 :: a -> X4 a
    readOffAddrAsX4 :: Addr# -> Int# -> State# s -> (# State# s, X4 a #)
    writeOffAddrAsX4 :: Addr# -> Int# -> X4 a -> State# s -> State# s

    {-# INLINE plusHorizontalX4 #-}
    plusHorizontalX4 :: (SIMD4 a, Num a) => X4 a -> a
    plusHorizontalX4 v = r1+r2+r3+r4
        where
            (# r1,r2,r3,r4 #) = unpackX4 v

    {-# INLINE timesHorizontalX4 #-}
    timesHorizontalX4 :: (SIMD4 a, Num a) => X4 a -> a
    timesHorizontalX4 v = r1*r2*r3*r4
        where
            (# r1,r2,r3,r4 #) = unpackX4 v

class SIMD4 a => SIMD4Float a where
    divideX4 :: X4 a -> X4 a -> X4 a

instance (Fractional a, SIMD4Float a) => Fractional (X4 a) where
    (/) = divideX4
    fromRational = broadcastX4 . fromRational
    {-# INLINE (/) #-}
    {-# INLINE fromRational #-}

instance (Show a, SIMD4 a) => Show (X4 a) where
    show v = show (r1,r2,r3,r4)
        where (# r1,r2,r3,r4 #) = unpackX4 v

instance (Num a, SIMD4 a) => Num (X4 a) where
    (+) = plusX4
    (*) = timesX4
    (-) = minusX4
    negate = negateX4
    abs = error "SIMD4 abs not defined"
    signum = error "SIMD4 signum not defined"
    fromInteger i = broadcastX4 (fromInteger i::a)
    {-# INLINE (+) #-}
    {-# INLINE (*) #-}
    {-# INLINE (-) #-}
    {-# INLINE negate #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}

#define mkSIMD4(t,tt,cons,vec,plus,minus,times,negate,indexArray,indexOffAddr,insert,unpack,pack,broadcast,readOffAddr,writeOffAddr) \
instance SIMD4 t where\
    data X4 t = cons vec ;\
    plusX4 (cons v1#) (cons v2#) = cons (plus v1# v2#)          ;\
    minusX4 (cons v1#) (cons v2#) = cons (minus v1# v2#)        ;\
    timesX4 (cons v1#) (cons v2#) = cons (times v1# v2#)        ;\
    negateX4 (cons v1#) = cons (negate v1#)                     ;\
    indexArrayAsX4 (ByteArray ba#) (I# i#) = cons (indexArray ba# i#) ;\
    indexOffAddrAsX4 (Addr addr#) (I# i#) = cons (indexOffAddr addr# i#) ;\
    insertX4 (cons v1#) (tt s#) (I# i#) = cons (insert v1# s# i#) ;\
    unpackX4 (cons v1#) = let (# r1,r2,r3,r4 #) = unpack v1# in (# tt r1, tt r2, tt r3, tt r4 #) ;\
    packX4 (# tt r1,tt r2, tt r3, tt r4 #) = cons (pack (# r1,r2,r3,r4 #)) ;\
    broadcastX4 (tt r) = cons (broadcast r) ;\
    readOffAddrAsX4 addr# i# s# = case readOffAddr addr# (mul4 i#) s# of \
            { (# s1#, x# #) -> (# s1#, cons x# #) }                      ;\
    writeOffAddrAsX4 addr# i# (cons v1#) s# = writeOffAddr addr# (mul4 i#) v1# s# ;\
    {-# INLINE plusX4 #-} ;\
    {-# INLINE minusX4 #-} ;\
    {-# INLINE timesX4 #-} ;\
    {-# INLINE negateX4 #-} ;\
    {-# INLINE indexArrayAsX4 #-} ;\
    {-# INLINE indexOffAddrAsX4 #-} ;\
    {-# INLINE insertX4 #-} ;\
    {-# INLINE unpackX4 #-} ;\
    {-# INLINE packX4 #-} ;\
    {-# INLINE broadcastX4 #-} ;\
    {-# INLINE readOffAddrAsX4 #-} ;\
    {-# INLINE writeOffAddrAsX4 #-}

mkSIMD4(Float,F#,FloatX4,FloatX4#,
        plusFloatX4#,minusFloatX4#,timesFloatX4#,negateFloatX4#,
        indexFloatArrayAsFloatX4#,indexFloatOffAddrAsFloatX4#,
        insertFloatX4#, unpackFloatX4#, packFloatX4#,broadcastFloatX4#,
        readFloatOffAddrAsFloatX4#, writeFloatOffAddrAsFloatX4#
        )

instance SIMD4Float Float where 
    divideX4 (FloatX4 v1#) (FloatX4 v2#) = FloatX4 (divideFloatX4# v1# v2#)
    {-# INLINE divideX4 #-}

mkSIMD4(Double,D#,DoubleX4,DoubleX4#,
        plusDoubleX4#,minusDoubleX4#,timesDoubleX4#,negateDoubleX4#,
        indexDoubleArrayAsDoubleX4#,indexDoubleOffAddrAsDoubleX4#,
        insertDoubleX4#, unpackDoubleX4#, packDoubleX4#,broadcastDoubleX4#,
        readDoubleOffAddrAsDoubleX4#, writeDoubleOffAddrAsDoubleX4#
        )

instance SIMD4Float Double where 
    divideX4 (DoubleX4 v1#) (DoubleX4 v2#) = DoubleX4 (divideDoubleX4# v1# v2#)
    {-# INLINE divideX4 #-}

-------------------
-- Prim SIMD4

mul4 :: Int# -> Int#
mul4 i# = unI# (I# i# * 4)

#define derivePrim(ty, ctr, sz, align, idx_arr, rd_arr, wr_arr, set_arr, idx_addr, rd_addr, wr_addr, set_addr) \
instance Prim ty where {                                        \
  sizeOf# _ = unI# sz                                           \
; alignment# _ = unI# align                                     \
; indexByteArray# arr# i# = ctr (idx_arr arr# (mul4 i#))               \
; readByteArray#  arr# i# s# = case rd_arr arr# (mul4 i#) s# of        \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeByteArray# arr# i# (ctr x#) s# = wr_arr arr# (mul4 i#) x# s#    \
; {-setByteArray# arr# i# n# (ctr x#) s#                          \
    = case unsafeCoerce# (internal (set_arr arr# (unI# (I# i# * 4)) n# x#)) s# of \
            { (# s1#, _ #) -> s1# }                                 \
  -}                                                              \
; indexOffAddr# addr# i# = ctr (idx_addr addr# (mul4 i#))              \
; readOffAddr#  addr# i# s# = case rd_addr addr# (mul4 i#) s# of       \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeOffAddr# addr# i# (ctr x#) s# = wr_addr addr# (mul4 i#) x# s#   \
; {-# INLINE sizeOf# #-}                                        \
; {-# INLINE alignment# #-}                                     \
; {-# INLINE indexByteArray# #-}                                \
; {-# INLINE readByteArray# #-}                                 \
; {-# INLINE writeByteArray# #-}                                \
; {-# INLINE indexOffAddr# #-}                                  \
; {-# INLINE readOffAddr# #-}                                   \
; {-# INLINE writeOffAddr# #-}                                  \
}

derivePrim((X4 Float), FloatX4, (sIZEOF_FLOAT*4), (aLIGNMENT_FLOAT*4),
           indexFloatArrayAsFloatX4#, readFloatArrayAsFloatX4#, writeFloatArrayAsFloatX4#, setFloatArray#,
           indexFloatOffAddrAsFloatX4#, readFloatOffAddrAsFloatX4#, writeFloatOffAddrAsFloatX4#, setFloatOffAddrAsFloatX4#)
            
derivePrim((X4 Double), DoubleX4, (sIZEOF_FLOAT*4), (aLIGNMENT_FLOAT*4),
           indexDoubleArrayAsDoubleX4#, readDoubleArrayAsDoubleX4#, writeDoubleArrayAsDoubleX4#, setDoubleArray#,
           indexDoubleOffAddrAsDoubleX4#, readDoubleOffAddrAsDoubleX4#, writeDoubleOffAddrAsDoubleX4#, setDoubleOffAddrAsDoubleX4#)

-------------------
-- Storable SIMD4

#define mkStorable(t) \
instance Storable (X4 t) where \
    sizeOf x = Data.Primitive.sizeOf x ;\
    alignment x = Data.Primitive.alignment x ;\
    peekElemOff (Ptr addr#) (I# i#) = primitive (readOffAddrAsX4 addr# i#) ;\
    pokeElemOff (Ptr addr#) (I# i#) a = primitive_ (writeOffAddrAsX4 addr# i# a) ;\
    {-# INLINE sizeOf #-} ;\
    {-# INLINE alignment #-} ;\
    {-# INLINE peekElemOff #-} ;\
    {-# INLINE pokeElemOff #-}

mkStorable(Float)
mkStorable(Double)

-------------------
-- vectors

#define primMVector(ty,con)                                             \
instance M.MVector VUM.MVector ty where {                                   \
  {-# INLINE basicLength #-}                                            \
; {-# INLINE basicUnsafeSlice #-}                                       \
; {-# INLINE basicOverlaps #-}                                          \
; {-# INLINE basicUnsafeNew #-}                                         \
; {-# INLINE basicUnsafeReplicate #-}                                   \
; {-# INLINE basicUnsafeRead #-}                                        \
; {-# INLINE basicUnsafeWrite #-}                                       \
; {-# INLINE basicClear #-}                                             \
; {-# INLINE basicSet #-}                                               \
; {-# INLINE basicUnsafeCopy #-}                                        \
; {-# INLINE basicUnsafeGrow #-}                                        \
; basicLength (con v) = M.basicLength v                                 \
; basicUnsafeSlice i n (con v) = con $ M.basicUnsafeSlice i n v         \
; basicOverlaps (con v1) (con v2) = M.basicOverlaps v1 v2               \
; basicUnsafeNew n = con `liftM` M.basicUnsafeNew n                     \
; {-basicUnsafeReplicate n x = con `liftM` M.basicUnsafeReplicate n x -}    \
; basicUnsafeRead (con v) i = M.basicUnsafeRead v i                     \
; basicUnsafeWrite (con v) i x = M.basicUnsafeWrite v i x               \
; basicClear (con v) = M.basicClear v                                   \
; {-basicSet (con v) x = M.basicSet v x                                -}   \
; basicUnsafeCopy (con v1) (con v2) = M.basicUnsafeCopy v1 v2           \
; basicUnsafeMove (con v1) (con v2) = M.basicUnsafeMove v1 v2           \
; basicUnsafeGrow (con v) n = con `liftM` M.basicUnsafeGrow v n } 

#define primVector(ty,con,mcon)                                         \
instance G.Vector VU.Vector ty where {                                     \
  {-# INLINE basicUnsafeFreeze #-}                                      \
; {-# INLINE basicUnsafeThaw #-}                                        \
; {-# INLINE basicLength #-}                                            \
; {-# INLINE basicUnsafeSlice #-}                                       \
; {-# INLINE basicUnsafeIndexM #-}                                      \
; {-# INLINE elemseq #-}                                                \
; basicUnsafeFreeze (mcon v) = con `liftM` G.basicUnsafeFreeze v        \
; basicUnsafeThaw (con v) = mcon `liftM` G.basicUnsafeThaw v            \
; basicLength (con v) = G.basicLength v                                 \
; basicUnsafeSlice i n (con v) = con $ G.basicUnsafeSlice i n v         \
; basicUnsafeIndexM (con v) i = G.basicUnsafeIndexM v i                 \
; basicUnsafeCopy (mcon mv) (con v) = G.basicUnsafeCopy mv v            \
; elemseq _ = seq }

newtype instance VUM.MVector s (X4 Float) = MV_FloatX4 (P.MVector s (X4 Float))
newtype instance VU.Vector     (X4 Float) = V_FloatX4  (P.Vector    (X4 Float))
instance VU.Unbox (X4 Float)
primMVector((X4 Float), MV_FloatX4)
primVector((X4 Float), V_FloatX4, MV_FloatX4)

newtype instance VUM.MVector s (X4 Double) = MV_DoubleX4 (P.MVector s (X4 Double))
newtype instance VU.Vector     (X4 Double) = V_DoubleX4  (P.Vector    (X4 Double))
instance VU.Unbox (X4 Double)
primMVector((X4 Double), MV_DoubleX4)
primVector((X4 Double), V_DoubleX4, MV_DoubleX4)

-- | FIXME: this is a huge hack to get around the fact that primitive vectors
-- do not export their constructors
data UnsafePrimVector a = UnsafePrimVector 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!ByteArray

-------------------------------------------------------------------------------
-- conversion functions

{-# INLINE unsafeVectorizeUnboxedX4 #-}
unsafeVectorizeUnboxedX4 :: (SIMD4 a, VU.Unbox a) => VU.Vector a -> VU.Vector (X4 a)
unsafeVectorizeUnboxedX4 v = unsafeCoerce pv
    where
        pv = UnsafePrimVector (len `div` 4) (off `div` 4) arr
        UnsafePrimVector len off arr = unsafeCoerce v

{-# INLINE vectorizeUnboxedX4 #-}
vectorizeUnboxedX4 :: (SIMD4 a, VU.Unbox a) => VU.Vector a -> VU.Vector (X4 a)
vectorizeUnboxedX4 v = if len `mod` 4 == 0 && off `mod` 4 == 0
    then unsafeCoerce pv
    else error "vectorizeUnboxedX4 vector wrong len/offset"
    where
        pv = UnsafePrimVector (len `div` 4) (off `div` 4) arr
        UnsafePrimVector len off arr = unsafeCoerce v

{-# INLINE unVectorizeUnboxedX4 #-}
unVectorizeUnboxedX4 :: (SIMD4 a, VU.Unbox a) => VU.Vector (X4 a) -> VU.Vector a
unVectorizeUnboxedX4 v = unsafeCoerce v
    where
        pv = UnsafePrimVector (len*4) (off*4)
        UnsafePrimVector len off arr = unsafeCoerce v

{-# INLINE unsafeVectorizeStorableX4 #-}
unsafeVectorizeStorableX4 :: (SIMD4 a, Storable a, Storable (X4 a)) => VS.Vector a -> VS.Vector (X4 a)
unsafeVectorizeStorableX4 v = VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len `div` 4)
    where
        (fp,len) = VS.unsafeToForeignPtr0 v

{-# INLINE vectorizeStorableX4 #-}
vectorizeStorableX4 :: (SIMD4 a, Storable a, Storable (X4 a)) => VS.Vector a -> VS.Vector (X4 a)
vectorizeStorableX4 v = if (len `mod` 4 == 0) 
    then VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len `div` 4)
    else error "vectorizeStorableX4 vector wrong len"
    where
        (fp,len) = VS.unsafeToForeignPtr0 v

{-# INLINE unVectorizeStorableX4 #-}
unVectorizeStorableX4 :: (SIMD4 a, Storable a, Storable (X4 a)) => VS.Vector (X4 a) -> VS.Vector a
unVectorizeStorableX4 v = VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len*4)
    where
        (fp,len) = VS.unsafeToForeignPtr0 v
