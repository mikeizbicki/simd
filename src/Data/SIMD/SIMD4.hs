{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- | This module wraps the SIMD operations that act on 4 values simultaneously.
module Data.SIMD.SIMD4
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

-- | this is a thin wrapper over the primitive operations
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

    -- | this operation is slow, avoid at all costs!
    {-# INLINE plusHorizontalX4 #-}
    plusHorizontalX4 :: (SIMD4 a, Num a) => X4 a -> a
    plusHorizontalX4 v = r1+r2+r3+r4
        where
            (# r1,r2,r3,r4 #) = unpackX4 v

    -- | this operation is slow, avoid at all costs!
    {-# INLINE timesHorizontalX4 #-}
    timesHorizontalX4 :: (SIMD4 a, Num a) => X4 a -> a
    timesHorizontalX4 v = r1*r2*r3*r4
        where
            (# r1,r2,r3,r4 #) = unpackX4 v

-- | this is a thin wrapper over the primitive division operation
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

mkSIMD4(Word32,W32#,Word32X4,Word32X4#,
        plusWord32X4#,minusWord32X4#,timesWord32X4#,(error "cannot negate Word32X4"),
        indexWord32ArrayAsWord32X4#,indexWord32OffAddrAsWord32X4#,
        insertWord32X4#, unpackWord32X4#, packWord32X4#,broadcastWord32X4#,
        readWord32OffAddrAsWord32X4#, writeWord32OffAddrAsWord32X4#
        )

mkSIMD4(Word64,W64#,Word64X4,Word64X4#,
        plusWord64X4#,minusWord64X4#,timesWord64X4#,(error "cannot negate Word64X4"),
        indexWord64ArrayAsWord64X4#,indexWord64OffAddrAsWord64X4#,
        insertWord64X4#, unpackWord64X4#, packWord64X4#,broadcastWord64X4#,
        readWord64OffAddrAsWord64X4#, writeWord64OffAddrAsWord64X4#
        )

mkSIMD4(Int32,I32#,Int32X4,Int32X4#,
        plusInt32X4#,minusInt32X4#,timesInt32X4#,negateInt32X4#,
        indexInt32ArrayAsInt32X4#,indexInt32OffAddrAsInt32X4#,
        insertInt32X4#, unpackInt32X4#, packInt32X4#,broadcastInt32X4#,
        readInt32OffAddrAsInt32X4#, writeInt32OffAddrAsInt32X4#
        )

mkSIMD4(Int64,I64#,Int64X4,Int64X4#,
        plusInt64X4#,minusInt64X4#,timesInt64X4#,negateInt64X4#,
        indexInt64ArrayAsInt64X4#,indexInt64OffAddrAsInt64X4#,
        insertInt64X4#, unpackInt64X4#, packInt64X4#,broadcastInt64X4#,
        readInt64OffAddrAsInt64X4#, writeInt64OffAddrAsInt64X4#
        )

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

derivePrim((X4 Int32), Int32X4, (sIZEOF_FLOAT*4), (aLIGNMENT_FLOAT*4),
           indexInt32ArrayAsInt32X4#, readInt32ArrayAsInt32X4#, writeInt32ArrayAsInt32X4#, setInt32Array#,
           indexInt32OffAddrAsInt32X4#, readInt32OffAddrAsInt32X4#, writeInt32OffAddrAsInt32X4#, setInt32OffAddrAsInt32X4#)
            
derivePrim((X4 Int64), Int64X4, (sIZEOF_FLOAT*4), (aLIGNMENT_FLOAT*4),
           indexInt64ArrayAsInt64X4#, readInt64ArrayAsInt64X4#, writeInt64ArrayAsInt64X4#, setInt64Array#,
           indexInt64OffAddrAsInt64X4#, readInt64OffAddrAsInt64X4#, writeInt64OffAddrAsInt64X4#, setInt64OffAddrAsInt64X4#)
            
derivePrim((X4 Word32), Word32X4, (sIZEOF_FLOAT*4), (aLIGNMENT_FLOAT*4),
           indexWord32ArrayAsWord32X4#, readWord32ArrayAsWord32X4#, writeWord32ArrayAsWord32X4#, setWord32Array#,
           indexWord32OffAddrAsWord32X4#, readWord32OffAddrAsWord32X4#, writeWord32OffAddrAsWord32X4#, setWord32OffAddrAsWord32X4#)
            
derivePrim((X4 Word64), Word64X4, (sIZEOF_FLOAT*4), (aLIGNMENT_FLOAT*4),
           indexWord64ArrayAsWord64X4#, readWord64ArrayAsWord64X4#, writeWord64ArrayAsWord64X4#, setWord64Array#,
           indexWord64OffAddrAsWord64X4#, readWord64OffAddrAsWord64X4#, writeWord64OffAddrAsWord64X4#, setWord64OffAddrAsWord64X4#)

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
mkStorable(Int32)
mkStorable(Int64)
mkStorable(Word32)
mkStorable(Word64)

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

newtype instance VUM.MVector s (X4 Int32) = MV_Int32X4 (P.MVector s (X4 Int32))
newtype instance VU.Vector     (X4 Int32) = V_Int32X4  (P.Vector    (X4 Int32))
instance VU.Unbox (X4 Int32)
primMVector((X4 Int32), MV_Int32X4)
primVector((X4 Int32), V_Int32X4, MV_Int32X4)

newtype instance VUM.MVector s (X4 Int64) = MV_Int64X4 (P.MVector s (X4 Int64))
newtype instance VU.Vector     (X4 Int64) = V_Int64X4  (P.Vector    (X4 Int64))
instance VU.Unbox (X4 Int64)
primMVector((X4 Int64), MV_Int64X4)
primVector((X4 Int64), V_Int64X4, MV_Int64X4)

newtype instance VUM.MVector s (X4 Word32) = MV_Word32X4 (P.MVector s (X4 Word32))
newtype instance VU.Vector     (X4 Word32) = V_Word32X4  (P.Vector    (X4 Word32))
instance VU.Unbox (X4 Word32)
primMVector((X4 Word32), MV_Word32X4)
primVector((X4 Word32), V_Word32X4, MV_Word32X4)

newtype instance VUM.MVector s (X4 Word64) = MV_Word64X4 (P.MVector s (X4 Word64))
newtype instance VU.Vector     (X4 Word64) = V_Word64X4  (P.Vector    (X4 Word64))
instance VU.Unbox (X4 Word64)
primMVector((X4 Word64), MV_Word64X4)
primVector((X4 Word64), V_Word64X4, MV_Word64X4)

-- | FIXME: this is a huge hack to get around the fact that primitive vectors
-- do not export their constructors
data UnsafePrimVector a = UnsafePrimVector 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!ByteArray

-------------------------------------------------------------------------------
-- conversion functions

-- | converts an unboxed vector into one that will use the SIMD instructions
-- without performing bounds checks
{-# INLINE unsafeVectorizeUnboxedX4 #-}
unsafeVectorizeUnboxedX4 :: (SIMD4 a, VU.Unbox a) => VU.Vector a -> VU.Vector (X4 a)
unsafeVectorizeUnboxedX4 v = unsafeCoerce pv
    where
        pv = UnsafePrimVector (len `div` 4) (off `div` 4) arr
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts an unboxed vector into one that will use the SIMD instructions
-- while performing bounds checks (this just means an error will occur)
{-# INLINE vectorizeUnboxedX4 #-}
vectorizeUnboxedX4 :: (SIMD4 a, VU.Unbox a) => VU.Vector a -> VU.Vector (X4 a)
vectorizeUnboxedX4 v = if len `mod` 4 == 0 && off `mod` 4 == 0
    then unsafeCoerce pv
    else error "vectorizeUnboxedX4 vector wrong len/offset"
    where
        pv = UnsafePrimVector (len `div` 4) (off `div` 4) arr
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts an unboxed SIMD vector into a standard unboxed vector
{-# INLINE unVectorizeUnboxedX4 #-}
unVectorizeUnboxedX4 :: (SIMD4 a, VU.Unbox a) => VU.Vector (X4 a) -> VU.Vector a
unVectorizeUnboxedX4 v = unsafeCoerce v
    where
        pv = UnsafePrimVector (len*4) (off*4)
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts a storable vector into one that will use the SIMD instructions
{-# INLINE unsafeVectorizeStorableX4 #-}
unsafeVectorizeStorableX4 :: (SIMD4 a, Storable a, Storable (X4 a)) => VS.Vector a -> VS.Vector (X4 a)
unsafeVectorizeStorableX4 v = VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len `div` 4)
    where
        (fp,len) = VS.unsafeToForeignPtr0 v

-- | converts a storable SIMD vector into a standard vector
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
