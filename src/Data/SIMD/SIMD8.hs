{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- | This module wraps the SIMD operations that act on 8 values simultaneously.
module Data.SIMD.SIMD8
    ( 
    
    -- * SIMD classes
      SIMD8 (..)
    , SIMD8Float (..)

    -- * conversion functions
    , unsafeVectorizeUnboxedX8
    , vectorizeUnboxedX8
    , unVectorizeUnboxedX8
    , vectorizeStorableX8
    , unVectorizeStorableX8
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
-- SIMD8

-- | this is a thin wrapper over the primitive operations
class SIMD8 a where
    data X8 a
    plusX8 :: X8 a -> X8 a -> X8 a
    minusX8 :: X8 a -> X8 a -> X8 a
    timesX8 :: X8 a -> X8 a -> X8 a
    negateX8 :: X8 a -> X8 a 
    indexArrayAsX8 :: ByteArray -> Int -> X8 a
    indexOffAddrAsX8 :: Addr -> Int -> X8 a
    insertX8 :: X8 a -> a -> Int -> X8 a
    unpackX8 :: X8 a -> (# a,a,a,a,a,a,a,a #)
    packX8 :: (# a,a,a,a,a,a,a,a #) -> X8 a
    broadcastX8 :: a -> X8 a
    readOffAddrAsX8 :: Addr# -> Int# -> State# s -> (# State# s, X8 a #)
    writeOffAddrAsX8 :: Addr# -> Int# -> X8 a -> State# s -> State# s

    -- | this operation is slow, avoid at all costs!
    {-# INLINE plusHorizontalX8 #-}
    plusHorizontalX8 :: (SIMD8 a, Num a) => X8 a -> a
    plusHorizontalX8 v = r1+r2+r3+r4+r5+r6+r7+r8
        where
            (# r1,r2,r3,r4,r5,r6,r7,r8 #) = unpackX8 v

    -- | this operation is slow, avoid at all costs!
    {-# INLINE timesHorizontalX8 #-}
    timesHorizontalX8 :: (SIMD8 a, Num a) => X8 a -> a
    timesHorizontalX8 v = r1*r2*r3*r4*r5*r6*r7*r8
        where
            (# r1,r2,r3,r4,r5,r6,r7,r8 #) = unpackX8 v

-- | this is a thin wrapper over the primitive division operation
class SIMD8 a => SIMD8Float a where
    divideX8 :: X8 a -> X8 a -> X8 a

instance (Fractional a, SIMD8Float a) => Fractional (X8 a) where
    (/) = divideX8
    fromRational = broadcastX8 . fromRational
    {-# INLINE (/) #-}
    {-# INLINE fromRational #-}

instance (Show a, SIMD8 a) => Show (X8 a) where
    show v = show (r1,r2,r3,r4,r5,r6,r7,r8)
        where (# r1,r2,r3,r4,r5,r6,r7,r8 #) = unpackX8 v

instance (Num a, SIMD8 a) => Num (X8 a) where
    (+) = plusX8
    (*) = timesX8
    (-) = minusX8
    negate = negateX8
    abs = error "SIMD8 abs not defined"
    signum = error "SIMD8 signum not defined"
    fromInteger i = broadcastX8 (fromInteger i::a)
    {-# INLINE (+) #-}
    {-# INLINE (*) #-}
    {-# INLINE (-) #-}
    {-# INLINE negate #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}

#define mkSIMD8(t,tt,cons,vec,plus,minus,times,negate,indexArray,indexOffAddr,insert,unpack,pack,broadcast,readOffAddr,writeOffAddr) \
instance SIMD8 t where\
    data X8 t = cons vec ;\
    plusX8 (cons v1#) (cons v2#) = cons (plus v1# v2#)          ;\
    minusX8 (cons v1#) (cons v2#) = cons (minus v1# v2#)        ;\
    timesX8 (cons v1#) (cons v2#) = cons (times v1# v2#)        ;\
    negateX8 (cons v1#) = cons (negate v1#)                     ;\
    indexArrayAsX8 (ByteArray ba#) (I# i#) = cons (indexArray ba# i#) ;\
    indexOffAddrAsX8 (Addr addr#) (I# i#) = cons (indexOffAddr addr# i#) ;\
    insertX8 (cons v1#) (tt s#) (I# i#) = cons (insert v1# s# i#) ;\
    unpackX8 (cons v1#) = let (# r1,r2,r3,r4,r5,r6,r7,r8 #) = unpack v1# in (# tt r1, tt r2, tt r3, tt r4, tt r5, tt r6, tt r7, tt r8 #) ;\
    packX8 (# tt r1,tt r2, tt r3, tt r4, tt r5, tt r6, tt r7, tt r8 #) = cons (pack (# r1,r2,r3,r4,r5,r6,r7,r8 #)) ;\
    broadcastX8 (tt r) = cons (broadcast r) ;\
    readOffAddrAsX8 addr# i# s# = case readOffAddr addr# (mul8 i#) s# of \
            { (# s1#, x# #) -> (# s1#, cons x# #) }                      ;\
    writeOffAddrAsX8 addr# i# (cons v1#) s# = writeOffAddr addr# (mul8 i#) v1# s# ;\
    {-# INLINE plusX8 #-} ;\
    {-# INLINE minusX8 #-} ;\
    {-# INLINE timesX8 #-} ;\
    {-# INLINE negateX8 #-} ;\
    {-# INLINE indexArrayAsX8 #-} ;\
    {-# INLINE indexOffAddrAsX8 #-} ;\
    {-# INLINE insertX8 #-} ;\
    {-# INLINE unpackX8 #-} ;\
    {-# INLINE packX8 #-} ;\
    {-# INLINE broadcastX8 #-} ;\
    {-# INLINE readOffAddrAsX8 #-} ;\
    {-# INLINE writeOffAddrAsX8 #-}

mkSIMD8(Float,F#,FloatX8,FloatX8#,
        plusFloatX8#,minusFloatX8#,timesFloatX8#,negateFloatX8#,
        indexFloatArrayAsFloatX8#,indexFloatOffAddrAsFloatX8#,
        insertFloatX8#, unpackFloatX8#, packFloatX8#,broadcastFloatX8#,
        readFloatOffAddrAsFloatX8#, writeFloatOffAddrAsFloatX8#
        )

instance SIMD8Float Float where 
    divideX8 (FloatX8 v1#) (FloatX8 v2#) = FloatX8 (divideFloatX8# v1# v2#)
    {-# INLINE divideX8 #-}

mkSIMD8(Double,D#,DoubleX8,DoubleX8#,
        plusDoubleX8#,minusDoubleX8#,timesDoubleX8#,negateDoubleX8#,
        indexDoubleArrayAsDoubleX8#,indexDoubleOffAddrAsDoubleX8#,
        insertDoubleX8#, unpackDoubleX8#, packDoubleX8#,broadcastDoubleX8#,
        readDoubleOffAddrAsDoubleX8#, writeDoubleOffAddrAsDoubleX8#
        )

instance SIMD8Float Double where 
    divideX8 (DoubleX8 v1#) (DoubleX8 v2#) = DoubleX8 (divideDoubleX8# v1# v2#)
    {-# INLINE divideX8 #-}

mkSIMD8(Word32,W32#,Word32X8,Word32X8#,
        plusWord32X8#,minusWord32X8#,timesWord32X8#,(error "cannot negate Word32X8"),
        indexWord32ArrayAsWord32X8#,indexWord32OffAddrAsWord32X8#,
        insertWord32X8#, unpackWord32X8#, packWord32X8#,broadcastWord32X8#,
        readWord32OffAddrAsWord32X8#, writeWord32OffAddrAsWord32X8#
        )

mkSIMD8(Word64,W64#,Word64X8,Word64X8#,
        plusWord64X8#,minusWord64X8#,timesWord64X8#,(error "cannot negate Word64X8"),
        indexWord64ArrayAsWord64X8#,indexWord64OffAddrAsWord64X8#,
        insertWord64X8#, unpackWord64X8#, packWord64X8#,broadcastWord64X8#,
        readWord64OffAddrAsWord64X8#, writeWord64OffAddrAsWord64X8#
        )

mkSIMD8(Int32,I32#,Int32X8,Int32X8#,
        plusInt32X8#,minusInt32X8#,timesInt32X8#,negateInt32X8#,
        indexInt32ArrayAsInt32X8#,indexInt32OffAddrAsInt32X8#,
        insertInt32X8#, unpackInt32X8#, packInt32X8#,broadcastInt32X8#,
        readInt32OffAddrAsInt32X8#, writeInt32OffAddrAsInt32X8#
        )

mkSIMD8(Int64,I64#,Int64X8,Int64X8#,
        plusInt64X8#,minusInt64X8#,timesInt64X8#,negateInt64X8#,
        indexInt64ArrayAsInt64X8#,indexInt64OffAddrAsInt64X8#,
        insertInt64X8#, unpackInt64X8#, packInt64X8#,broadcastInt64X8#,
        readInt64OffAddrAsInt64X8#, writeInt64OffAddrAsInt64X8#
        )

-------------------
-- Prim SIMD8

mul8 :: Int# -> Int#
mul8 i# = unI# (I# i# * 8)

#define derivePrim(ty, ctr, sz, align, idx_arr, rd_arr, wr_arr, set_arr, idx_addr, rd_addr, wr_addr, set_addr) \
instance Prim ty where {                                        \
  sizeOf# _ = unI# sz                                           \
; alignment# _ = unI# align                                     \
; indexByteArray# arr# i# = ctr (idx_arr arr# (mul8 i#))               \
; readByteArray#  arr# i# s# = case rd_arr arr# (mul8 i#) s# of        \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeByteArray# arr# i# (ctr x#) s# = wr_arr arr# (mul8 i#) x# s#    \
; {-setByteArray# arr# i# n# (ctr x#) s#                          \
    = case unsafeCoerce# (internal (set_arr arr# (unI# (I# i# * 8)) n# x#)) s# of \
            { (# s1#, _ #) -> s1# }                                 \
  -}                                                              \
; indexOffAddr# addr# i# = ctr (idx_addr addr# (mul8 i#))              \
; readOffAddr#  addr# i# s# = case rd_addr addr# (mul8 i#) s# of       \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeOffAddr# addr# i# (ctr x#) s# = wr_addr addr# (mul8 i#) x# s#   \
; {-# INLINE sizeOf# #-}                                        \
; {-# INLINE alignment# #-}                                     \
; {-# INLINE indexByteArray# #-}                                \
; {-# INLINE readByteArray# #-}                                 \
; {-# INLINE writeByteArray# #-}                                \
; {-# INLINE indexOffAddr# #-}                                  \
; {-# INLINE readOffAddr# #-}                                   \
; {-# INLINE writeOffAddr# #-}                                  \
}

derivePrim((X8 Float), FloatX8, (sIZEOF_FLOAT*8), (aLIGNMENT_FLOAT*8),
           indexFloatArrayAsFloatX8#, readFloatArrayAsFloatX8#, writeFloatArrayAsFloatX8#, setFloatArray#,
           indexFloatOffAddrAsFloatX8#, readFloatOffAddrAsFloatX8#, writeFloatOffAddrAsFloatX8#, setFloatOffAddrAsFloatX8#)
            
derivePrim((X8 Double), DoubleX8, (sIZEOF_FLOAT*8), (aLIGNMENT_FLOAT*8),
           indexDoubleArrayAsDoubleX8#, readDoubleArrayAsDoubleX8#, writeDoubleArrayAsDoubleX8#, setDoubleArray#,
           indexDoubleOffAddrAsDoubleX8#, readDoubleOffAddrAsDoubleX8#, writeDoubleOffAddrAsDoubleX8#, setDoubleOffAddrAsDoubleX8#)

derivePrim((X8 Int32), Int32X8, (sIZEOF_FLOAT*8), (aLIGNMENT_FLOAT*8),
           indexInt32ArrayAsInt32X8#, readInt32ArrayAsInt32X8#, writeInt32ArrayAsInt32X8#, setInt32Array#,
           indexInt32OffAddrAsInt32X8#, readInt32OffAddrAsInt32X8#, writeInt32OffAddrAsInt32X8#, setInt32OffAddrAsInt32X8#)
            
derivePrim((X8 Int64), Int64X8, (sIZEOF_FLOAT*8), (aLIGNMENT_FLOAT*8),
           indexInt64ArrayAsInt64X8#, readInt64ArrayAsInt64X8#, writeInt64ArrayAsInt64X8#, setInt64Array#,
           indexInt64OffAddrAsInt64X8#, readInt64OffAddrAsInt64X8#, writeInt64OffAddrAsInt64X8#, setInt64OffAddrAsInt64X8#)
            
derivePrim((X8 Word32), Word32X8, (sIZEOF_FLOAT*8), (aLIGNMENT_FLOAT*8),
           indexWord32ArrayAsWord32X8#, readWord32ArrayAsWord32X8#, writeWord32ArrayAsWord32X8#, setWord32Array#,
           indexWord32OffAddrAsWord32X8#, readWord32OffAddrAsWord32X8#, writeWord32OffAddrAsWord32X8#, setWord32OffAddrAsWord32X8#)
            
derivePrim((X8 Word64), Word64X8, (sIZEOF_FLOAT*8), (aLIGNMENT_FLOAT*8),
           indexWord64ArrayAsWord64X8#, readWord64ArrayAsWord64X8#, writeWord64ArrayAsWord64X8#, setWord64Array#,
           indexWord64OffAddrAsWord64X8#, readWord64OffAddrAsWord64X8#, writeWord64OffAddrAsWord64X8#, setWord64OffAddrAsWord64X8#)

-------------------
-- Storable SIMD8

#define mkStorable(t) \
instance Storable (X8 t) where \
    sizeOf x = Data.Primitive.sizeOf x ;\
    alignment x = Data.Primitive.alignment x ;\
    peekElemOff (Ptr addr#) (I# i#) = primitive (readOffAddrAsX8 addr# i#) ;\
    pokeElemOff (Ptr addr#) (I# i#) a = primitive_ (writeOffAddrAsX8 addr# i# a) ;\
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

newtype instance VUM.MVector s (X8 Float) = MV_FloatX8 (P.MVector s (X8 Float))
newtype instance VU.Vector     (X8 Float) = V_FloatX8  (P.Vector    (X8 Float))
instance VU.Unbox (X8 Float)
primMVector((X8 Float), MV_FloatX8)
primVector((X8 Float), V_FloatX8, MV_FloatX8)

newtype instance VUM.MVector s (X8 Double) = MV_DoubleX8 (P.MVector s (X8 Double))
newtype instance VU.Vector     (X8 Double) = V_DoubleX8  (P.Vector    (X8 Double))
instance VU.Unbox (X8 Double)
primMVector((X8 Double), MV_DoubleX8)
primVector((X8 Double), V_DoubleX8, MV_DoubleX8)

newtype instance VUM.MVector s (X8 Int32) = MV_Int32X8 (P.MVector s (X8 Int32))
newtype instance VU.Vector     (X8 Int32) = V_Int32X8  (P.Vector    (X8 Int32))
instance VU.Unbox (X8 Int32)
primMVector((X8 Int32), MV_Int32X8)
primVector((X8 Int32), V_Int32X8, MV_Int32X8)

newtype instance VUM.MVector s (X8 Int64) = MV_Int64X8 (P.MVector s (X8 Int64))
newtype instance VU.Vector     (X8 Int64) = V_Int64X8  (P.Vector    (X8 Int64))
instance VU.Unbox (X8 Int64)
primMVector((X8 Int64), MV_Int64X8)
primVector((X8 Int64), V_Int64X8, MV_Int64X8)

newtype instance VUM.MVector s (X8 Word32) = MV_Word32X8 (P.MVector s (X8 Word32))
newtype instance VU.Vector     (X8 Word32) = V_Word32X8  (P.Vector    (X8 Word32))
instance VU.Unbox (X8 Word32)
primMVector((X8 Word32), MV_Word32X8)
primVector((X8 Word32), V_Word32X8, MV_Word32X8)

newtype instance VUM.MVector s (X8 Word64) = MV_Word64X8 (P.MVector s (X8 Word64))
newtype instance VU.Vector     (X8 Word64) = V_Word64X8  (P.Vector    (X8 Word64))
instance VU.Unbox (X8 Word64)
primMVector((X8 Word64), MV_Word64X8)
primVector((X8 Word64), V_Word64X8, MV_Word64X8)

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
{-# INLINE unsafeVectorizeUnboxedX8 #-}
unsafeVectorizeUnboxedX8 :: (SIMD8 a, VU.Unbox a) => VU.Vector a -> VU.Vector (X8 a)
unsafeVectorizeUnboxedX8 v = unsafeCoerce pv
    where
        pv = UnsafePrimVector (len `div` 8) (off `div` 8) arr
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts an unboxed vector into one that will use the SIMD instructions
-- while performing bounds checks (this just means an error will occur)
{-# INLINE vectorizeUnboxedX8 #-}
vectorizeUnboxedX8 :: (SIMD8 a, VU.Unbox a) => VU.Vector a -> VU.Vector (X8 a)
vectorizeUnboxedX8 v = if len `mod` 8 == 0 && off `mod` 8 == 0
    then unsafeCoerce pv
    else error "vectorizeUnboxedX8 vector wrong len/offset"
    where
        pv = UnsafePrimVector (len `div` 8) (off `div` 8) arr
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts an unboxed SIMD vector into a standard unboxed vector
{-# INLINE unVectorizeUnboxedX8 #-}
unVectorizeUnboxedX8 :: (SIMD8 a, VU.Unbox a) => VU.Vector (X8 a) -> VU.Vector a
unVectorizeUnboxedX8 v = unsafeCoerce v
    where
        pv = UnsafePrimVector (len*8) (off*8)
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts a storable vector into one that will use the SIMD instructions
{-# INLINE unsafeVectorizeStorableX8 #-}
unsafeVectorizeStorableX8 :: (SIMD8 a, Storable a, Storable (X8 a)) => VS.Vector a -> VS.Vector (X8 a)
unsafeVectorizeStorableX8 v = VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len `div` 8)
    where
        (fp,len) = VS.unsafeToForeignPtr0 v

-- | converts a storable SIMD vector into a standard vector
{-# INLINE vectorizeStorableX8 #-}
vectorizeStorableX8 :: (SIMD8 a, Storable a, Storable (X8 a)) => VS.Vector a -> VS.Vector (X8 a)
vectorizeStorableX8 v = if (len `mod` 8 == 0) 
    then VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len `div` 8)
    else error "vectorizeStorableX8 vector wrong len"
    where
        (fp,len) = VS.unsafeToForeignPtr0 v

{-# INLINE unVectorizeStorableX8 #-}
unVectorizeStorableX8 :: (SIMD8 a, Storable a, Storable (X8 a)) => VS.Vector (X8 a) -> VS.Vector a
unVectorizeStorableX8 v = VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len*8)
    where
        (fp,len) = VS.unsafeToForeignPtr0 v
