{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- | This module wraps the SIMD operations that act on 2 values simultaneously.
module Data.SIMD2
    ( 
    
    -- * SIMD classes
      SIMD2 (..)
    , SIMD2Float (..)

    -- * conversion functions
    , unsafeVectorizeUnboxedX2
    , vectorizeUnboxedX2
    , unVectorizeUnboxedX2
    , vectorizeStorableX2
    , unVectorizeStorableX2
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
-- SIMD2

-- | this is a thin wrapper over the primitive operations
class SIMD2 a where
    data X2 a
    plusX2 :: X2 a -> X2 a -> X2 a
    minusX2 :: X2 a -> X2 a -> X2 a
    timesX2 :: X2 a -> X2 a -> X2 a
    negateX2 :: X2 a -> X2 a 
    indexArrayAsX2 :: ByteArray -> Int -> X2 a
    indexOffAddrAsX2 :: Addr -> Int -> X2 a
    insertX2 :: X2 a -> a -> Int -> X2 a
    unpackX2 :: X2 a -> (# a,a #)
    packX2 :: (# a,a #) -> X2 a
    broadcastX2 :: a -> X2 a
    readOffAddrAsX2 :: Addr# -> Int# -> State# s -> (# State# s, X2 a #)
    writeOffAddrAsX2 :: Addr# -> Int# -> X2 a -> State# s -> State# s

    -- | this operation is slow, avoid at all costs!
    {-# INLINE plusHorizontalX2 #-}
    plusHorizontalX2 :: (SIMD2 a, Num a) => X2 a -> a
    plusHorizontalX2 v = r1+r2
        where
            (# r1,r2 #) = unpackX2 v

    -- | this operation is slow, avoid at all costs!
    {-# INLINE timesHorizontalX2 #-}
    timesHorizontalX2 :: (SIMD2 a, Num a) => X2 a -> a
    timesHorizontalX2 v = r1*r2
        where
            (# r1,r2 #) = unpackX2 v

-- | this is a thin wrapper over the primitive division operation
class SIMD2 a => SIMD2Float a where
    divideX2 :: X2 a -> X2 a -> X2 a

instance (Fractional a, SIMD2Float a) => Fractional (X2 a) where
    (/) = divideX2
    fromRational = broadcastX2 . fromRational
    {-# INLINE (/) #-}
    {-# INLINE fromRational #-}

instance (Show a, SIMD2 a) => Show (X2 a) where
    show v = show (r1,r2)
        where (# r1,r2 #) = unpackX2 v

instance (Num a, SIMD2 a) => Num (X2 a) where
    (+) = plusX2
    (*) = timesX2
    (-) = minusX2
    negate = negateX2
    abs = error "SIMD2 abs not defined"
    signum = error "SIMD2 signum not defined"
    fromInteger i = broadcastX2 (fromInteger i::a)
    {-# INLINE (+) #-}
    {-# INLINE (*) #-}
    {-# INLINE (-) #-}
    {-# INLINE negate #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}

#define mkSIMD2(t,tt,cons,vec,plus,minus,times,negate,indexArray,indexOffAddr,insert,unpack,pack,broadcast,readOffAddr,writeOffAddr) \
instance SIMD2 t where\
    data X2 t = cons vec ;\
    plusX2 (cons v1#) (cons v2#) = cons (plus v1# v2#)          ;\
    minusX2 (cons v1#) (cons v2#) = cons (minus v1# v2#)        ;\
    timesX2 (cons v1#) (cons v2#) = cons (times v1# v2#)        ;\
    negateX2 (cons v1#) = cons (negate v1#)                     ;\
    indexArrayAsX2 (ByteArray ba#) (I# i#) = cons (indexArray ba# i#) ;\
    indexOffAddrAsX2 (Addr addr#) (I# i#) = cons (indexOffAddr addr# i#) ;\
    insertX2 (cons v1#) (tt s#) (I# i#) = cons (insert v1# s# i#) ;\
    unpackX2 (cons v1#) = let (# r1,r2 #) = unpack v1# in (# tt r1, tt r2 #) ;\
    packX2 (# tt r1,tt r2 #) = cons (pack (# r1,r2 #)) ;\
    broadcastX2 (tt r) = cons (broadcast r) ;\
    readOffAddrAsX2 addr# i# s# = case readOffAddr addr# (mul2 i#) s# of \
            { (# s1#, x# #) -> (# s1#, cons x# #) }                      ;\
    writeOffAddrAsX2 addr# i# (cons v1#) s# = writeOffAddr addr# (mul2 i#) v1# s# ;\
    {-# INLINE plusX2 #-} ;\
    {-# INLINE minusX2 #-} ;\
    {-# INLINE timesX2 #-} ;\
    {-# INLINE negateX2 #-} ;\
    {-# INLINE indexArrayAsX2 #-} ;\
    {-# INLINE indexOffAddrAsX2 #-} ;\
    {-# INLINE insertX2 #-} ;\
    {-# INLINE unpackX2 #-} ;\
    {-# INLINE packX2 #-} ;\
    {-# INLINE broadcastX2 #-} ;\
    {-# INLINE readOffAddrAsX2 #-} ;\
    {-# INLINE writeOffAddrAsX2 #-}

mkSIMD2(Double,D#,DoubleX2,DoubleX2#,
        plusDoubleX2#,minusDoubleX2#,timesDoubleX2#,negateDoubleX2#,
        indexDoubleArrayAsDoubleX2#,indexDoubleOffAddrAsDoubleX2#,
        insertDoubleX2#, unpackDoubleX2#, packDoubleX2#,broadcastDoubleX2#,
        readDoubleOffAddrAsDoubleX2#, writeDoubleOffAddrAsDoubleX2#
        )

instance SIMD2Float Double where 
    divideX2 (DoubleX2 v1#) (DoubleX2 v2#) = DoubleX2 (divideDoubleX2# v1# v2#)
    {-# INLINE divideX2 #-}

mkSIMD2(Word64,W64#,Word64X2,Word64X2#,
        plusWord64X2#,minusWord64X2#,timesWord64X2#,(error "cannot negate Word64X2"),
        indexWord64ArrayAsWord64X2#,indexWord64OffAddrAsWord64X2#,
        insertWord64X2#, unpackWord64X2#, packWord64X2#,broadcastWord64X2#,
        readWord64OffAddrAsWord64X2#, writeWord64OffAddrAsWord64X2#
        )

mkSIMD2(Int64,I64#,Int64X2,Int64X2#,
        plusInt64X2#,minusInt64X2#,timesInt64X2#,negateInt64X2#,
        indexInt64ArrayAsInt64X2#,indexInt64OffAddrAsInt64X2#,
        insertInt64X2#, unpackInt64X2#, packInt64X2#,broadcastInt64X2#,
        readInt64OffAddrAsInt64X2#, writeInt64OffAddrAsInt64X2#
        )

-------------------
-- Prim SIMD2

mul2 :: Int# -> Int#
mul2 i# = unI# (I# i# * 2)

#define derivePrim(ty, ctr, sz, align, idx_arr, rd_arr, wr_arr, set_arr, idx_addr, rd_addr, wr_addr, set_addr) \
instance Prim ty where {                                        \
  sizeOf# _ = unI# sz                                           \
; alignment# _ = unI# align                                     \
; indexByteArray# arr# i# = ctr (idx_arr arr# (mul2 i#))               \
; readByteArray#  arr# i# s# = case rd_arr arr# (mul2 i#) s# of        \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeByteArray# arr# i# (ctr x#) s# = wr_arr arr# (mul2 i#) x# s#    \
; {-setByteArray# arr# i# n# (ctr x#) s#                          \
    = case unsafeCoerce# (internal (set_arr arr# (unI# (I# i# * 2)) n# x#)) s# of \
            { (# s1#, _ #) -> s1# }                                 \
  -}                                                              \
; indexOffAddr# addr# i# = ctr (idx_addr addr# (mul2 i#))              \
; readOffAddr#  addr# i# s# = case rd_addr addr# (mul2 i#) s# of       \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeOffAddr# addr# i# (ctr x#) s# = wr_addr addr# (mul2 i#) x# s#   \
; {-# INLINE sizeOf# #-}                                        \
; {-# INLINE alignment# #-}                                     \
; {-# INLINE indexByteArray# #-}                                \
; {-# INLINE readByteArray# #-}                                 \
; {-# INLINE writeByteArray# #-}                                \
; {-# INLINE indexOffAddr# #-}                                  \
; {-# INLINE readOffAddr# #-}                                   \
; {-# INLINE writeOffAddr# #-}                                  \
}

derivePrim((X2 Double), DoubleX2, (sIZEOF_FLOAT*2), (aLIGNMENT_FLOAT*2),
           indexDoubleArrayAsDoubleX2#, readDoubleArrayAsDoubleX2#, writeDoubleArrayAsDoubleX2#, setDoubleArray#,
           indexDoubleOffAddrAsDoubleX2#, readDoubleOffAddrAsDoubleX2#, writeDoubleOffAddrAsDoubleX2#, setDoubleOffAddrAsDoubleX2#)

derivePrim((X2 Int64), Int64X2, (sIZEOF_FLOAT*2), (aLIGNMENT_FLOAT*2),
           indexInt64ArrayAsInt64X2#, readInt64ArrayAsInt64X2#, writeInt64ArrayAsInt64X2#, setInt64Array#,
           indexInt64OffAddrAsInt64X2#, readInt64OffAddrAsInt64X2#, writeInt64OffAddrAsInt64X2#, setInt64OffAddrAsInt64X2#)
            
derivePrim((X2 Word64), Word64X2, (sIZEOF_FLOAT*2), (aLIGNMENT_FLOAT*2),
           indexWord64ArrayAsWord64X2#, readWord64ArrayAsWord64X2#, writeWord64ArrayAsWord64X2#, setWord64Array#,
           indexWord64OffAddrAsWord64X2#, readWord64OffAddrAsWord64X2#, writeWord64OffAddrAsWord64X2#, setWord64OffAddrAsWord64X2#)

-------------------
-- Storable SIMD2

#define mkStorable(t) \
instance Storable (X2 t) where \
    sizeOf x = Data.Primitive.sizeOf x ;\
    alignment x = Data.Primitive.alignment x ;\
    peekElemOff (Ptr addr#) (I# i#) = primitive (readOffAddrAsX2 addr# i#) ;\
    pokeElemOff (Ptr addr#) (I# i#) a = primitive_ (writeOffAddrAsX2 addr# i# a) ;\
    {-# INLINE sizeOf #-} ;\
    {-# INLINE alignment #-} ;\
    {-# INLINE peekElemOff #-} ;\
    {-# INLINE pokeElemOff #-}

mkStorable(Double)
mkStorable(Int64)
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

newtype instance VUM.MVector s (X2 Double) = MV_DoubleX2 (P.MVector s (X2 Double))
newtype instance VU.Vector     (X2 Double) = V_DoubleX2  (P.Vector    (X2 Double))
instance VU.Unbox (X2 Double)
primMVector((X2 Double), MV_DoubleX2)
primVector((X2 Double), V_DoubleX2, MV_DoubleX2)

newtype instance VUM.MVector s (X2 Int64) = MV_Int64X2 (P.MVector s (X2 Int64))
newtype instance VU.Vector     (X2 Int64) = V_Int64X2  (P.Vector    (X2 Int64))
instance VU.Unbox (X2 Int64)
primMVector((X2 Int64), MV_Int64X2)
primVector((X2 Int64), V_Int64X2, MV_Int64X2)

newtype instance VUM.MVector s (X2 Word64) = MV_Word64X2 (P.MVector s (X2 Word64))
newtype instance VU.Vector     (X2 Word64) = V_Word64X2  (P.Vector    (X2 Word64))
instance VU.Unbox (X2 Word64)
primMVector((X2 Word64), MV_Word64X2)
primVector((X2 Word64), V_Word64X2, MV_Word64X2)

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
{-# INLINE unsafeVectorizeUnboxedX2 #-}
unsafeVectorizeUnboxedX2 :: (SIMD2 a, VU.Unbox a) => VU.Vector a -> VU.Vector (X2 a)
unsafeVectorizeUnboxedX2 v = unsafeCoerce pv
    where
        pv = UnsafePrimVector (len `div` 2) (off `div` 2) arr
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts an unboxed vector into one that will use the SIMD instructions
-- while performing bounds checks (this just means an error will occur)
{-# INLINE vectorizeUnboxedX2 #-}
vectorizeUnboxedX2 :: (SIMD2 a, VU.Unbox a) => VU.Vector a -> VU.Vector (X2 a)
vectorizeUnboxedX2 v = if len `mod` 2 == 0 && off `mod` 2 == 0
    then unsafeCoerce pv
    else error "vectorizeUnboxedX2 vector wrong len/offset"
    where
        pv = UnsafePrimVector (len `div` 2) (off `div` 2) arr
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts an unboxed SIMD vector into a standard unboxed vector
{-# INLINE unVectorizeUnboxedX2 #-}
unVectorizeUnboxedX2 :: (SIMD2 a, VU.Unbox a) => VU.Vector (X2 a) -> VU.Vector a
unVectorizeUnboxedX2 v = unsafeCoerce v
    where
        pv = UnsafePrimVector (len*2) (off*2)
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts a storable vector into one that will use the SIMD instructions
{-# INLINE unsafeVectorizeStorableX2 #-}
unsafeVectorizeStorableX2 :: (SIMD2 a, Storable a, Storable (X2 a)) => VS.Vector a -> VS.Vector (X2 a)
unsafeVectorizeStorableX2 v = VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len `div` 2)
    where
        (fp,len) = VS.unsafeToForeignPtr0 v

-- | converts a storable SIMD vector into a standard vector
{-# INLINE vectorizeStorableX2 #-}
vectorizeStorableX2 :: (SIMD2 a, Storable a, Storable (X2 a)) => VS.Vector a -> VS.Vector (X2 a)
vectorizeStorableX2 v = if (len `mod` 2 == 0) 
    then VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len `div` 2)
    else error "vectorizeStorableX2 vector wrong len"
    where
        (fp,len) = VS.unsafeToForeignPtr0 v

{-# INLINE unVectorizeStorableX2 #-}
unVectorizeStorableX2 :: (SIMD2 a, Storable a, Storable (X2 a)) => VS.Vector (X2 a) -> VS.Vector a
unVectorizeStorableX2 v = VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len*2)
    where
        (fp,len) = VS.unsafeToForeignPtr0 v
