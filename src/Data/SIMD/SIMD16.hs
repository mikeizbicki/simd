{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- | This module wraps the SIMD operations that act on 16 values simultaneously.
module Data.SIMD.SIMD16
    ( 
    
    -- * SIMD classes
      SIMD16 (..)
    , SIMD16Float (..)

    -- * conversion functions
    , unsafeVectorizeUnboxedX16
    , vectorizeUnboxedX16
    , unVectorizeUnboxedX16
    , vectorizeStorableX16
    , unVectorizeStorableX16
    )
    where

import Control.Monad
import Control.Monad.Primitive
import Data.List
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
-- SIMD16

-- | this is a thin wrapper over the primitive operations
class SIMD16 a where
    data X16 a
    plusX16 :: X16 a -> X16 a -> X16 a
    minusX16 :: X16 a -> X16 a -> X16 a
    timesX16 :: X16 a -> X16 a -> X16 a
    negateX16 :: X16 a -> X16 a 
    indexArrayAsX16 :: ByteArray -> Int -> X16 a
    indexOffAddrAsX16 :: Addr -> Int -> X16 a
    insertX16 :: X16 a -> a -> Int -> X16 a
    unpackX16 :: X16 a -> (# a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a #)
    packX16 :: (# a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a #) -> X16 a
    broadcastX16 :: a -> X16 a
    readOffAddrAsX16 :: Addr# -> Int# -> State# s -> (# State# s, X16 a #)
    writeOffAddrAsX16 :: Addr# -> Int# -> X16 a -> State# s -> State# s

    -- | this operation is slow, avoid at all costs!
    {-# INLINE plusHorizontalX16 #-}
    plusHorizontalX16 :: (SIMD16 a, Num a) => X16 a -> a
    plusHorizontalX16 v = r1+r2+r3+r4+r5+r6+r7+r8+r9+r10+r11+r12+r13+r14+r15+r16
        where
            (# r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16 #) = unpackX16 v

    -- | this operation is slow, avoid at all costs!
    {-# INLINE timesHorizontalX16 #-}
    timesHorizontalX16 :: (SIMD16 a, Num a) => X16 a -> a
    timesHorizontalX16 v = r1*r2*r3*r4*r5*r6*r7*r8*r9*r10*r11*r12*r13*r14*r15*r16
        where
            (# r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16 #) = unpackX16 v

-- | this is a thin wrapper over the primitive division operation
class SIMD16 a => SIMD16Float a where
    divideX16 :: X16 a -> X16 a -> X16 a

instance (Fractional a, SIMD16Float a) => Fractional (X16 a) where
    (/) = divideX16
    fromRational = broadcastX16 . fromRational
    {-# INLINE (/) #-}
    {-# INLINE fromRational #-}

instance (Show a, SIMD16 a) => Show (X16 a) where
    show v = (init $ show (r1,r2,r3,r4,r5,r6,r7,r8))
          ++ ","
          ++ (tail $ show (r9,r10,r11,r12,r13,r14,r15,r16))
        where (# r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16 #) = unpackX16 v

instance (Num a, SIMD16 a) => Num (X16 a) where
    (+) = plusX16
    (*) = timesX16
    (-) = minusX16
    negate = negateX16
    abs = error "SIMD16 abs not defined"
    signum = error "SIMD16 signum not defined"
    fromInteger i = broadcastX16 (fromInteger i::a)
    {-# INLINE (+) #-}
    {-# INLINE (*) #-}
    {-# INLINE (-) #-}
    {-# INLINE negate #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}

#define mkSIMD16(t,tt,cons,vec,plus,minus,times,negate,indexArray,indexOffAddr,insert,unpack,pack,broadcast,readOffAddr,writeOffAddr) \
instance SIMD16 t where\
    data X16 t = cons vec ;\
    plusX16 (cons v1#) (cons v2#) = cons (plus v1# v2#)          ;\
    minusX16 (cons v1#) (cons v2#) = cons (minus v1# v2#)        ;\
    timesX16 (cons v1#) (cons v2#) = cons (times v1# v2#)        ;\
    negateX16 (cons v1#) = cons (negate v1#)                     ;\
    indexArrayAsX16 (ByteArray ba#) (I# i#) = cons (indexArray ba# i#) ;\
    indexOffAddrAsX16 (Addr addr#) (I# i#) = cons (indexOffAddr addr# i#) ;\
    insertX16 (cons v1#) (tt s#) (I# i#) = cons (insert v1# s# i#) ;\
    unpackX16 (cons v1#) = let (# r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16 #) = unpack v1# in (# tt r1, tt r2, tt r3, tt r4, tt r5, tt r6, tt r7, tt r8, tt r9, tt r10, tt r11, tt r12, tt r13, tt r14, tt r15, tt r16 #) ;\
    packX16 (# tt r1,tt r2, tt r3, tt r4, tt r5, tt r6, tt r7, tt r8, tt r9, tt r10, tt r11, tt r12, tt r13, tt r14, tt r15, tt r16 #) = cons (pack (# r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16 #)) ;\
    broadcastX16 (tt r) = cons (broadcast r) ;\
    readOffAddrAsX16 addr# i# s# = case readOffAddr addr# (mul16 i#) s# of \
            { (# s1#, x# #) -> (# s1#, cons x# #) }                      ;\
    writeOffAddrAsX16 addr# i# (cons v1#) s# = writeOffAddr addr# (mul16 i#) v1# s# ;\
    {-# INLINE plusX16 #-} ;\
    {-# INLINE minusX16 #-} ;\
    {-# INLINE timesX16 #-} ;\
    {-# INLINE negateX16 #-} ;\
    {-# INLINE indexArrayAsX16 #-} ;\
    {-# INLINE indexOffAddrAsX16 #-} ;\
    {-# INLINE insertX16 #-} ;\
    {-# INLINE unpackX16 #-} ;\
    {-# INLINE packX16 #-} ;\
    {-# INLINE broadcastX16 #-} ;\
    {-# INLINE readOffAddrAsX16 #-} ;\
    {-# INLINE writeOffAddrAsX16 #-}

mkSIMD16(Float,F#,FloatX16,FloatX16#,
        plusFloatX16#,minusFloatX16#,timesFloatX16#,negateFloatX16#,
        indexFloatArrayAsFloatX16#,indexFloatOffAddrAsFloatX16#,
        insertFloatX16#, unpackFloatX16#, packFloatX16#,broadcastFloatX16#,
        readFloatOffAddrAsFloatX16#, writeFloatOffAddrAsFloatX16#
        )

instance SIMD16Float Float where 
    divideX16 (FloatX16 v1#) (FloatX16 v2#) = FloatX16 (divideFloatX16# v1# v2#)
    {-# INLINE divideX16 #-}

mkSIMD16(Word32,W32#,Word32X16,Word32X16#,
        plusWord32X16#,minusWord32X16#,timesWord32X16#,(error "cannot negate Word32X16"),
        indexWord32ArrayAsWord32X16#,indexWord32OffAddrAsWord32X16#,
        insertWord32X16#, unpackWord32X16#, packWord32X16#,broadcastWord32X16#,
        readWord32OffAddrAsWord32X16#, writeWord32OffAddrAsWord32X16#
        )

mkSIMD16(Int32,I32#,Int32X16,Int32X16#,
        plusInt32X16#,minusInt32X16#,timesInt32X16#,negateInt32X16#,
        indexInt32ArrayAsInt32X16#,indexInt32OffAddrAsInt32X16#,
        insertInt32X16#, unpackInt32X16#, packInt32X16#,broadcastInt32X16#,
        readInt32OffAddrAsInt32X16#, writeInt32OffAddrAsInt32X16#
        )

-------------------
-- Prim SIMD16

mul16 :: Int# -> Int#
mul16 i# = unI# (I# i# * 16)

#define derivePrim(ty, ctr, sz, align, idx_arr, rd_arr, wr_arr, set_arr, idx_addr, rd_addr, wr_addr, set_addr) \
instance Prim ty where {                                        \
  sizeOf# _ = unI# sz                                           \
; alignment# _ = unI# align                                     \
; indexByteArray# arr# i# = ctr (idx_arr arr# (mul16 i#))               \
; readByteArray#  arr# i# s# = case rd_arr arr# (mul16 i#) s# of        \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeByteArray# arr# i# (ctr x#) s# = wr_arr arr# (mul16 i#) x# s#    \
; {-setByteArray# arr# i# n# (ctr x#) s#                          \
    = case unsafeCoerce# (internal (set_arr arr# (unI# (I# i# * 16)) n# x#)) s# of \
            { (# s1#, _ #) -> s1# }                                 \
  -}                                                              \
; indexOffAddr# addr# i# = ctr (idx_addr addr# (mul16 i#))              \
; readOffAddr#  addr# i# s# = case rd_addr addr# (mul16 i#) s# of       \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeOffAddr# addr# i# (ctr x#) s# = wr_addr addr# (mul16 i#) x# s#   \
; {-# INLINE sizeOf# #-}                                        \
; {-# INLINE alignment# #-}                                     \
; {-# INLINE indexByteArray# #-}                                \
; {-# INLINE readByteArray# #-}                                 \
; {-# INLINE writeByteArray# #-}                                \
; {-# INLINE indexOffAddr# #-}                                  \
; {-# INLINE readOffAddr# #-}                                   \
; {-# INLINE writeOffAddr# #-}                                  \
}

derivePrim((X16 Float), FloatX16, (sIZEOF_FLOAT*16), (aLIGNMENT_FLOAT*16),
           indexFloatArrayAsFloatX16#, readFloatArrayAsFloatX16#, writeFloatArrayAsFloatX16#, setFloatArray#,
           indexFloatOffAddrAsFloatX16#, readFloatOffAddrAsFloatX16#, writeFloatOffAddrAsFloatX16#, setFloatOffAddrAsFloatX16#)
            
derivePrim((X16 Int32), Int32X16, (sIZEOF_FLOAT*16), (aLIGNMENT_FLOAT*16),
           indexInt32ArrayAsInt32X16#, readInt32ArrayAsInt32X16#, writeInt32ArrayAsInt32X16#, setInt32Array#,
           indexInt32OffAddrAsInt32X16#, readInt32OffAddrAsInt32X16#, writeInt32OffAddrAsInt32X16#, setInt32OffAddrAsInt32X16#)
            
derivePrim((X16 Word32), Word32X16, (sIZEOF_FLOAT*16), (aLIGNMENT_FLOAT*16),
           indexWord32ArrayAsWord32X16#, readWord32ArrayAsWord32X16#, writeWord32ArrayAsWord32X16#, setWord32Array#,
           indexWord32OffAddrAsWord32X16#, readWord32OffAddrAsWord32X16#, writeWord32OffAddrAsWord32X16#, setWord32OffAddrAsWord32X16#)
            
-------------------
-- Storable SIMD16

#define mkStorable(t) \
instance Storable (X16 t) where \
    sizeOf x = Data.Primitive.sizeOf x ;\
    alignment x = Data.Primitive.alignment x ;\
    peekElemOff (Ptr addr#) (I# i#) = primitive (readOffAddrAsX16 addr# i#) ;\
    pokeElemOff (Ptr addr#) (I# i#) a = primitive_ (writeOffAddrAsX16 addr# i# a) ;\
    {-# INLINE sizeOf #-} ;\
    {-# INLINE alignment #-} ;\
    {-# INLINE peekElemOff #-} ;\
    {-# INLINE pokeElemOff #-}

mkStorable(Float)
mkStorable(Int32)
mkStorable(Word32)

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

newtype instance VUM.MVector s (X16 Float) = MV_FloatX16 (P.MVector s (X16 Float))
newtype instance VU.Vector     (X16 Float) = V_FloatX16  (P.Vector    (X16 Float))
instance VU.Unbox (X16 Float)
primMVector((X16 Float), MV_FloatX16)
primVector((X16 Float), V_FloatX16, MV_FloatX16)

newtype instance VUM.MVector s (X16 Int32) = MV_Int32X16 (P.MVector s (X16 Int32))
newtype instance VU.Vector     (X16 Int32) = V_Int32X16  (P.Vector    (X16 Int32))
instance VU.Unbox (X16 Int32)
primMVector((X16 Int32), MV_Int32X16)
primVector((X16 Int32), V_Int32X16, MV_Int32X16)

newtype instance VUM.MVector s (X16 Word32) = MV_Word32X16 (P.MVector s (X16 Word32))
newtype instance VU.Vector     (X16 Word32) = V_Word32X16  (P.Vector    (X16 Word32))
instance VU.Unbox (X16 Word32)
primMVector((X16 Word32), MV_Word32X16)
primVector((X16 Word32), V_Word32X16, MV_Word32X16)

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
{-# INLINE unsafeVectorizeUnboxedX16 #-}
unsafeVectorizeUnboxedX16 :: (SIMD16 a, VU.Unbox a) => VU.Vector a -> VU.Vector (X16 a)
unsafeVectorizeUnboxedX16 v = unsafeCoerce pv
    where
        pv = UnsafePrimVector (len `div` 16) (off `div` 16) arr
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts an unboxed vector into one that will use the SIMD instructions
-- while performing bounds checks (this just means an error will occur)
{-# INLINE vectorizeUnboxedX16 #-}
vectorizeUnboxedX16 :: (SIMD16 a, VU.Unbox a) => VU.Vector a -> VU.Vector (X16 a)
vectorizeUnboxedX16 v = if len `mod` 16 == 0 && off `mod` 16 == 0
    then unsafeCoerce pv
    else error "vectorizeUnboxedX16 vector wrong len/offset"
    where
        pv = UnsafePrimVector (len `div` 16) (off `div` 16) arr
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts an unboxed SIMD vector into a standard unboxed vector
{-# INLINE unVectorizeUnboxedX16 #-}
unVectorizeUnboxedX16 :: (SIMD16 a, VU.Unbox a) => VU.Vector (X16 a) -> VU.Vector a
unVectorizeUnboxedX16 v = unsafeCoerce pv
    where
        pv = UnsafePrimVector (len*16) (off*16) arr
        UnsafePrimVector len off arr = unsafeCoerce v

-- | converts a storable vector into one that will use the SIMD instructions
{-# INLINE unsafeVectorizeStorableX16 #-}
unsafeVectorizeStorableX16 :: (SIMD16 a, Storable a, Storable (X16 a)) => VS.Vector a -> VS.Vector (X16 a)
unsafeVectorizeStorableX16 v = VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len `div` 16)
    where
        (fp,len) = VS.unsafeToForeignPtr0 v

-- | converts a storable SIMD vector into a standard vector
{-# INLINE vectorizeStorableX16 #-}
vectorizeStorableX16 :: (SIMD16 a, Storable a, Storable (X16 a)) => VS.Vector a -> VS.Vector (X16 a)
vectorizeStorableX16 v = if (len `mod` 16 == 0) 
    then VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len `div` 16)
    else error "vectorizeStorableX16 vector wrong len"
    where
        (fp,len) = VS.unsafeToForeignPtr0 v

{-# INLINE unVectorizeStorableX16 #-}
unVectorizeStorableX16 :: (SIMD16 a, Storable a, Storable (X16 a)) => VS.Vector (X16 a) -> VS.Vector a
unVectorizeStorableX16 v = VS.unsafeFromForeignPtr0 (castForeignPtr fp) (len*16)
    where
        (fp,len) = VS.unsafeToForeignPtr0 v
