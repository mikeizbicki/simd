simd
====

This library provides a simple interface to the SIMD primops provided in GHC 7.8.
SIMD (Single Instruction Multiple Data) CPU instructions provide an easy way to parallelize numeric computations. 
GHC 7.8 provides primops that let us access these CPU instructions. 
This package wraps thos primops in a more user friendly form.
These primops can only be used with the llvm backend, so you must have llvm installed and compile any program using this library with the -llvm option.


The API is in two parts. 
First, it provides a thin wrapper around the primops in the same style as the Data.Primitive API. 
This wrapper is then augmented with Num (and where appropriate) Fractional instances that make access to the SIMD types much more convenient.
Second, it provides an interface for working with vectors in parallel. 
This interface consists of Unbox and Storable instances for the SIMD types, and efficient methods for converting between a SIMD Vector and a standard vector.

The examples folder contains criterion benchmarks for calculating the L2 distance between two vectors.
The folder provides many different ways to calculate the distance.
Note that the versions that rely on higher order functions are extremely slow with the SIMD instructions, and so that style of programming should not be used.

It might be useful to provide some rewrite rules to make vectorization an automatic performance improvement.
This is not easy to do with the current Data.Vector API, however, because all the vector operations get inlined before the rules can fire.
In any case, people who care enough about performance to want SIMD instructions will probably want to write the SIMD code manually.

Performance graphs
-------

These performance graphs measure how long it takes to calculate the Euclidean distance between two vectors of different lengths.
For large vectors, almost all the work can be done in parallel and SIMD instructions work very well.
For small vectors, non parallelizable instructions and initialization overhead become more important.
For vectors of size 16, we actually run slower using the simd operations.
I suspect with some fiddling around, this overhead could be reduced.
But I don't ever use vectors that small, so I didn't bother trying.

Performance tests will vary a lot depending on what computer you are using and what instructions it supports well.
I ran these tests on my 64 bit Core 2 Duo laptop.

<p align=center>
<img src="http://izbicki.me/public/cs/github/summary16000.png" alt="graph" />
</p>
<p align=center>
<img src="http://izbicki.me/public/cs/github/summary1600.png" alt="graph" />
</p>
<p align=center>
<img src="http://izbicki.me/public/cs/github/summary160.png" alt="graph" />
</p>
<p align=center>
<img src="http://izbicki.me/public/cs/github/summary16.png" alt="graph" />
</p>
