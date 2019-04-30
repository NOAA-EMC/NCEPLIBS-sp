# *** manually set environments (for gnu compiler) of sp ***

# !!! module environment (*THEIA*) !!!
 module load gcc/6.2.0

 ANCHORDIR=..
 export COMP=gnu
 export SP_VER=v2.0.2
 export SP_SRC=
 export SP_LIB4=$ANCHORDIR/libsp_${SP_VER}_4.a
 export SP_LIB8=$ANCHORDIR/libsp_${SP_VER}_8.a
 export SP_LIBd=$ANCHORDIR/libsp_${SP_VER}_d.a

 export CC=gcc
 export FC=gfortran
 export CPP=cpp
 export OMPCC="$CC -fopenmp"
 export OMPFC="$FC -fopenmp"
 export MPICC=mpigcc
 export MPIFC=mpigfortran

 export DEBUG="-g -O0"
 export CFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export FFLAGS="-cpp -O3 -fPIC"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export MPIFFLAGS="-cpp -O3 -fPIC"
 export MODPATH="-J"
 export I4R4=""
 export I4R8="-fdefault-real-8"
 export I8R8="-fdefault-integer-8 -fdefault-real-8"

 export CPPDEFS=""
 export CFLAGSDEFS=""
 export FFLAGSDEFS="-fconvert=big-endian"

 export USECC=""
 export USEFC="YES"
 export DEPS=""
