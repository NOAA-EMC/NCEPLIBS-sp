# *** manually set environments (for intel compiler) of sp ***

# !!! module environment (*THEIA*) !!!
 module load intel/18.1.163
#module load ics/17.0.3

 ANCHORDIR=..
 export COMP=ips
 export SP_VER=v2.0.2
 export SP_SRC=
 export SP_LIB4=$ANCHORDIR/libsp_${SP_VER}_4.a
 export SP_LIB8=$ANCHORDIR/libsp_${SP_VER}_8.a
 export SP_LIBd=$ANCHORDIR/libsp_${SP_VER}_d.a

 export CC=icc
 export FC=ifort
 export CPP=cpp
 export OMPCC="$CC -qopenmp"
 export OMPFC="$FC -qopenmp"
 export MPICC=mpiicc
 export MPIFC=mpiifort

 export DEBUG="-g -O0"
 export CFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export FFLAGS="-fpp -O3 -xHOST -traceback -fPIC"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export MPIFFLAGS="-fpp -O3 -xHOST -traceback -fPIC"
 export MODPATH="-module "
 export I4R4="-integer-size 32 -real-size 32"
 export I4R8="-integer-size 32 -real-size 64"
 export I8R8="-integer-size 64 -real-size 64"

 export CPPDEFS=""
 export CFLAGSDEFS=""
 export FFLAGSDEFS="-auto -convert big_endian -assume byterecl -fp-model strict"

 export USECC=""
 export USEFC="YES"
 export DEPS=""
