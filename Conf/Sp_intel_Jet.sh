# *** for Theia/Gaea/Jet (intel) ***

 export CC=icc
 export FC=ifort
 export CPP=cpp
 export OMPCC="$CC -qopenmp"
 export OMPFC="$FC -qopenmp"
 export MPICC=mpiicc
 export MPIFC=mpiifort

 export DEBUG="-g -traceback -O0"
 export CFLAGS="-g -O3 -traceback -xHOST -fPIC"
 export FFLAGS="-g -O3 -traceback -xHOST -auto -convert big_endian -assume byterecl -fp-model strict -fPIC"
 export FPPCPP="-cpp"
 export FREEFORM="-free"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-g -O3 -traceback -xHOST -fPIC"
 export MPIFFLAGS="-g -O3 -traceback -xHOST -auto -convert big_endian -assume byterecl -fp-model strict -fPIC"
 export MODPATH="-module "
 export I4R4="-integer-size 32 -real-size 32"
 export I4R8="-integer-size 32 -real-size 64"
 export I8R8="-integer-size 64 -real-size 64"

 export CPPDEFS=""
 export CFLAGSDEFS="-DUNDERSCORE -DLINUX"
 export FFLAGSDEFS="-DLINUX"

 export USECC=""
 export USEFC="YES"
 export DEPS=""
