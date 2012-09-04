set -x
ifort -o exe-4 unittest01.f90 -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32 ../../lib/libsp_4.a 
ifort -o exe-d unittest01.f90 -convert big_endian -assume byterecl -fp-model strict -i4 -r8 ../../lib/libsp_d.a
ifort -o exe-8 unittest01.f90 -convert big_endian -assume byterecl -fp-model strict -i8 -r8 ../../lib/libsp_8.a
echo "./exe-4__________________________________________________________________________________________"
./exe-4
echo "./exe-8__________________________________________________________________________________________"
./exe-8
echo "./exe-d___________________________________________________________________________________________"
./exe-d

