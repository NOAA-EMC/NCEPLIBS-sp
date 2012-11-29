set -x
ifort -o exe-4 unittest01.f90 -auto -openmp -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32 -fpp -DLINUX ../../splib_v2/libsp_4.a 
ifort -o exe-d unittest01.f90 -auto -openmp -convert big_endian -assume byterecl -fp-model strict -i4 -r8 -fpp -DLINUX ../../splib_v2/libsp_d.a
ifort -o exe-8 unittest01.f90 -auto -openmp -convert big_endian -assume byterecl -fp-model strict -fpp -DLINUX -i8 -r8 ../../splib_v2/libsp_8.a
#ifort -o exe-8 unittest01.f90 -auto -openmp -convert big_endian -assume byterecl -fp-model strict -fpp -DLINUX -i8 -r8 /gpfs/td1/meso/save/Eugene.Mirvis/SP_debug/WCOSS/splib_v2/libsp_8.a
echo "./exe-4__________________________________________________________________________________________"
./exe-4
echo "./exe-8__________________________________________________________________________________________"
./exe-8
echo "./exe-d___________________________________________________________________________________________"
./exe-d

