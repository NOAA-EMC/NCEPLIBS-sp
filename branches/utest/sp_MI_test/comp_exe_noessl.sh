set -x
ifort -o exe-4 spunit01.f90 -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32 /gpfs/es1/u/Eugene.Mirvis/lib_noessl/libsp_4.a
ifort -o exe-d spunit01.f90 -convert big_endian -assume byterecl -fp-model strict -i4 -r8 /gpfs/es1/u/Eugene.Mirvis/lib_noessl/libsp_d.a
ifort -o exe-8 spunit01.f90 -convert big_endian -assume byterecl -fp-model strict -i8 -r8 /gpfs/es1/u/Eugene.Mirvis/lib_noessl/libsp_8.a
