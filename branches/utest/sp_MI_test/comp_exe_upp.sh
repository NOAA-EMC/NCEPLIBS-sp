set -x
ifort -o exe-4 unittest01.f90 -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32 /gpfs/es1/u/Eugene.Mirvis/lib_upp/libsp_4.a 
ifort -o exe-d unittest01.f90 -convert big_endian -assume byterecl -fp-model strict -i4 -r8 /gpfs/es1/u/Eugene.Mirvis/lib_upp/libsp_d.a
ifort -o exe-8 unittest01.f90 -convert big_endian -assume byterecl -fp-model strict -i8 -r8 /gpfs/es1/u/Eugene.Mirvis/lib_upp/libsp_8.a
