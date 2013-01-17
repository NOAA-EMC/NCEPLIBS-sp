set -x
ifort -o exe-8 unittest01.f90 -convert big_endian -assume byterecl -fp-model strict -i8 -r8 /gpfs/es1/u/Eugene.Mirvis/lib_hwrf_util1/libsp_8.a -mkl
