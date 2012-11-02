ifort -o exe-4 gausslat_driver.f90 gausslat.f -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32 /gpfs/es1/u/Eugene.Mirvis/lib_upp/libsp_4.a
ifort -o exe-8 gausslat_driver.f90 gausslat.f  -g -convert big_endian -assume byterecl -fp-model strict -i8 -r8 /gpfs/es1/u/Eugene.Mirvis/lib_upp/libsp_8.a
ifort  -o exe-d gausslat_driver.f90 gausslat.f  -convert big_endian -assume byterecl -fp-model strict -i4 -r8 /gpfs/es1/u/Eugene.Mirvis/lib_upp/libsp_d.a
