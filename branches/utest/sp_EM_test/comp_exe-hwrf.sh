ifort -o exe-4 gausslat_driver.f90 gausslat.f -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32 -mkl ../../../sp_mkl-/libsp_4.a
ifort -o exe-8 gausslat_driver.f90 gausslat.f -convert big_endian -assume byterecl -fp-model strict -i8 -r8 ../../../lib/sp_mkl-/libsp_8.a -mkl 
ifort  -o exe-d gausslat_driver.f90 gausslat.f  -convert big_endian -assume byterecl -fp-model strict -i4 -r8 ../../../lib/sp_mkl-/libsp_d.a -mkl
