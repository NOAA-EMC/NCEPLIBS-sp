ifort -o exe-4 gausslat_driver.f90 gausslat.f  -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32 /scratch/wx20em/NEMS/DISRTRIB/lib/libsp_4.a
ifort -o exe-8 gausslat_driver.f90 gausslat.f  -convert big_endian -assume byterecl -fp-model strict -i8 -r8 /scratch/wx20em/NEMS/DISRTRIB/lib/libsp_8.a
ifort -o exe-d gausslat_driver.f90 gausslat.f  -convert big_endian -assume byterecl -fp-model strict -i4 -r8 /scratch/wx20em/NEMS/DISRTRIB/lib/libsp_d.a
