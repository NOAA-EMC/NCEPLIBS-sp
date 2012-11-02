LIB_i4r4 = libsp_4.a
LIB_i4r8 = libsp_d.a
LIB_i8r8 = libsp_8.a

###include ../../../configure.hwrf
FFLAGS1 = -DsysLinux -DLINUX -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32
FFLAGS2 = -DsysLinux -DLINUX -convert big_endian -assume byterecl -fp-model strict -i4 -r8 
FFLAGS3 = -DsysLinux -DLINUX -convert big_endian -assume byterecl -fp-model strict -i8 -r8
FC = ifort -i8 -r8 -convert big_endian -assume byterecl -fp-model strict
CP = /bin/cp
RM = /bin/rm
AR = /usr/bin/ar
.SUFFIXES: .o .f .F

OBJS = bll2ps.o spanaly.o spdz2uv.o speps.o spfft.o spfft1.o \
spfftpt.o spgradq.o spgradx.o spgrady.o splaplac.o \
splegend.o sppad.o spsynth.o sptez.o sptezd.o sptezm.o sptezmd.o sptezmv.o \
sptezv.o sptgpm.o sptgpmd.o sptgpmv.o sptgps.o sptgpsd.o sptgpsv.o sptgpt.o \
sptgptd.o sptgptsd.o sptgptv.o sptgptvd.o sptran.o sptrand.o sptranf.o sptranf0.o \
sptranf1.o sptranfv.o sptranv.o sptrun.o sptrund.o sptrung.o sptrungv.o sptrunl.o \
sptrunm.o sptrunmv.o sptruns.o sptrunsv.o sptrunv.o spuv2dz.o spvar.o spwget.o

OBJS1 = fftpack.o ncpus.o spffte.o splat.o dgef_dges.o

SRC_F77 = $(OBJS:.o=.f)

SRC_F = $(OBJS1:.o=.F)

ALL: $(LIB_i8r8)
.PHONY: ALL

$(LIB_i8r8): $(OBJS)
	$(FC) $(FFLAGS3) -c $(SRC_F) $(SRC_F77)
	$(AR) $(ARFLAGS) $(LIB_i8r8) $(OBJS1) $(OBJS)
	$(CP) $(LIB_i8r8) ../../lib_hwrf_util/
	$(RM) $(OBJS) $(OBJS1) 

clean:
	$(RM) $(OBJS) $(OBJS1) 

