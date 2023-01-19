@mainpage

## Documentation for Previous Versions of NCEPLIBS-sp

* [NCEPLIBS-sp Version 2.3.3](v2.3.3/index.html)

## Introduction

The spectral transform library splib, developed in the 1990s,
contains FORTRAN subprograms to be used for a variety of spectral
transform functions.

The library has been optimized for the CRAY machines, taking full
advantage of both the vector and parallel capabilities. The library is
particularly efficient when transforming many fields at one time. Some
entry points will diagnose the environmental number of CPUs available,
but others require the number of CPUs used be specified.

The library can handle both scalar and two-dimensional vector fields.
Each vector field will be represented in spectral space appropriately
by its respective spherical divergence and curl (vorticity), thus
avoiding the pole problems associated with representing components
separately.

Some of the functions performed by the library are spectral
interpolations between two grids, spectral truncations in place on a
grid, and basic spectral transforms between grid and wave space. Only
global Gaussian or global equidistant cylindrical grids are allowed
for transforming into wave space. There are no such restricitions on
grids for transforming from wave space. However, there are special
fast entry points for transforming wave space to polar stereographic
and Mercator grids as well as the aforementioned cylindrical grids.

The indexing of the cylindrical transform grids is totally general.
The grids may run north to south or south to north; they may run east
to west or west to east; they may start at any longitude as long as
the prime meridian is on the grid; they may be dimensioned in any
order (e.g. (i,j,k), (k,j,i), (i,k,nfield,j), etc.). Furthermore, the
transform may be performed on only some of the latitudes at one time
as long as both hemisphere counterparts are transformed at the same
time (as in the global spectral model). The grid indexing will
default to the customary global indexing, i.e. north to south, east to
west, prime meridian as first longitude, and (i,j,k) order.

The wave space may be either triangular or rhomboidal in shape. Its
internal indexing is strictly "IBM order", i.e. zonal wavenumber is
the slower index with the real and imaginary components always paired
together. The imaginary components of all the zonally symmetric modes
should always be zero, as should the global mean of any divergence and
vorticity fields. The stride between the start of successive wave
fields is general, defaulting to the computed length of each field.

This documentation is divided into 3 chapters. Chapter I is this
introduction.  Chapter II is a list of all entry points. Chapter III
is a set of examples.

## Subprograms

Spectral interpolations or truncations between grid and grid

   Name         |Function
   ----         |--------
   sptrun()     |SPECTRALLY TRUNCATE GRIDDED SCALAR FIELDS
   sptrunv()    |SPECTRALLY TRUNCATE GRIDDED VECTOR FIELDS
   sptrung()    |SPECTRALLY INTERPOLATE SCALARS TO STATIONS
   sptrungv()   |SPECTRALLY INTERPOLATE VECTORS TO STATIONS
   sptruns()    |SPECTRALLY INTERPOLATE SCALARS TO POLAR STEREO
   sptrunsv()   |SPECTRALLY INTERPOLATE VECTORS TO POLAR STEREO
   sptrunm()    |SPECTRALLY INTERPOLATE SCALARS TO MERCATOR
   sptrunmv()   |SPECTRALLY INTERPOLATE VECTORS TO MERCATOR

Spectral transforms between wave and grid

   Name         |Function
   ----         |------------------------------------------------------------------
   sptran()     |PERFORM A SCALAR SPHERICAL TRANSFORM
   sptranv()    |PERFORM A VECTOR SPHERICAL TRANSFORM
   sptrand()    |PERFORM A GRADIENT SPHERICAL TRANSFORM
   sptgpt()     |TRANSFORM SPECTRAL SCALAR TO STATION POINTS
   sptgptv()    |TRANSFORM SPECTRAL VECTOR TO STATION POINTS
   sptgptd()    |TRANSFORM SPECTRAL TO STATION POINT GRADIENTS
   sptgps()     |TRANSFORM SPECTRAL SCALAR TO POLAR STEREO
   sptgpsv()    |TRANSFORM SPECTRAL VECTOR TO POLAR STEREO
   sptgpsd()    |TRANSFORM SPECTRAL TO POLAR STEREO GRADIENTS
   sptgpm()     |TRANSFORM SPECTRAL SCALAR TO MERCATOR
   sptgpmv()    |TRANSFORM SPECTRAL VECTOR TO MERCATOR
   sptgpmd()    |TRANSFORM SPECTRAL TO MERCATOR GRADIENTS

Spectral transform utilities

   Name         |Function
   ----         |------------------------------------------------------------------
   spgget()     |GET GRID-SPACE CONSTANTS
   spwget()     |GET WAVE-SPACE CONSTANTS
   splat()      |COMPUTE LATITUDE FUNCTIONS
   speps()      |COMPUTE UTILITY SPECTRAL FIELDS
   splegend()   |COMPUTE LEGENDRE POLYNOMIALS
   spanaly()    |ANALYZE SPECTRAL FROM FOURIER
   spsynth()    |SYNTHESIZE FOURIER FROM SPECTRAL
   spdz2uv()    |COMPUTE WINDS FROM DIVERGENCE AND VORTICITY
   spuv2dz()    |COMPUTE DIVERGENCE AND VORTICITY FROM WINDS
   spgradq()    |COMPUTE GRADIENT IN SPECTRAL SPACE
   splaplac()   |COMPUTE LAPLACIAN IN SPECTRAL SPACE

## Examples

Example 1. Interpolate heights and winds from a latlon grid
           to two antipodal polar stereographic grids.
           Subprograms GETGB and PUTGB from w3lib are referenced.

<pre>
c  unit number 11 is the input latlon grib file
c  unit number 31 is the input latlon grib index file
c  unit number 51 is the output northern polar stereographic grib file
c  unit number 52 is the output southern polar stereographic grib file
c  nominal spectral truncation is r40
c  maximum input gridsize is 360x181
c  maximum number of levels wanted is 12
      parameter(lug=11,lui=31,lun=51,lus=52)
      parameter(iromb=1,maxwv=40,jf=360*181,kx=12)
      integer kp5(kx),kp6(kx),kp7(kx)
      integer kpo(kx)
      data kpo/1000,850,700,500,400,300,250,200,150,100,70,50/
c height
      km=12
      kp5=7
      kp6=100
      kp7=kpo
      call gs65(lug,lui,lun,lus,jf,km,kp5,kp6,kp7,iromb,maxwv)
c winds
      km=12
      kp5=33
      kp6=100
      kp7=kpo
      call gv65(lug,lui,lun,lus,jf,km,kp5,kp6,kp7,iromb,maxwv)
c
      stop
      end
c
      subroutine gs65(lug,lui,lun,lus,jf,km,kp5,kp6,kp7,iromb,maxwv)
c  interpolates a scalar field using spectral transforms.
      integer kp5(km),kp6(km),kp7(km)
c  output grids are 65x65 (381 km true at latitide 60).
c  nh grid oriented at 280E; sh grid oriented at 100E.
      parameter(nph=32,nps=2*nph+1,npq=nps*nps)
      parameter(true=60.,xmesh=381.e3,orient=280.)
      parameter(rerth=6.3712e6)
      parameter(pi=3.14159265358979,dpr=180./pi) 
      real gn(npq,km),gs(npq,km)
      integer jpds(25),jgds(22),kpds(25,km),kgds(22,km)
      logical lb(jf)
      real f(jf,km)
c
      g2=((1.+sin(abs(true)/dpr))*rerth/xmesh)**2
      r2=2*nph**2
      rlatn1=dpr*asin((g2-r2)/(g2+r2))
      rlonn1=mod(orient+315,360.)
      rlats1=-rlatn1
      rlons1=mod(rlonn1+270,360.)
      jpds=-1
      do k=1,km
        jpds(5)=kp5(k)
        jpds(6)=kp6(k)
        jpds(7)=kp7(k)
        j=0
        call getgb(lug,lui,jf,j,jpds,jgds,kf,j,kpds(1,k),kgds(1,k),
     &             lb,f(1,k),iret)
        if(iret.ne.0) call exit(1)
        if(mod(kpds(4,k)/64,2).eq.1) call exit(2)
      enddo
      idrt=kgds(1,1)
      imax=kgds(2,1)
      jmax=kgds(3,1)
c
      call sptruns(iromb,maxwv,idrt,imax,jmax,km,nps,
     &             0,0,0,jf,0,0,0,0,true,xmesh,orient,f,gn,gs)
c
      do k=1,km
        kpds(3,k)=27
        kgds(1,k)=5
        kgds(2,k)=nps
        kgds(3,k)=nps
        kgds(4,k)=nint(rlatn1*1.e3)
        kgds(5,k)=nint(rlonn1*1.e3)
        kgds(6,k)=8
        kgds(7,k)=nint(orient*1.e3)
        kgds(8,k)=nint(xmesh)
        kgds(9,k)=nint(xmesh)
        kgds(10,k)=0
        kgds(11,k)=64
        call putgb(lun,npq,kpds(1,k),kgds(1,k),lb,gn(1,k),iret)
      enddo
      do k=1,km
        kpds(3,k)=28
        kgds(1,k)=5
        kgds(2,k)=nps
        kgds(3,k)=nps
        kgds(4,k)=nint(rlats1*1.e3)
        kgds(5,k)=nint(rlons1*1.e3)
        kgds(6,k)=8
        kgds(7,k)=nint(mod(orient+180,360.)*1.e3)
        kgds(8,k)=nint(xmesh)
        kgds(9,k)=nint(xmesh)
        kgds(10,k)=128
        kgds(11,k)=64
        call putgb(lus,npq,kpds(1,k),kgds(1,k),lb,gs(1,k),iret)
      enddo
c
      end
c
      subroutine gv65(lug,lui,lun,lus,jf,km,kp5,kp6,kp7,iromb,maxwv)
c  interpolates a vector field using spectral transforms.
      integer kp5(km),kp6(km),kp7(km)
c  output grids are 65x65 (381 km true at latitide 60).
c  nh grid oriented at 280E; sh grid oriented at 100E.
c  winds are rotated to be relative to grid coordinates.
      parameter(nph=32,nps=2*nph+1,npq=nps*nps)
      parameter(true=60.,xmesh=381.e3,orient=280.)
      parameter(rerth=6.3712e6)
      parameter(pi=3.14159265358979,dpr=180./pi) 
      real un(npq,km),vn(npq,km),us(npq,km),vs(npq,km)
      integer jpds(25),jgds(22),kpds(25,km),kgds(22,km)
      logical lb(jf)
      real u(jf,km),v(jf,km)
c
      g2=((1.+sin(abs(true)/dpr))*rerth/xmesh)**2
      r2=2*nph**2
      rlatn1=dpr*asin((g2-r2)/(g2+r2))
      rlonn1=mod(orient+315,360.)
      rlats1=-rlatn1
      rlons1=mod(rlonn1+270,360.)
      jpds=-1
      do k=1,km
        jpds(5)=kp5(k)
        jpds(6)=kp6(k)
        jpds(7)=kp7(k)
        j=0
        call getgb(lug,lui,jf,j,jpds,jgds,kf,j,kpds(1,k),kgds(1,k),
     &             lb,u(1,k),iret)
        if(iret.ne.0) call exit(1)
        if(mod(kpds(4,k)/64,2).eq.1) call exit(2)
        jpds=kpds(:,k)
        jgds=kgds(:,k)
        jpds(5)=jpds(5)+1
        j=0
        call getgb(lug,lui,jf,j,jpds,jgds,kf,j,kpds(1,k),kgds(1,k),
     &             lb,v(1,k),iret)
        if(iret.ne.0) call exit(1)
        if(mod(kpds(4,k)/64,2).eq.1) call exit(2)
      enddo
      idrt=kgds(1,1)
      imax=kgds(2,1)
      jmax=kgds(3,1)
c
      call sptrunsv(iromb,maxwv,idrt,imax,jmax,km,nps,
     &              0,0,0,jf,0,0,0,0,true,xmesh,orient,u,v,
     &              .true.,un,vn,us,vs,.false.,dum,dum,dum,dum,
     &              .false.,dum,dum,dum,dum)
c
      do k=1,km
        kpds(3,k)=27
        kgds(1,k)=5
        kgds(2,k)=nps
        kgds(3,k)=nps
        kgds(4,k)=nint(rlatn1*1.e3)
        kgds(5,k)=nint(rlonn1*1.e3)
        kgds(6,k)=8
        kgds(7,k)=nint(orient*1.e3)
        kgds(8,k)=nint(xmesh)
        kgds(9,k)=nint(xmesh)
        kgds(10,k)=0
        kgds(11,k)=64
        kpds(5,k)=kp5(k)
        call putgb(lun,npq,kpds(1,k),kgds(1,k),lb,un(1,k),iret)
      enddo
      do k=1,km
        kpds(3,k)=27
        kgds(1,k)=5
        kgds(2,k)=nps
        kgds(3,k)=nps
        kgds(4,k)=nint(rlatn1*1.e3)
        kgds(5,k)=nint(rlonn1*1.e3)
        kgds(6,k)=8
        kgds(7,k)=nint(orient*1.e3)
        kgds(8,k)=nint(xmesh)
        kgds(9,k)=nint(xmesh)
        kgds(10,k)=0
        kgds(11,k)=64
        kpds(5,k)=kp5(k)+1
        call putgb(lun,npq,kpds(1,k),kgds(1,k),lb,vn(1,k),iret)
      enddo
      do k=1,km
        kpds(3,k)=28
        kgds(1,k)=5
        kgds(2,k)=nps
        kgds(3,k)=nps
        kgds(4,k)=nint(rlats1*1.e3)
        kgds(5,k)=nint(rlons1*1.e3)
        kgds(6,k)=8
        kgds(7,k)=nint(mod(orient+180,360.)*1.e3)
        kgds(8,k)=nint(xmesh)
        kgds(9,k)=nint(xmesh)
        kgds(10,k)=128
        kgds(11,k)=64
        kpds(5,k)=kp5(k)
        call putgb(lus,npq,kpds(1,k),kgds(1,k),lb,us(1,k),iret)
      enddo
      do k=1,km
        kpds(3,k)=28
        kgds(1,k)=5
        kgds(2,k)=nps
        kgds(3,k)=nps
        kgds(4,k)=nint(rlats1*1.e3)
        kgds(5,k)=nint(rlons1*1.e3)
        kgds(6,k)=8
        kgds(7,k)=nint(mod(orient+180,360.)*1.e3)
        kgds(8,k)=nint(xmesh)
        kgds(9,k)=nint(xmesh)
        kgds(10,k)=128
        kgds(11,k)=64
        kpds(5,k)=kp5(k)+1
        call putgb(lus,npq,kpds(1,k),kgds(1,k),lb,vs(1,k),iret)
      enddo
c
      end

Example 2. Spectrally truncate winds in place on a latlon grid.

c  unit number 11 is the input latlon grib file
c  unit number 31 is the input latlon grib index file
c  unit number 51 is the output latlon grib file
c  nominal spectral truncation is r40
c  maximum input gridsize is 360x181
c  maximum number of levels wanted is 12
      parameter(lug=11,lui=31,luo=51)
      parameter(iromb=1,maxwv=40,jf=360*181,kx=12)
      integer kp5(kx),kp6(kx),kp7(kx)
      integer kpo(kx)
      data kpo/1000,850,700,500,400,300,250,200,150,100,70,50/
c winds
      km=12
      kp5=33
      kp6=100
      kp7=kpo
      call gvr40(lug,lui,luo,jf,km,kp5,kp6,kp7,iromb,maxwv)
c
      stop
      end
c
      subroutine gvr40(lug,lui,luo,jf,km,kp5,kp6,kp7,iromb,maxwv)
c  interpolates a vector field using spectral transforms.
      integer kp5(km),kp6(km),kp7(km)
      integer jpds(25),jgds(22),kpds(25,km),kgds(22,km)
      logical lb(jf)
      real u(jf,km),v(jf,km)
c
      jpds=-1
      do k=1,km
        jpds(5)=kp5(k)
        jpds(6)=kp6(k)
        jpds(7)=kp7(k)
        j=0
        call getgb(lug,lui,jf,j,jpds,jgds,kf,j,kpds(1,k),kgds(1,k),
     &             lb,u(1,k),iret)
        if(iret.ne.0) call exit(1)
        if(mod(kpds(4,k)/64,2).eq.1) call exit(2)
        jpds=kpds(:,k)
        jgds=kgds(:,k)
        jpds(5)=jpds(5)+1
        j=0
        call getgb(lug,lui,jf,j,jpds,jgds,kf,j,kpds(1,k),kgds(1,k),
     &             lb,v(1,k),iret)
        if(iret.ne.0) call exit(1)
        if(mod(kpds(4,k)/64,2).eq.1) call exit(2)
      enddo
      idrt=kgds(1,1)
      imax=kgds(2,1)
      jmax=kgds(3,1)
c
      call sptrunv(iromb,maxwv,idrt,imax,jmax,idrt,imax,jmax,km,
     &             0,0,0,jf,0,0,jf,0,u,v,.true.,u,v,
     &             .false.,dum,dum,.false.,dum,dum)
c
      do k=1,km
        kpds(5,k)=kp5(k)
        call putgb(luo,kf,kpds(1,k),kgds(1,k),lb,u(1,k),iret)
      enddo
      do k=1,km
        kpds(5,k)=kp5(k)+1
        call putgb(luo,kf,kpds(1,k),kgds(1,k),lb,v(1,k),iret)
      enddo
c
      end

Example 3. Compute latlon temperatures from spectral temperatures and
           compute latlon winds from spectral divergence and vorticity.

c  unit number 11 is the input sigma file
c  unit number 51 is the output latlon file
c  nominal spectral truncation is t62
c  output gridsize is 144x73
c  number of levels is 28
      parameter(iromb=0,maxwv=62)
      parameter(idrt=0,im=144,jm=73)
      parameter(levs=28)
      parameter(mx=(maxwv+1)*((iromb+1)*maxwv+2)/2)
      real t(mx,levs),d(mx,levs),z(mx,levs)
      real tg(im,jm,km),ug(im,jm,km),vg(im,jm,km)
c  temperature
      do k=1,4
        read(11)
      enddo
      do k=1,levs
        read(11) (t(m,k),m=1,mx)
      enddo
      call sptran(iromb,maxwv,idrt,im,jm,levs,0,0,0,0,0,0,0,0,1,
     &            t,tg(1,1,1),tg(1,jm,1),1)
      call sptran(
      do k=1,levs
        write(51) ((tg(i,j,k),i=1,im),j=1,jm)
      enddo
c  winds
      do k=1,levs
        read(11) (d(m,k),m=1,mx)
        read(11) (z(m,k),m=1,mx)
      enddo
      call sptranv(iromb,maxwv,idrt,im,jm,levs,0,0,0,0,0,0,0,0,1,
     &             d,z,ug(1,1,1),ug(1,jm,1),vg(1,1,1),vg(1,jm,1),1)
      do k=1,levs
        write(51) ((ug(i,j,k),i=1,im),j=1,jm)
        write(51) ((vg(i,j,k),i=1,im),j=1,jm)
      enddo
      end
</pre>

