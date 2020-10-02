# Documentation of the spectral transform library splib
May 2, 1996

## Introduction

The spectral transform library splib contains FORTRAN subprograms
to be used for a variety of spectral transform functions.
The library has been optimized for the CRAY machines, taking full advantage
of both the vector and parallel capabilities.  The library is particularly
efficient when transforming many fields at one time.  Some entry points
will diagnose the environmental number of CPUs available, but others require
the number of CPUs used be specified.  The library is reasonably transportable
to other platforms with compilers allowing dynamic automatic arrays.

The library can handle both scalar and two-dimensional vector fields.
Each vector field will be represented in spectral space appropriately
by its respective spherical divergence and curl (vorticity), thus
avoiding the pole problems associated with representing components separately.

Some of the functions performed by the library are spectral interpolations
between two grids, spectral truncations in place on a grid, and basic
spectral transforms between grid and wave space.  Only global Gaussian
or global equidistant cylindrical grids are allowed for transforming into
wave space.  There are no such restricitions on grids for transforming from
wave space.  However, there are special fast entry points for transforming wave
space to polar stereographic and Mercator grids as well as the aforementioned
cylindrical grids.

The indexing of the cylindrical transform grids is totally general.
The grids may run north to south or south to north; they may run east to west
or west to east; they may start at any longitude as long as the prime meridian
is on the grid; they may be dimensioned in any order (e.g. (i,j,k), (k,j,i),
(i,k,nfield,j), etc.).  Furthermore, the transform may be performed on only
some of the latitudes at one time as long as both hemisphere counterparts
are transformed at the same time (as in the global spectral model).
The grid indexing will default to the customary global indexing, i.e. north to
south, east to west, prime meridian as first longitude, and (i,j,k) order.

The wave space may be either triangular or rhomboidal in shape.
Its internal indexing is strictly "IBM order", i.e. zonal wavenumber is the
slower index with the real and imaginary components always paired together.
The imaginary components of all the zonally symmetric modes should always
be zero, as should the global mean of any divergence and vorticity fields.
The stride between the start of successive wave fields is general,
defaulting to the computed length of each field.

This documentation is divided into 4 chapters.  Chapter I is this introduction.
Chapter II is a list of all entry points.  Chapter III is a set of examples.
Chapter IV is a recapitulation of all the docblocks.

## Entry point list

   Name       Function
   ----       ------------------------------------------------------------------

              Spectral interpolations or truncations between grid and grid

<pre>
   SPTRUN     SPECTRALLY TRUNCATE GRIDDED SCALAR FIELDS
   SPTRUNV    SPECTRALLY TRUNCATE GRIDDED VECTOR FIELDS
   SPTRUNG    SPECTRALLY INTERPOLATE SCALARS TO STATIONS
   SPTRUNGV   SPECTRALLY INTERPOLATE VECTORS TO STATIONS
   SPTRUNS    SPECTRALLY INTERPOLATE SCALARS TO POLAR STEREO
   SPTRUNSV   SPECTRALLY INTERPOLATE VECTORS TO POLAR STEREO
   SPTRUNM    SPECTRALLY INTERPOLATE SCALARS TO MERCATOR
   SPTRUNMV   SPECTRALLY INTERPOLATE VECTORS TO MERCATOR
</pre>

              Spectral transforms between wave and grid

<pre>
   SPTRAN     PERFORM A SCALAR SPHERICAL TRANSFORM
   SPTRANV    PERFORM A VECTOR SPHERICAL TRANSFORM
   SPTRAND    PERFORM A GRADIENT SPHERICAL TRANSFORM
   SPTGPT     TRANSFORM SPECTRAL SCALAR TO STATION POINTS
   SPTGPTV    TRANSFORM SPECTRAL VECTOR TO STATION POINTS
   SPTGPTD    TRANSFORM SPECTRAL TO STATION POINT GRADIENTS
   SPTGPS     TRANSFORM SPECTRAL SCALAR TO POLAR STEREO
   SPTGPSV    TRANSFORM SPECTRAL VECTOR TO POLAR STEREO
   SPTGPSD    TRANSFORM SPECTRAL TO POLAR STEREO GRADIENTS
   SPTGPM     TRANSFORM SPECTRAL SCALAR TO MERCATOR
   SPTGPMV    TRANSFORM SPECTRAL VECTOR TO MERCATOR
   SPTGPMD    TRANSFORM SPECTRAL TO MERCATOR GRADIENTS
</pre>

              Spectral transform utilities

<pre>
   SPGGET     GET GRID-SPACE CONSTANTS
   SPWGET     GET WAVE-SPACE CONSTANTS
   SPLAT      COMPUTE LATITUDE FUNCTIONS
   SPEPS      COMPUTE UTILITY SPECTRAL FIELDS
   SPLEGEND   COMPUTE LEGENDRE POLYNOMIALS
   SPANALY    ANALYZE SPECTRAL FROM FOURIER
   SPSYNTH    SYNTHESIZE FOURIER FROM SPECTRAL
   SPDZ2UV    COMPUTE WINDS FROM DIVERGENCE AND VORTICITY
   SPUV2DZ    COMPUTE DIVERGENCE AND VORTICITY FROM WINDS
   SPGRADQ    COMPUTE GRADIENT IN SPECTRAL SPACE
   SPLAPLAC   COMPUTE LAPLACIAN IN SPECTRAL SPACE
</pre>

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

## Docblocks

The primary documentation of splib is via the docblocks in its subprograms.
The following recapitulation of docblocks is current as of May, 1996.


Docblock for sptrunv.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRUNV    SPECTRALLY TRUNCATE GRIDDED VECTOR FIELDS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM SPECTRALLY TRUNCATES VECTOR FIELDS
C           ON A GLOBAL CYLINDRICAL GRID, RETURNING THE FIELDS
C           TO A POSSIBLY DIFFERENT GLOBAL CYLINDRICAL GRID.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           EITHER GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE GRID FIELDS MAY HAVE GENERAL INDEXING.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED.
C           OVER ZONAL WAVENUMBER TO ENSURE REPRODUCIBILITY.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRUNV(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,
C    &                   IDRTO,IMAXO,JMAXO,KMAX,
C    &                   IPRIME,ISKIPI,JSKIPI,KSKIPI,
C    &                   ISKIPO,JSKIPO,KSKIPO,JCPU,GRIDUI,GRIDVI,
C    &                   LUV,GRIDUO,GRIDVO,LDZ,GRIDDO,GRIDZO,
C    &                   LPS,GRIDPO,GRIDSO)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRTI    - INTEGER INPUT GRID IDENTIFIER
C                (IDRTI=4 FOR GAUSSIAN GRID,
C                 IDRTI=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRTI=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAXI    - INTEGER EVEN NUMBER OF INPUT LONGITUDES.
C     JMAXI    - INTEGER NUMBER OF INPUT LATITUDES.
C     IDRTO    - INTEGER OUTPUT GRID IDENTIFIER
C                (IDRTO=4 FOR GAUSSIAN GRID,
C                 IDRTO=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRTO=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAXO    - INTEGER EVEN NUMBER OF OUTPUT LONGITUDES.
C     JMAXO    - INTEGER NUMBER OF OUTPUT LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     IPRIME   - INTEGER INPUT LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C                (OUTPUT LONGITUDE INDEX FOR PRIME MERIDIAN ASSUMED 1.)
C     ISKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LONGITUDES
C                (DEFAULTS TO 1 IF ISKIPI=0)
C     JSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAXI IF JSKIPI=0)
C     KSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS
C                (DEFAULTS TO IMAXI*JMAXI IF KSKIPI=0)
C     ISKIPO   - INTEGER SKIP NUMBER BETWEEN OUTPUT LONGITUDES
C                (DEFAULTS TO 1 IF ISKIPO=0)
C     JSKIPO   - INTEGER SKIP NUMBER BETWEEN OUTPUT LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAXO IF JSKIPO=0)
C     KSKIPO   - INTEGER SKIP NUMBER BETWEEN OUTPUT GRID FIELDS
C                (DEFAULTS TO IMAXO*JMAXO IF KSKIPO=0)
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C                (DEFAULTS TO ENVIRONMENT NCPUS IF JCPU=0)
C     GRIDUI   - REAL (*) INPUT GRID U-WINDS
C     GRIDVI   - REAL (*) INPUT GRID V-WINDS
C     LUV      - LOGICAL FLAG WHETHER TO RETURN WINDS
C     LDZ      - LOGICAL FLAG WHETHER TO RETURN DIVERGENCE AND VORTICITY
C     LPS      - LOGICAL FLAG WHETHER TO RETURN POTENTIAL AND STREAMFCN
C   OUTPUT ARGUMENTS:
C     GRIDUO   - REAL (*) OUTPUT U-WINDS IF LUV
C                (MAY OVERLAY INPUT FIELDS IF GRID SHAPE IS APPROPRIATE)
C     GRIDVO   - REAL (*) OUTPUT V-WINDS IF LUV
C                (MAY OVERLAY INPUT FIELDS IF GRID SHAPE IS APPROPRIATE)
C     GRIDDO   - REAL (*) OUTPUT DIVERGENCES IF LDZ
C                (MAY OVERLAY INPUT FIELDS IF GRID SHAPE IS APPROPRIATE)
C     GRIDZO   - REAL (*) OUTPUT VORTICITIES IF LDZ
C                (MAY OVERLAY INPUT FIELDS IF GRID SHAPE IS APPROPRIATE)
C     GRIDPO   - REAL (*) OUTPUT POTENTIALS IF LPS
C                (MAY OVERLAY INPUT FIELDS IF GRID SHAPE IS APPROPRIATE)
C     GRIDSO   - REAL (*) OUTPUT STREAMFCNS IF LPS
C                (MAY OVERLAY INPUT FIELDS IF GRID SHAPE IS APPROPRIATE)
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLAPLAC     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM
C   SPTRANV      PERFORM A VECTOR SPHERICAL TRANSFORM
C   NCPUS        GETS ENVIRONMENT NUMBER OF CPUS
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptrung.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRUNG    SPECTRALLY INTERPOLATE SCALARS TO STATIONS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM SPECTRALLY TRUNCATES SCALAR FIELDS
C           ON A GLOBAL CYLINDRICAL GRID, RETURNING THE FIELDS
C           TO SPECIFIED SETS OF STATION POINTS ON THE GLOBE.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE GRID AND POINT FIELDS MAY HAVE GENERAL INDEXING.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRUNG(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,NMAX,
C    &                   IPRIME,ISKIPI,JSKIPI,KSKIPI,KGSKIP,
C    &                   NRSKIP,NGSKIP,JCPU,RLAT,RLON,GRIDI,GP)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRTI    - INTEGER INPUT GRID IDENTIFIER
C                (IDRTI=4 FOR GAUSSIAN GRID,
C                 IDRTI=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRTI=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAXI    - INTEGER EVEN NUMBER OF INPUT LONGITUDES.
C     JMAXI    - INTEGER NUMBER OF INPUT LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NMAX     - INTEGER NUMBER OF STATION POINTS TO RETURN
C     IPRIME   - INTEGER INPUT LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C                (OUTPUT LONGITUDE INDEX FOR PRIME MERIDIAN ASSUMED 1.)
C     ISKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LONGITUDES
C                (DEFAULTS TO 1 IF ISKIPI=0)
C     JSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAXI IF JSKIPI=0)
C     KSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS
C                (DEFAULTS TO IMAXI*JMAXI IF KSKIPI=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINT SETS
C                (DEFAULTS TO NMAX IF KGSKIP=0)
C     NRSKIP   - INTEGER SKIP NUMBER BETWEEN STATION LATS AND LONS
C                (DEFAULTS TO 1 IF NRSKIP=0)
C     NGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINTS
C                (DEFAULTS TO 1 IF NGSKIP=0)
C     RLAT     - REAL (*) STATION LATITUDES IN DEGREES
C     RLON     - REAL (*) STATION LONGITUDES IN DEGREES
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C                (DEFAULTS TO ENVIRONMENT NCPUS IF JCPU=0)
C     GRIDI    - REAL (*) INPUT GRID FIELDS
C   OUTPUT ARGUMENTS:
C     GP       - REAL (*) STATION POINT SETS
C
C SUBPROGRAMS CALLED:
C   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM
C   SPTGPT       TRANSFORM SPECTRAL SCALAR TO STATION POINTS
C   NCPUS        GETS ENVIRONMENT NUMBER OF CPUS
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptrungv.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRUNGV   SPECTRALLY INTERPOLATE VECTORS TO STATIONS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM SPECTRALLY TRUNCATES VECTORS FIELDS
C           ON A GLOBAL CYLINDRICAL GRID, RETURNING THE FIELDS
C           TO SPECIFIED SETS OF STATION POINTS ON THE GLOBE.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE GRID AND POINT FIELDS MAY HAVE GENERAL INDEXING.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRUNGV(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,NMAX,
C    &                    IPRIME,ISKIPI,JSKIPI,KSKIPI,KGSKIP,
C    &                    NRSKIP,NGSKIP,JCPU,RLAT,RLON,GRIDUI,GRIDVI,
C    &                    LUV,UP,VP,LDZ,DP,ZP,LPS,PP,SP)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRTI    - INTEGER INPUT GRID IDENTIFIER
C                (IDRTI=4 FOR GAUSSIAN GRID,
C                 IDRTI=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRTI=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAXI    - INTEGER EVEN NUMBER OF INPUT LONGITUDES.
C     JMAXI    - INTEGER NUMBER OF INPUT LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NMAX     - INTEGER NUMBER OF STATION POINTS TO RETURN
C     IPRIME   - INTEGER INPUT LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C                (OUTPUT LONGITUDE INDEX FOR PRIME MERIDIAN ASSUMED 1.)
C     ISKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LONGITUDES
C                (DEFAULTS TO 1 IF ISKIPI=0)
C     JSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAXI IF JSKIPI=0)
C     KSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS
C                (DEFAULTS TO IMAXI*JMAXI IF KSKIPI=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINT SETS
C                (DEFAULTS TO NMAX IF KGSKIP=0)
C     NRSKIP   - INTEGER SKIP NUMBER BETWEEN STATION LATS AND LONS
C                (DEFAULTS TO 1 IF NRSKIP=0)
C     NGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINTS
C                (DEFAULTS TO 1 IF NGSKIP=0)
C     RLAT     - REAL (*) STATION LATITUDES IN DEGREES
C     RLON     - REAL (*) STATION LONGITUDES IN DEGREES
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C                (DEFAULTS TO ENVIRONMENT NCPUS IF JCPU=0)
C     GRIDUI   - REAL (*) INPUT GRID U-WINDS
C     GRIDVI   - REAL (*) INPUT GRID V-WINDS
C     LUV      - LOGICAL FLAG WHETHER TO RETURN WINDS
C     LDZ      - LOGICAL FLAG WHETHER TO RETURN DIVERGENCE AND VORTICITY
C     LPS      - LOGICAL FLAG WHETHER TO RETURN POTENTIAL AND STREAMFCN
C   OUTPUT ARGUMENTS:
C     UP       - REAL (*) STATION U-WINDS IF LUV
C     VP       - REAL (*) STATION V-WINDS IF LUV
C     DP       - REAL (*) STATION DIVERGENCES IF LDZ
C     ZP       - REAL (*) STATION VORTICITIES IF LDZ
C     PP       - REAL (*) STATION POTENTIALS IF LPS
C     SP       - REAL (*) STATION STREAMFCNS IF LPS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLAPLAC     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C   SPTRANV      PERFORM A VECTOR SPHERICAL TRANSFORM
C   SPTGPT       TRANSFORM SPECTRAL SCALAR TO STATION POINTS
C   SPTGPTV      TRANSFORM SPECTRAL VECTOR TO STATION POINTS
C   NCPUS        GETS ENVIRONMENT NUMBER OF CPUS
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptruns.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRUNS    SPECTRALLY INTERPOLATE SCALARS TO POLAR STEREO
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM SPECTRALLY TRUNCATES SCALAR FIELDS
C           ON A GLOBAL CYLINDRICAL GRID, RETURNING THE FIELDS
C           TO SPECIFIC PAIRS OF POLAR STEREOGRAPHIC SCALAR FIELDS.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE GRID FIELDS MAY HAVE GENERAL INDEXING.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRUNS(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,NPS,
C    &                   IPRIME,ISKIPI,JSKIPI,KSKIPI,KGSKIP,
C    &                   NISKIP,NJSKIP,JCPU,TRUE,XMESH,ORIENT,
C    &                   GRIDI,GN,GS)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRTI    - INTEGER INPUT GRID IDENTIFIER
C                (IDRTI=4 FOR GAUSSIAN GRID,
C                 IDRTI=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRTI=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAXI    - INTEGER EVEN NUMBER OF INPUT LONGITUDES.
C     JMAXI    - INTEGER NUMBER OF INPUT LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NPS      - INTEGER ODD ORDER OF THE POLAR STEREOGRAPHIC GRIDS
C     IPRIME   - INTEGER INPUT LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C                (OUTPUT LONGITUDE INDEX FOR PRIME MERIDIAN ASSUMED 1.)
C     ISKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LONGITUDES
C                (DEFAULTS TO 1 IF ISKIPI=0)
C     JSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAXI IF JSKIPI=0)
C     KSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS
C                (DEFAULTS TO IMAXI*JMAXI IF KSKIPI=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO NPS*NPS IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO NPS IF NJSKIP=0)
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C                (DEFAULTS TO ENVIRONMENT NCPUS IF JCPU=0)
C     TRUE     - REAL LATITUDE AT WHICH PS GRID IS TRUE (USUALLY 60.)
C     XMESH    - REAL GRID LENGTH AT TRUE LATITUDE (M)
C     ORIENT   - REAL LONGITUDE AT BOTTOM OF NORTHERN PS GRID
C                (SOUTHERN PS GRID WILL HAVE OPPOSITE ORIENTATION.)
C     GRIDI    - REAL (*) INPUT GRID FIELDS
C   OUTPUT ARGUMENTS:
C     GN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC FIELDS
C     GS       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC FIELDS
C
C SUBPROGRAMS CALLED:
C   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM
C   SPTGPS       TRANSFORM SPECTRAL SCALAR TO POLAR STEREO.
C   NCPUS        GETS ENVIRONMENT NUMBER OF CPUS
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptrunsv.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRUNSV   SPECTRALLY INTERPOLATE VECTORS TO POLAR STEREO
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM SPECTRALLY TRUNCATES VECTOR FIELDS
C           ON A GLOBAL CYLINDRICAL GRID, RETURNING THE FIELDS
C           TO SPECIFIC PAIRS OF POLAR STEREOGRAPHIC SCALAR FIELDS.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE GRID FIELDS MAY HAVE GENERAL INDEXING.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRUNSV(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,NPS,
C    &                    IPRIME,ISKIPI,JSKIPI,KSKIPI,KGSKIP,
C    &                    NISKIP,NJSKIP,JCPU,TRUE,XMESH,ORIENT,
C    &                    GRIDUI,GRIDVI,
C    &                    LUV,UN,VN,US,VS,LDZ,DN,ZN,DS,ZS,
C    &                    LPS,PN,SN,PS,SS)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRTI    - INTEGER INPUT GRID IDENTIFIER
C                (IDRTI=4 FOR GAUSSIAN GRID,
C                 IDRTI=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRTI=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAXI    - INTEGER EVEN NUMBER OF INPUT LONGITUDES.
C     JMAXI    - INTEGER NUMBER OF INPUT LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NPS      - INTEGER ODD ORDER OF THE POLAR STEREOGRAPHIC GRIDS
C     IPRIME   - INTEGER INPUT LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C                (OUTPUT LONGITUDE INDEX FOR PRIME MERIDIAN ASSUMED 1.)
C     ISKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LONGITUDES
C                (DEFAULTS TO 1 IF ISKIPI=0)
C     JSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAXI IF JSKIPI=0)
C     KSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS
C                (DEFAULTS TO IMAXI*JMAXI IF KSKIPI=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO NPS*NPS IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO NPS IF NJSKIP=0)
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C                (DEFAULTS TO ENVIRONMENT NCPUS IF JCPU=0)
C     TRUE     - REAL LATITUDE AT WHICH PS GRID IS TRUE (USUALLY 60.)
C     XMESH    - REAL GRID LENGTH AT TRUE LATITUDE (M)
C     ORIENT   - REAL LONGITUDE AT BOTTOM OF NORTHERN PS GRID
C                (SOUTHERN PS GRID WILL HAVE OPPOSITE ORIENTATION.)
C     GRIDUI   - REAL (*) INPUT GRID U-WINDS
C     GRIDVI   - REAL (*) INPUT GRID V-WINDS
C     LUV      - LOGICAL FLAG WHETHER TO RETURN WINDS
C     LDZ      - LOGICAL FLAG WHETHER TO RETURN DIVERGENCE AND VORTICITY
C     LPS      - LOGICAL FLAG WHETHER TO RETURN POTENTIAL AND STREAMFCN
C   OUTPUT ARGUMENTS:
C     UN       - REAL (*) NORTHERN PS U-WINDS IF LUV
C     VN       - REAL (*) NORTHERN PS V-WINDS IF LUV
C     US       - REAL (*) SOUTHERN PS U-WINDS IF LUV
C     VS       - REAL (*) SOUTHERN PS V-WINDS IF LUV
C     DN       - REAL (*) NORTHERN DIVERGENCES IF LDZ
C     ZN       - REAL (*) NORTHERN VORTICITIES IF LDZ
C     DS       - REAL (*) SOUTHERN DIVERGENCES IF LDZ
C     ZS       - REAL (*) SOUTHERN VORTICITIES IF LDZ
C     PN       - REAL (*) NORTHERN POTENTIALS IF LPS
C     SN       - REAL (*) NORTHERN STREAMFCNS IF LPS
C     PS       - REAL (*) SOUTHERN POTENTIALS IF LPS
C     SS       - REAL (*) SOUTHERN STREAMFCNS IF LPS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLAPLAC     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C   SPTRANV      PERFORM A VECTOR SPHERICAL TRANSFORM
C   SPTGPS       TRANSFORM SPECTRAL SCALAR TO POLAR STEREO.
C   SPTGPSV      TRANSFORM SPECTRAL VECTOR TO POLAR STEREO.
C   NCPUS        GETS ENVIRONMENT NUMBER OF CPUS
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptrunm.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRUNM    SPECTRALLY INTERPOLATE SCALARS TO MERCATOR
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM SPECTRALLY TRUNCATES SCALAR FIELDS
C           ON A GLOBAL CYLINDRICAL GRID, RETURNING THE FIELDS
C           TO A MERCATOR GRID.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE GRID FIELDS MAY HAVE GENERAL INDEXING.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRUNM(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,MI,MJ,
C    &                   IPRIME,ISKIPI,JSKIPI,KSKIPI,KGSKIP,
C    &                   NISKIP,NJSKIP,JCPU,RLAT1,RLON1,DLAT,DLON,
C    &                   GRIDI,GM)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRTI    - INTEGER INPUT GRID IDENTIFIER
C                (IDRTI=4 FOR GAUSSIAN GRID,
C                 IDRTI=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRTI=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAXI    - INTEGER EVEN NUMBER OF INPUT LONGITUDES.
C     JMAXI    - INTEGER NUMBER OF INPUT LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     MI       - INTEGER NUMBER OF POINTS IN THE FASTER ZONAL DIRECTION
C     MJ       - INTEGER NUMBER OF POINTS IN THE SLOWER MERID DIRECTION
C     IPRIME   - INTEGER INPUT LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C                (OUTPUT LONGITUDE INDEX FOR PRIME MERIDIAN ASSUMED 1.)
C     ISKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LONGITUDES
C                (DEFAULTS TO 1 IF ISKIPI=0)
C     JSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAXI IF JSKIPI=0)
C     KSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS
C                (DEFAULTS TO IMAXI*JMAXI IF KSKIPI=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO NPS*NPS IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO NPS IF NJSKIP=0)
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C                (DEFAULTS TO ENVIRONMENT NCPUS IF JCPU=0)
C     RLAT1    - REAL LATITUDE OF THE FIRST GRID POINT IN DEGREES
C     RLON1    - REAL LONGITUDE OF THE FIRST GRID POINT IN DEGREES
C     DLAT     - REAL LATITUDE INCREMENT IN DEGREES SUCH THAT
C                D(PHI)/D(J)=DLAT*COS(PHI) WHERE J IS MERIDIONAL INDEX.
C                DLAT IS NEGATIVE FOR GRIDS INDEXED SOUTHWARD.
C                (IN TERMS OF GRID INCREMENT DY VALID AT LATITUDE RLATI,
C                 THE LATITUDE INCREMENT DLAT IS DETERMINED AS
C                 DLAT=DPR*DY/(RERTH*COS(RLATI/DPR))
C                 WHERE DPR=180/PI AND RERTH IS EARTH'S RADIUS)
C     DLON     - REAL LONGITUDE INCREMENT IN DEGREES SUCH THAT
C                D(LAMBDA)/D(I)=DLON WHERE I IS ZONAL INDEX.
C                DLON IS NEGATIVE FOR GRIDS INDEXED WESTWARD.
C     GRIDI    - REAL (*) INPUT GRID FIELDS
C   OUTPUT ARGUMENTS:
C     GM       - REAL (*) MERCATOR FIELDS
C
C SUBPROGRAMS CALLED:
C   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM
C   SPTGPM       TRANSFORM SPECTRAL SCALAR TO MERCATOR
C   NCPUS        GETS ENVIRONMENT NUMBER OF CPUS
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptrunmv.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRUNMV   SPECTRALLY INTERPOLATE VECTORS TO MERCATOR
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM SPECTRALLY TRUNCATES VECTOR FIELDS
C           ON A GLOBAL CYLINDRICAL GRID, RETURNING THE FIELDS
C           TO A MERCATOR GRID.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE GRID FIELDS MAY HAVE GENERAL INDEXING.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRUNMV(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,MI,MJ,
C    &                    IPRIME,ISKIPI,JSKIPI,KSKIPI,KGSKIP,
C    &                    NISKIP,NJSKIP,JCPU,RLAT1,RLON1,DLAT,DLON,
C    &                    GRIDUI,GRIDVI,LUV,UM,VM,LDZ,DM,ZM,LPS,PM,SM)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRTI    - INTEGER INPUT GRID IDENTIFIER
C                (IDRTI=4 FOR GAUSSIAN GRID,
C                 IDRTI=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRTI=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAXI    - INTEGER EVEN NUMBER OF INPUT LONGITUDES.
C     JMAXI    - INTEGER NUMBER OF INPUT LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     MI       - INTEGER NUMBER OF POINTS IN THE FASTER ZONAL DIRECTION
C     MJ       - INTEGER NUMBER OF POINTS IN THE SLOWER MERID DIRECTION
C     IPRIME   - INTEGER INPUT LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C                (OUTPUT LONGITUDE INDEX FOR PRIME MERIDIAN ASSUMED 1.)
C     ISKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LONGITUDES
C                (DEFAULTS TO 1 IF ISKIPI=0)
C     JSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAXI IF JSKIPI=0)
C     KSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS
C                (DEFAULTS TO IMAXI*JMAXI IF KSKIPI=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO MI*MJ IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO MI IF NJSKIP=0)
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C                (DEFAULTS TO ENVIRONMENT NCPUS IF JCPU=0)
C     RLAT1    - REAL LATITUDE OF THE FIRST GRID POINT IN DEGREES
C     RLON1    - REAL LONGITUDE OF THE FIRST GRID POINT IN DEGREES
C     DLAT     - REAL LATITUDE INCREMENT IN DEGREES SUCH THAT
C                D(PHI)/D(J)=DLAT*COS(PHI) WHERE J IS MERIDIONAL INDEX.
C                DLAT IS NEGATIVE FOR GRIDS INDEXED SOUTHWARD.
C                (IN TERMS OF GRID INCREMENT DY VALID AT LATITUDE RLATI,
C                 THE LATITUDE INCREMENT DLAT IS DETERMINED AS
C                 DLAT=DPR*DY/(RERTH*COS(RLATI/DPR))
C                 WHERE DPR=180/PI AND RERTH IS EARTH'S RADIUS)
C     DLON     - REAL LONGITUDE INCREMENT IN DEGREES SUCH THAT
C                D(LAMBDA)/D(I)=DLON WHERE I IS ZONAL INDEX.
C                DLON IS NEGATIVE FOR GRIDS INDEXED WESTWARD.
C     GRIDUI   - REAL (*) INPUT GRID U-WINDS
C     GRIDVI   - REAL (*) INPUT GRID V-WINDS
C     LUV      - LOGICAL FLAG WHETHER TO RETURN WINDS
C     LDZ      - LOGICAL FLAG WHETHER TO RETURN DIVERGENCE AND VORTICITY
C     LPS      - LOGICAL FLAG WHETHER TO RETURN POTENTIAL AND STREAMFCN
C   OUTPUT ARGUMENTS:
C     UM       - REAL (*) MERCATOR U-WINDS IF LUV
C     VM       - REAL (*) MERCATOR V-WINDS IF LUV
C     DM       - REAL (*) MERCATOR DIVERGENCES IF LDZ
C     ZM       - REAL (*) MERCATOR VORTICITIES IF LDZ
C     PM       - REAL (*) MERCATOR POTENTIALS IF LPS
C     SM       - REAL (*) MERCATOR STREAMFCNS IF LPS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLAPLAC     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C   SPTRANV      PERFORM A VECTOR SPHERICAL TRANSFORM
C   SPTGPM       TRANSFORM SPECTRAL SCALAR TO MERCATOR
C   SPTGPMV      TRANSFORM SPECTRAL VECTOR TO MERCATOR
C   NCPUS        GETS ENVIRONMENT NUMBER OF CPUS
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptran.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRAN     PERFORM A SCALAR SPHERICAL TRANSFORM
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           BETWEEN SPECTRAL COEFFICIENTS OF SCALAR QUANTITIES
C           AND FIELDS ON A GLOBAL CYLINDRICAL GRID.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           TRANSFORMS ARE DONE IN LATITUDE PAIRS FOR EFFICIENCY;
C           THUS GRID ARRAYS FOR EACH HEMISPHERE MUST BE PASSED.
C           IF SO REQUESTED, JUST A SUBSET OF THE LATITUDE PAIRS
C           MAY BE TRANSFORMED IN EACH INVOCATION OF THE SUBPROGRAM.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER LATITUDE EXCEPT
C           THE TRANSFORM FROM FOURIER TO SPECTRAL IS MULTIPROCESSED
C           OVER ZONAL WAVENUMBER TO ENSURE REPRODUCIBILITY.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRAN(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAX,
C    &                  IPRIME,ISKIP,JNSKIP,JSSKIP,KWSKIP,KGSKIP,
C    &                  JBEG,JEND,JCPU,
C    &                  WAVE,GRIDN,GRIDS,IDIR)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRT     - INTEGER GRID IDENTIFIER
C                (IDRT=4 FOR GAUSSIAN GRID,
C                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAX     - INTEGER EVEN NUMBER OF LONGITUDES.
C     JMAX     - INTEGER NUMBER OF LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     IPRIME   - INTEGER LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C     ISKIP    - INTEGER SKIP NUMBER BETWEEN LONGITUDES
C                (DEFAULTS TO 1 IF ISKIP=0)
C     JNSKIP   - INTEGER SKIP NUMBER BETWEEN N.H. LATITUDES FROM NORTH
C                (DEFAULTS TO IMAX IF JNSKIP=0)
C     JSSKIP   - INTEGER SKIP NUMBER BETWEEN S.H. LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAX IF JSSKIP=0)
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO IMAX*JMAX IF KGSKIP=0)
C     JBEG     - INTEGER LATITUDE INDEX (FROM POLE) TO BEGIN TRANSFORM
C                (DEFAULTS TO 1 IF JBEG=0)
C                (IF JBEG=0 AND IDIR<0, WAVE IS ZEROED BEFORE TRANSFORM)
C     JEND     - INTEGER LATITUDE INDEX (FROM POLE) TO END TRANSFORM
C                (DEFAULTS TO (JMAX+1)/2 IF JEND=0)
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C     WAVE     - REAL (*) WAVE FIELDS IF IDIR>0
C     GRIDN    - REAL (*) N.H. GRID FIELDS (STARTING AT JBEG) IF IDIR<0
C     GRIDS    - REAL (*) S.H. GRID FIELDS (STARTING AT JBEG) IF IDIR<0
C     IDIR     - INTEGER TRANSFORM FLAG
C                (IDIR>0 FOR WAVE TO GRID, IDIR<0 FOR GRID TO WAVE)
C   OUTPUT ARGUMENTS:
C     WAVE     - REAL (*) WAVE FIELDS IF IDIR<0
C     GRIDN    - REAL (*) N.H. GRID FIELDS (STARTING AT JBEG) IF IDIR>0
C     GRIDS    - REAL (*) S.H. GRID FIELDS (STARTING AT JBEG) IF IDIR>0
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPGGET       GET GRID-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C   SPANALY      ANALYZE SPECTRAL FROM FOURIER
C   RFFTMLT      PERFORM FAST FOURIER TRANSFORM
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptranv.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRANV    PERFORM A VECTOR SPHERICAL TRANSFORM
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           BETWEEN SPECTRAL COEFFICIENTS OF DIVERGENCES AND CURLS
C           AND VECTOR FIELDS ON A GLOBAL CYLINDRICAL GRID.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           TRANSFORMS ARE DONE IN LATITUDE PAIRS FOR EFFICIENCY;
C           THUS GRID ARRAYS FOR EACH HEMISPHERE MUST BE PASSED.
C           IF SO REQUESTED, JUST A SUBSET OF THE LATITUDE PAIRS
C           MAY BE TRANSFORMED IN EACH INVOCATION OF THE SUBPROGRAM.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER LATITUDE EXCEPT
C           THE TRANSFORM FROM FOURIER TO SPECTRAL IS MULTIPROCESSED
C           OVER ZONAL WAVENUMBER TO ENSURE REPRODUCIBILITY.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRANV(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAX,
C    &                   IPRIME,ISKIP,JNSKIP,JSSKIP,KWSKIP,KGSKIP,
C    &                   JBEG,JEND,JCPU,
C    &                   WAVED,WAVEZ,GRIDUN,GRIDUS,GRIDVN,GRIDVS,IDIR)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRT     - INTEGER GRID IDENTIFIER
C                (IDRT=4 FOR GAUSSIAN GRID,
C                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAX     - INTEGER EVEN NUMBER OF LONGITUDES.
C     JMAX     - INTEGER NUMBER OF LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     IPRIME   - INTEGER LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C     ISKIP    - INTEGER SKIP NUMBER BETWEEN LONGITUDES
C                (DEFAULTS TO 1 IF ISKIP=0)
C     JNSKIP   - INTEGER SKIP NUMBER BETWEEN N.H. LATITUDES FROM NORTH
C                (DEFAULTS TO IMAX IF JNSKIP=0)
C     JSSKIP   - INTEGER SKIP NUMBER BETWEEN S.H. LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAX IF JSSKIP=0)
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO IMAX*JMAX IF KGSKIP=0)
C     JBEG     - INTEGER LATITUDE INDEX (FROM POLE) TO BEGIN TRANSFORM
C                (DEFAULTS TO 1 IF JBEG=0)
C                (IF JBEG=0 AND IDIR<0, WAVE IS ZEROED BEFORE TRANSFORM)
C     JEND     - INTEGER LATITUDE INDEX (FROM POLE) TO END TRANSFORM
C                (DEFAULTS TO (JMAX+1)/2 IF JEND=0)
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C     WAVED    - REAL (*) WAVE DIVERGENCE FIELDS IF IDIR>0
C     WAVEZ    - REAL (*) WAVE VORTICITY FIELDS IF IDIR>0
C     GRIDUN   - REAL (*) N.H. GRID U-WINDS (STARTING AT JBEG) IF IDIR<0
C     GRIDUS   - REAL (*) S.H. GRID U-WINDS (STARTING AT JBEG) IF IDIR<0
C     GRIDVN   - REAL (*) N.H. GRID V-WINDS (STARTING AT JBEG) IF IDIR<0
C     GRIDVS   - REAL (*) S.H. GRID V-WINDS (STARTING AT JBEG) IF IDIR<0
C     IDIR     - INTEGER TRANSFORM FLAG
C                (IDIR>0 FOR WAVE TO GRID, IDIR<0 FOR GRID TO WAVE)
C   OUTPUT ARGUMENTS:
C     WAVED    - REAL (*) WAVE DIVERGENCE FIELDS IF IDIR<0
C                [WAVED=(D(GRIDU)/DLAM+D(CLAT*GRIDV)/DPHI)/(CLAT*RERTH)]
C     WAVEZ    - REAL (*) WAVE VORTICITY FIELDS IF IDIR<0
C                [WAVEZ=(D(GRIDV)/DLAM-D(CLAT*GRIDU)/DPHI)/(CLAT*RERTH)]
C     GRIDUN   - REAL (*) N.H. GRID U-WINDS (STARTING AT JBEG) IF IDIR>0
C     GRIDUS   - REAL (*) S.H. GRID U-WINDS (STARTING AT JBEG) IF IDIR>0
C     GRIDVN   - REAL (*) N.H. GRID V-WINDS (STARTING AT JBEG) IF IDIR>0
C     GRIDVS   - REAL (*) S.H. GRID V-WINDS (STARTING AT JBEG) IF IDIR>0
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPGGET       GET GRID-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C   SPANALY      ANALYZE SPECTRAL FROM FOURIER
C   RFFTMLT      PERFORM FAST FOURIER TRANSFORM
C   SPDZ2UV      COMPUTE WINDS FROM DIVERGENCE AND VORTICITY
C   SPUV2DZ      COMPUTE DIVERGENCE AND VORTICITY FROM WINDS
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptrand.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRAND    PERFORM A GRADIENT SPHERICAL TRANSFORM
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           BETWEEN SPECTRAL COEFFICIENTS OF SCALAR FIELDS
C           AND THEIR MEANS AND GRADIENTS ON A GLOBAL CYLINDRICAL GRID.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           TRANSFORMS ARE DONE IN LATITUDE PAIRS FOR EFFICIENCY;
C           THUS GRID ARRAYS FOR EACH HEMISPHERE MUST BE PASSED.
C           IF SO REQUESTED, JUST A SUBSET OF THE LATITUDE PAIRS
C           MAY BE TRANSFORMED IN EACH INVOCATION OF THE SUBPROGRAM.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER LATITUDE EXCEPT
C           THE TRANSFORM FROM FOURIER TO SPECTRAL IS MULTIPROCESSED
C           OVER ZONAL WAVENUMBER TO ENSURE REPRODUCIBILITY.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRAND(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAX,
C    &                   IPRIME,ISKIP,JNSKIP,JSSKIP,KWSKIP,KGSKIP,
C    &                   JBEG,JEND,JCPU,
C    &                   WAVE,GRIDMN,GRIDXN,GRIDXS,GRIDYN,GRIDYS,IDIR)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRT     - INTEGER GRID IDENTIFIER
C                (IDRT=4 FOR GAUSSIAN GRID,
C                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAX     - INTEGER EVEN NUMBER OF LONGITUDES.
C     JMAX     - INTEGER NUMBER OF LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     IPRIME   - INTEGER LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C     ISKIP    - INTEGER SKIP NUMBER BETWEEN LONGITUDES
C                (DEFAULTS TO 1 IF ISKIP=0)
C     JNSKIP   - INTEGER SKIP NUMBER BETWEEN N.H. LATITUDES FROM NORTH
C                (DEFAULTS TO IMAX IF JNSKIP=0)
C     JSSKIP   - INTEGER SKIP NUMBER BETWEEN S.H. LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAX IF JSSKIP=0)
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO IMAX*JMAX IF KGSKIP=0)
C     JBEG     - INTEGER LATITUDE INDEX (FROM POLE) TO BEGIN TRANSFORM
C                (DEFAULTS TO 1 IF JBEG=0)
C                (IF JBEG=0 AND IDIR<0, WAVE IS ZEROED BEFORE TRANSFORM)
C     JEND     - INTEGER LATITUDE INDEX (FROM POLE) TO END TRANSFORM
C                (DEFAULTS TO (JMAX+1)/2 IF JEND=0)
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C     WAVE     - REAL (*) WAVE FIELDS IF IDIR>0
C     GRIDMN   - REAL (KMAX) GLOBAL MEANS IF IDIR<0
C     GRIDXN   - REAL (*) N.H. X-GRADIENTS (STARTING AT JBEG) IF IDIR<0
C     GRIDXS   - REAL (*) S.H. X-GRADIENTS (STARTING AT JBEG) IF IDIR<0
C     GRIDYN   - REAL (*) N.H. Y-GRADIENTS (STARTING AT JBEG) IF IDIR<0
C     GRIDYS   - REAL (*) S.H. Y-GRADIENTS (STARTING AT JBEG) IF IDIR<0
C     IDIR     - INTEGER TRANSFORM FLAG
C                (IDIR>0 FOR WAVE TO GRID, IDIR<0 FOR GRID TO WAVE)
C   OUTPUT ARGUMENTS:
C     WAVE     - REAL (*) WAVE FIELDS IF IDIR<0
C     GRIDMN   - REAL (KMAX) GLOBAL MEANS IF IDIR>0
C     GRIDXN   - REAL (*) N.H. X-GRADIENTS (STARTING AT JBEG) IF IDIR>0
C     GRIDXS   - REAL (*) S.H. X-GRADIENTS (STARTING AT JBEG) IF IDIR>0
C                [GRIDX=(D(WAVE)/DLAM)/(CLAT*RERTH)]
C     GRIDYN   - REAL (*) N.H. Y-GRADIENTS (STARTING AT JBEG) IF IDIR>0
C     GRIDYS   - REAL (*) S.H. Y-GRADIENTS (STARTING AT JBEG) IF IDIR>0
C                [GRIDY=(D(WAVE)/DPHI)/RERTH]
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLAPLAC     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C   SPTRANV      PERFORM A VECTOR SPHERICAL TRANSFORM
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptgpt.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPT     TRANSFORM SPECTRAL SCALAR TO STATION POINTS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF SCALAR QUANTITIES
C           TO SPECIFIED SETS OF STATION POINTS ON THE GLOBE.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND POINT FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER STATIONS.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTGPT(IROMB,MAXWV,KMAX,NMAX,
C    &                  KWSKIP,KGSKIP,NRSKIP,NGSKIP,
C    &                  RLAT,RLON,WAVE,GP)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NMAX     - INTEGER NUMBER OF STATION POINTS TO RETURN
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINT SETS
C                (DEFAULTS TO NMAX IF KGSKIP=0)
C     NRSKIP   - INTEGER SKIP NUMBER BETWEEN STATION LATS AND LONS
C                (DEFAULTS TO 1 IF NRSKIP=0)
C     NGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINTS
C                (DEFAULTS TO 1 IF NGSKIP=0)
C     RLAT     - REAL (*) STATION LATITUDES IN DEGREES
C     RLON     - REAL (*) STATION LONGITUDES IN DEGREES
C     WAVE     - REAL (*) WAVE FIELDS
C   OUTPUT ARGUMENTS:
C     GP       - REAL (*) STATION POINT SETS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptgptv.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPTV    TRANSFORM SPECTRAL VECTOR TO STATION POINTS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF DIVERGENCES AND CURLS
C           TO SPECIFIED SETS OF STATION POINT VECTORS ON THE GLOBE.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND POINT FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER STATIONS.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTGPTV(IROMB,MAXWV,KMAX,NMAX,
C    &                   KWSKIP,KGSKIP,NRSKIP,NGSKIP,
C    &                   RLAT,RLON,WAVED,WAVEZ,UP,VP)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NMAX     - INTEGER NUMBER OF STATION POINTS TO RETURN
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINT SETS
C                (DEFAULTS TO NMAX IF KGSKIP=0)
C     NRSKIP   - INTEGER SKIP NUMBER BETWEEN STATION LATS AND LONS
C                (DEFAULTS TO 1 IF NRSKIP=0)
C     NGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINTS
C                (DEFAULTS TO 1 IF NGSKIP=0)
C     RLAT     - REAL (*) STATION LATITUDES IN DEGREES
C     RLON     - REAL (*) STATION LONGITUDES IN DEGREES
C     WAVED    - REAL (*) WAVE DIVERGENCE FIELDS
C     WAVEZ    - REAL (*) WAVE VORTICITY FIELDS
C   OUTPUT ARGUMENTS:
C     UP       - REAL (*) STATION POINT U-WIND SETS
C     VP       - REAL (*) STATION POINT V-WIND SETS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C   SPDZ2UV      COMPUTE WINDS FROM DIVERGENCE AND VORTICITY
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptgptd.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPTD    TRANSFORM SPECTRAL TO STATION POINT GRADIENTS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF SCALAR FIELDS
C           TO SPECIFIED SETS OF STATION POINT GRADIENTS ON THE GLOBE.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND POINT FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER STATIONS.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTGPTD(IROMB,MAXWV,KMAX,NMAX,
C    &                   KWSKIP,KGSKIP,NRSKIP,NGSKIP,
C    &                   RLAT,RLON,WAVE,XP,YP)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NMAX     - INTEGER NUMBER OF STATION POINTS TO RETURN
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINT SETS
C                (DEFAULTS TO NMAX IF KGSKIP=0)
C     NRSKIP   - INTEGER SKIP NUMBER BETWEEN STATION LATS AND LONS
C                (DEFAULTS TO 1 IF NRSKIP=0)
C     NGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINTS
C                (DEFAULTS TO 1 IF NGSKIP=0)
C     RLAT     - REAL (*) STATION LATITUDES IN DEGREES
C     RLON     - REAL (*) STATION LONGITUDES IN DEGREES
C     WAVE     - REAL (*) WAVE FIELDS
C   OUTPUT ARGUMENTS:
C     XP       - REAL (*) STATION POINT X-GRADIENT SETS
C     YP       - REAL (*) STATION POINT Y-GRADIENT SETS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLAPLAC     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C   SPTGPTV      TRANSFORM SPECTRAL VECTOR TO STATION POINTS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptgps.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPS     TRANSFORM SPECTRAL SCALAR TO POLAR STEREO.
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF SCALAR QUANTITIES
C           TO SCALAR FIELDS ON A PAIR OF POLAR STEREOGRAPHIC GRIDS.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE TWO SQUARE POLAR STEREOGRAPHIC GRIDS ARE CENTERED
C           ON THE RESPECTIVE POLES, WITH THE ORIENTATION LONGITUDE
C           OF THE SOUTHERN HEMISPHERE GRID 180 DEGREES OPPOSITE
C           THAT OF THE NORTHERN HEMISPHERE GRID.
C
C           THE TRANSFORM IS MADE EFFICIENT             \ 4 | 5 /
C           BY COMBINING POINTS IN EIGHT SECTORS         \  |  /
C           OF EACH POLAR STEREOGRAPHIC GRID,           3 \ | / 6
C           NUMBERED AS IN THE DIAGRAM AT RIGHT.           \|/
C           THE POLE AND THE SECTOR BOUNDARIES          ----+----
C           ARE TREATED SPECIALLY IN THE CODE.             /|\
C           UNFORTUNATELY, THIS APPROACH INDUCES        2 / | \ 7
C           SOME HAIRY INDEXING AND CODE LOQUACITY,      /  |  \
C           FOR WHICH THE DEVELOPER APOLOGIZES.         / 1 | 8 \
C
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER SECTOR POINTS.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTGPS(IROMB,MAXWV,KMAX,NPS,
C    &                  KWSKIP,KGSKIP,NISKIP,NJSKIP,
C    &                  TRUE,XMESH,ORIENT,WAVE,GN,GS)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NPS      - INTEGER ODD ORDER OF THE POLAR STEREOGRAPHIC GRIDS
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO NPS*NPS IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO NPS IF NJSKIP=0)
C     TRUE     - REAL LATITUDE AT WHICH PS GRID IS TRUE (USUALLY 60.)
C     XMESH    - REAL GRID LENGTH AT TRUE LATITUDE (M)
C     ORIENT   - REAL LONGITUDE AT BOTTOM OF NORTHERN PS GRID
C                (SOUTHERN PS GRID WILL HAVE OPPOSITE ORIENTATION.)
C     WAVE     - REAL (*) WAVE FIELDS
C   OUTPUT ARGUMENTS:
C     GN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC FIELDS
C     GS       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC FIELDS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptgpsv.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPSV    TRANSFORM SPECTRAL VECTOR TO POLAR STEREO.
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF DIVERGENCES AND CURLS
C           TO VECTOR FIELDS ON A PAIR OF POLAR STEREOGRAPHIC GRIDS.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE TWO SQUARE POLAR STEREOGRAPHIC GRIDS ARE CENTERED
C           ON THE RESPECTIVE POLES, WITH THE ORIENTATION LONGITUDE
C           OF THE SOUTHERN HEMISPHERE GRID 180 DEGREES OPPOSITE
C           THAT OF THE NORTHERN HEMISPHERE GRID.
C           THE VECTORS ARE AUTOMATICALLY ROTATED TO BE RESOLVED
C           RELATIVE TO THE RESPECTIVE POLAR STEREOGRAPHIC GRIDS.
C
C           THE TRANSFORM IS MADE EFFICIENT             \ 4 | 5 /
C           BY COMBINING POINTS IN EIGHT SECTORS         \  |  /
C           OF EACH POLAR STEREOGRAPHIC GRID,           3 \ | / 6
C           NUMBERED AS IN THE DIAGRAM AT RIGHT.           \|/
C           THE POLE AND THE SECTOR BOUNDARIES          ----+----
C           ARE TREATED SPECIALLY IN THE CODE.             /|\
C           UNFORTUNATELY, THIS APPROACH INDUCES        2 / | \ 7
C           SOME HAIRY INDEXING AND CODE LOQUACITY,      /  |  \
C           FOR WHICH THE DEVELOPER APOLOGIZES.         / 1 | 8 \
C
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER SECTOR POINTS.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTGPSV(IROMB,MAXWV,KMAX,NPS,
C    &                   KWSKIP,KGSKIP,NISKIP,NJSKIP,
C    &                   TRUE,XMESH,ORIENT,WAVED,WAVEZ,UN,VN,US,VS)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NPS      - INTEGER ODD ORDER OF THE POLAR STEREOGRAPHIC GRIDS
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO NPS*NPS IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO NPS IF NJSKIP=0)
C     TRUE     - REAL LATITUDE AT WHICH PS GRID IS TRUE (USUALLY 60.)
C     XMESH    - REAL GRID LENGTH AT TRUE LATITUDE (M)
C     ORIENT   - REAL LONGITUDE AT BOTTOM OF NORTHERN PS GRID
C                (SOUTHERN PS GRID WILL HAVE OPPOSITE ORIENTATION.)
C     WAVED    - REAL (*) WAVE DIVERGENCE FIELDS
C     WAVEZ    - REAL (*) WAVE VORTICITY FIELDS
C   OUTPUT ARGUMENTS:
C     UN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC U-WINDS
C     VN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC V-WINDS
C     US       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC U-WINDS
C     VS       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC V-WINDS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C   SPDZ2UV      COMPUTE WINDS FROM DIVERGENCE AND VORTICITY
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptgpsd.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPSD    TRANSFORM SPECTRAL TO POLAR STEREO. GRADIENTS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF SCALAR FIELDS
C           TO GRADIENT FIELDS ON A PAIR OF POLAR STEREOGRAPHIC GRIDS.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE TWO SQUARE POLAR STEREOGRAPHIC GRIDS ARE CENTERED
C           ON THE RESPECTIVE POLES, WITH THE ORIENTATION LONGITUDE
C           OF THE SOUTHERN HEMISPHERE GRID 180 DEGREES OPPOSITE
C           THAT OF THE NORTHERN HEMISPHERE GRID.
C           THE VECTORS ARE AUTOMATICALLY ROTATED TO BE RESOLVED
C           RELATIVE TO THE RESPECTIVE POLAR STEREOGRAPHIC GRIDS.
C
C           THE TRANSFORM IS MADE EFFICIENT             \ 4 | 5 /
C           BY COMBINING POINTS IN EIGHT SECTORS         \  |  /
C           OF EACH POLAR STEREOGRAPHIC GRID,           3 \ | / 6
C           NUMBERED AS IN THE DIAGRAM AT RIGHT.           \|/
C           THE POLE AND THE SECTOR BOUNDARIES          ----+----
C           ARE TREATED SPECIALLY IN THE CODE.             /|\
C           UNFORTUNATELY, THIS APPROACH INDUCES        2 / | \ 7
C           SOME HAIRY INDEXING AND CODE LOQUACITY,      /  |  \
C           FOR WHICH THE DEVELOPER APOLOGIZES.         / 1 | 8 \
C
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER SECTOR POINTS.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTGPSD(IROMB,MAXWV,KMAX,NPS,
C    &                   KWSKIP,KGSKIP,NISKIP,NJSKIP,
C    &                   TRUE,XMESH,ORIENT,WAVE,XP,YP)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NPS      - INTEGER ODD ORDER OF THE POLAR STEREOGRAPHIC GRIDS
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO NPS*NPS IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO NPS IF NJSKIP=0)
C     TRUE     - REAL LATITUDE AT WHICH PS GRID IS TRUE (USUALLY 60.)
C     XMESH    - REAL GRID LENGTH AT TRUE LATITUDE (M)
C     ORIENT   - REAL LONGITUDE AT BOTTOM OF NORTHERN PS GRID
C                (SOUTHERN PS GRID WILL HAVE OPPOSITE ORIENTATION.)
C     WAVE     - REAL (*) WAVE FIELDS
C   OUTPUT ARGUMENTS:
C     XN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC X-GRADIENTS
C     YN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC Y-GRADIENTS
C     XS       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC X-GRADIENTS
C     YS       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC Y-GRADIENTS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLAPLAC     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C   SPTGPSV      TRANSFORM SPECTRAL VECTOR TO POLAR STEREO.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptgpm.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPM     TRANSFORM SPECTRAL SCALAR TO MERCATOR
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF SCALAR QUANTITIES
C           TO SCALAR FIELDS ON A MERCATOR GRID.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE MERCATOR GRID IS IDENTIFIED BY THE LOCATION
C           OF ITS FIRST POINT AND BY ITS RESPECTIVE INCREMENTS.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER SECTOR POINTS.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTGPM(IROMB,MAXWV,KMAX,MI,MJ,
C    &                  KWSKIP,KGSKIP,NISKIP,NJSKIP,
C    &                  RLAT1,RLON1,DLAT,DLON,WAVE,GM)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     MI       - INTEGER NUMBER OF POINTS IN THE FASTER ZONAL DIRECTION
C     MJ       - INTEGER NUMBER OF POINTS IN THE SLOWER MERID DIRECTION
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO MI*MJ IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO MI IF NJSKIP=0)
C     RLAT1    - REAL LATITUDE OF THE FIRST GRID POINT IN DEGREES
C     RLON1    - REAL LONGITUDE OF THE FIRST GRID POINT IN DEGREES
C     DLAT     - REAL LATITUDE INCREMENT IN DEGREES SUCH THAT
C                D(PHI)/D(J)=DLAT*COS(PHI) WHERE J IS MERIDIONAL INDEX.
C                DLAT IS NEGATIVE FOR GRIDS INDEXED SOUTHWARD.
C                (IN TERMS OF GRID INCREMENT DY VALID AT LATITUDE RLATI,
C                 THE LATITUDE INCREMENT DLAT IS DETERMINED AS
C                 DLAT=DPR*DY/(RERTH*COS(RLATI/DPR))
C                 WHERE DPR=180/PI AND RERTH IS EARTH'S RADIUS)
C     DLON     - REAL LONGITUDE INCREMENT IN DEGREES SUCH THAT
C                D(LAMBDA)/D(I)=DLON WHERE I IS ZONAL INDEX.
C                DLON IS NEGATIVE FOR GRIDS INDEXED WESTWARD.
C     WAVE     - REAL (*) WAVE FIELDS
C   OUTPUT ARGUMENTS:
C     GM       - REAL (*) MERCATOR FIELDS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptgpmv.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPMV    TRANSFORM SPECTRAL VECTOR TO MERCATOR
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF DIVERGENCES AND CURLS
C           TO VECTOR FIELDS ON A MERCATOR GRID.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE MERCATOR GRID IS IDENTIFIED BY THE LOCATION
C           OF ITS FIRST POINT AND BY ITS RESPECTIVE INCREMENTS.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER SECTOR POINTS.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTGPMV(IROMB,MAXWV,KMAX,MI,MJ,
C    &                   KWSKIP,KGSKIP,NISKIP,NJSKIP,
C    &                   RLAT1,RLON1,DLAT,DLON,WAVED,WAVEZ,UM,VM)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     MI       - INTEGER NUMBER OF POINTS IN THE FASTER ZONAL DIRECTION
C     MJ       - INTEGER NUMBER OF POINTS IN THE SLOWER MERID DIRECTION
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO MI*MJ IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO MI IF NJSKIP=0)
C     RLAT1    - REAL LATITUDE OF THE FIRST GRID POINT IN DEGREES
C     RLON1    - REAL LONGITUDE OF THE FIRST GRID POINT IN DEGREES
C     DLAT     - REAL LATITUDE INCREMENT IN DEGREES SUCH THAT
C                D(PHI)/D(J)=DLAT*COS(PHI) WHERE J IS MERIDIONAL INDEX.
C                DLAT IS NEGATIVE FOR GRIDS INDEXED SOUTHWARD.
C                (IN TERMS OF GRID INCREMENT DY VALID AT LATITUDE RLATI,
C                 THE LATITUDE INCREMENT DLAT IS DETERMINED AS
C                 DLAT=DPR*DY/(RERTH*COS(RLATI/DPR))
C                 WHERE DPR=180/PI AND RERTH IS EARTH'S RADIUS)
C     DLON     - REAL LONGITUDE INCREMENT IN DEGREES SUCH THAT
C                D(LAMBDA)/D(I)=DLON WHERE I IS ZONAL INDEX.
C                DLON IS NEGATIVE FOR GRIDS INDEXED WESTWARD.
C     WAVED    - REAL (*) WAVE DIVERGENCE FIELDS
C     WAVEZ    - REAL (*) WAVE VORTICITY FIELDS
C   OUTPUT ARGUMENTS:
C     UM       - REAL (*) MERCATOR U-WINDS
C     VM       - REAL (*) MERCATOR V-WINDS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C   SPDZ2UV      COMPUTE WINDS FROM DIVERGENCE AND VORTICITY
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for sptgpmd.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPMD    TRANSFORM SPECTRAL TO MERCATOR GRADIENTS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF SCALAR FIELDS
C           TO GRADIENT FIELDS ON A MERCATOR GRID.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE MERCATOR GRID IS IDENTIFIED BY THE LOCATION
C           OF ITS FIRST POINT AND BY ITS RESPECTIVE INCREMENTS.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER SECTOR POINTS.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTGPMD(IROMB,MAXWV,KMAX,MI,MJ,
C    &                   KWSKIP,KGSKIP,NISKIP,NJSKIP,
C    &                   RLAT1,RLON1,DLAT,DLON,WAVE,XM,YM)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     MI       - INTEGER NUMBER OF POINTS IN THE FASTER ZONAL DIRECTION
C     MJ       - INTEGER NUMBER OF POINTS IN THE SLOWER MERID DIRECTION
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO MI*MJ IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO MI IF NJSKIP=0)
C     RLAT1    - REAL LATITUDE OF THE FIRST GRID POINT IN DEGREES
C     RLON1    - REAL LONGITUDE OF THE FIRST GRID POINT IN DEGREES
C     DLAT     - REAL LATITUDE INCREMENT IN DEGREES SUCH THAT
C                D(PHI)/D(J)=DLAT*COS(PHI) WHERE J IS MERIDIONAL INDEX.
C                DLAT IS NEGATIVE FOR GRIDS INDEXED SOUTHWARD.
C                (IN TERMS OF GRID INCREMENT DY VALID AT LATITUDE RLATI,
C                 THE LATITUDE INCREMENT DLAT IS DETERMINED AS
C                 DLAT=DPR*DY/(RERTH*COS(RLATI/DPR))
C                 WHERE DPR=180/PI AND RERTH IS EARTH'S RADIUS)
C     DLON     - REAL LONGITUDE INCREMENT IN DEGREES SUCH THAT
C                D(LAMBDA)/D(I)=DLON WHERE I IS ZONAL INDEX.
C                DLON IS NEGATIVE FOR GRIDS INDEXED WESTWARD.
C     WAVE     - REAL (*) WAVE FIELDS
C   OUTPUT ARGUMENTS:
C     XM       - REAL (*) MERCATOR X-GRADIENTS
C     YM       - REAL (*) MERCATOR Y-GRADIENTS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLAPLAC     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C   SPTGPMV      TRANSFORM SPECTRAL VECTOR TO MERCATOR
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for spgget.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPGGET     GET GRID-SPACE CONSTANTS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM GETS GRID-SPACE CONSTANTS.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPGGET(IDRT,IMAX,JMAX,CLAT,SLAT,WLAT,TRIG,IFAX)
C   INPUT ARGUMENTS:
C     IDRT     - INTEGER GRID IDENTIFIER
C                (IDRT=4 FOR GAUSSIAN GRID,
C                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAX     - INTEGER NUMBER OF LONGITUDES.
C     JMAX     - INTEGER NUMBER OF LATITUDES.
C   OUTPUT ARGUMENTS:
C     CLAT     - REAL (JMAX) COSINES LATITUDE
C     SLAT     - REAL (JMAX) SINES LATITUDE
C     WLAT     - REAL (JMAX) GAUSSIAN WEIGHTS
C     TRIG     - REAL (2*IMAX) FFT TRIG VALUES
C     IFAX     - INTEGER (20) FFT FACTORS
C
C SUBPROGRAMS CALLED:
C   SPLAT        COMPUTE LATITUDE FUNCTIONS
C   FFTFAX       COMPUTE FFT CONSTANTS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$

Docblock for spwget.

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPWGET     GET WAVE-SPACE CONSTANTS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM GETS WAVE-SPACE CONSTANTS.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C   OUTPUT ARGUMENTS:
C     EPS      - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C     EPSTOP   - REAL (MAXWV+1)
C     ENN1     - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C     ELONN1   - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C     EON      - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C     EONTOP   - REAL (MAXWV+1)
C
C SUBPROGRAMS CALLED:
C   SPEPS        COMPUTE UTILITY SPECTRAL FIELDS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$

Docblock for splat.

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPLAT      COMPUTE LATITUDE FUNCTIONS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-20
C
C ABSTRACT: COMPUTES COSINES OF COLATITUDE AND GAUSSIAN WEIGHTS
C           FOR ONE OF THE FOLLOWING SPECIFIC GLOBAL SETS OF LATITUDES.
C             GAUSSIAN LATITUDES (IDRT=4)
C             EQUALLY-SPACED LATITUDES INCLUDING POLES (IDRT=0)
C             EQUALLY-SPACED LATITUDES EXCLUDING POLES (IDRT=256)
C           THE GAUSSIAN LATITUDES ARE LOCATED AT THE ZEROES OF THE
C           LEGENDRE POLYNOMIAL OF THE GIVEN ORDER.  THESE LATITUDES
C           ARE EFFICIENT FOR REVERSIBLE TRANSFORMS FROM SPECTRAL SPACE.
C           (ABOUT TWICE AS MANY EQUALLY-SPACED LATITUDES ARE NEEDED.)
C           THE WEIGHTS FOR THE EQUALLY-SPACED LATITUDES ARE BASED ON
C           ELLSAESSER (JAM,1966).  (NO WEIGHT IS GIVEN THE POLE POINT.)
C           NOTE THAT WHEN ANALYZING GRID TO SPECTRAL IN LATITUDE PAIRS,
C           IF AN EQUATOR POINT EXISTS, ITS WEIGHT SHOULD BE HALVED.
C
C PROGRAM HISTORY LOG:
C   96-02-20  IREDELL
C
C USAGE:    CALL SPLAT(IDRT,JMAX,SLAT,WLAT)
C
C   INPUT ARGUMENT LIST:
C     IDRT     - INTEGER GRID IDENTIFIER
C                (IDRT=4 FOR GAUSSIAN GRID,
C                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     JMAX     - INTEGER NUMBER OF LATITUDES.
C
C   OUTPUT ARGUMENT LIST:
C     SLAT     - REAL (JMAX) SINES OF LATITUDE.
C     WLAT     - REAL (JMAX) GAUSSIAN WEIGHTS.
C
C SUBPROGRAMS CALLED:
C   MINV         SOLVES FULL MATRIX PROBLEM
C
C REMARKS: FORTRAN 90 EXTENSIONS ARE USED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
</pre>

Docblock for speps.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPEPS       COMPUTE UTILITY SPECTRAL FIELDS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES CONSTANT FIELDS INDEXED IN THE SPECTRAL DOMAIN
C           IN "IBM ORDER" (ZONAL WAVENUMBER IS THE SLOWER INDEX).
C           IF L IS THE ZONAL WAVENUMBER AND N IS THE TOTAL WAVENUMBER
C           AND A IS THE EARTH RADIUS, THEN THE FIELDS RETURNED ARE:
C           (1) NORMALIZING FACTOR EPSILON=SQRT((N**2-L**2)/(4*N**2-1))
C           (2) LAPLACIAN FACTOR N*(N+1)/A**2
C           (3) ZONAL DERIVATIVE/LAPLACIAN FACTOR L/(N*(N+1))*A
C           (4) MERIDIONAL DERIVATIVE/LAPLACIAN FACTOR EPSILON/N*A
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPEPS(I,M,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C
C   OUTPUT ARGUMENT LIST:
C     EPS      - REAL ((M+1)*((I+1)*M+2)/2) SQRT((N**2-L**2)/(4*N**2-1))
C     EPSTOP   - REAL (M+1) SQRT((N**2-L**2)/(4*N**2-1)) OVER TOP
C     ENN1     - REAL ((M+1)*((I+1)*M+2)/2) N*(N+1)/A**2
C     ELONN1   - REAL ((M+1)*((I+1)*M+2)/2) L/(N*(N+1))*A
C     EON      - REAL ((M+1)*((I+1)*M+2)/2) EPSILON/N*A
C     EONTOP   - REAL (M+1) EPSILON/N*A OVER TOP
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
</pre>

Docblock for splegend.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPLEGEND    COMPUTE LEGENDRE POLYNOMIALS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: EVALUATES THE ORTHONORMAL ASSOCIATED LEGENDRE POLYNOMIALS
C           IN THE SPECTRAL DOMAIN AT A GIVEN LATITUDE.
C           SUBPROGRAM SPLEGEND SHOULD BE CALLED ALREADY.
C           IF L IS THE ZONAL WAVENUMBER, N IS THE TOTAL WAVENUMBER,
C           AND EPS(L,N)=SQRT((N**2-L**2)/(4*N**2-1)) THEN
C           THE FOLLOWING BOOTSTRAPPING FORMULAS ARE USED:
C           PLN(0,0)=SQRT(0.5)
C           PLN(L,L)=PLN(L-1,L-1)*CLAT*SQRT(FLOAT(2*L+1)/FLOAT(2*L))
C           PLN(L,N)=(SLAT*PLN(L,N-1)-EPS(L,N-1)*PLN(L,N-2))/EPS(L,N)
C           SYNTHESIS AT THE POLE NEEDS ONLY TWO ZONAL WAVENUMBERS.
C           SCALAR FIELDS ARE SYNTHESIZED WITH ZONAL WAVENUMBER 0 WHILE
C           VECTOR FIELDS ARE SYNTHESIZED WITH ZONAL WAVENUMBER 1.
C           (THUS POLAR VECTOR FIELDS ARE IMPLICITLY DIVIDED BY CLAT.)
C           THE FOLLOWING BOOTSTRAPPING FORMULAS ARE USED AT THE POLE:
C           PLN(0,0)=SQRT(0.5)
C           PLN(1,1)=SQRT(0.75)
C           PLN(L,N)=(PLN(L,N-1)-EPS(L,N-1)*PLN(L,N-2))/EPS(L,N)
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPLEGEND(I,M,SLAT,CLAT,EPS,EPSTOP,PLN,PLNTOP)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     SLAT     - REAL SINE OF LATITUDE
C     CLAT     - REAL COSINE OF LATITUDE
C     EPS      - REAL ((M+1)*((I+1)*M+2)/2) SQRT((N**2-L**2)/(4*N**2-1))
C     EPSTOP   - REAL (M+1) SQRT((N**2-L**2)/(4*N**2-1)) OVER TOP
C
C   OUTPUT ARGUMENT LIST:
C     PLN      - REAL ((M+1)*((I+1)*M+2)/2) LEGENDRE POLYNOMIAL
C     PLNTOP   - REAL (M+1) LEGENDRE POLYNOMIAL OVER TOP
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
</pre>

Docblock for spanaly.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPANALY     ANALYZE SPECTRAL FROM FOURIER
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: ANALYZES SPECTRAL COEFFICIENTS FROM FOURIER COEFFICIENTS
C           FOR A LATITUDE PAIR (NORTHERN AND SOUTHERN HEMISPHERES).
C           VECTOR COMPONENTS ARE MULTIPLIED BY COSINE OF LATITUDE.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C   94-08-01  MARK IREDELL   MOVED ZONAL WAVENUMBER LOOP INSIDE
C
C USAGE:    CALL SPANALY(I,M,IM,IX,NC,NCTOP,KM,WGT,CLAT,PLN,PLNTOP,MP,
C    &                   F,SPC,SPCTOP)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     IM       - INTEGER EVEN NUMBER OF FOURIER COEFFICIENTS
C     IX       - INTEGER DIMENSION OF FOURIER COEFFICIENTS (IX>=IM+2)
C     NC       - INTEGER DIMENSION OF SPECTRAL COEFFICIENTS
C                (NC>=(M+1)*((I+1)*M+2))
C     NCTOP    - INTEGER DIMENSION OF SPECTRAL COEFFICIENTS OVER TOP
C                (NCTOP>=2*(M+1))
C     KM       - INTEGER NUMBER OF FIELDS
C     WGT      - REAL GAUSSIAN WEIGHT
C     CLAT     - REAL COSINE OF LATITUDE
C     PLN      - REAL ((M+1)*((I+1)*M+2)/2) LEGENDRE POLYNOMIALS
C     PLNTOP   - REAL (M+1) LEGENDRE POLYNOMIAL OVER TOP
C     MP       - INTEGER (KM) IDENTIFIERS (0 FOR SCALAR, 1 FOR VECTOR)
C     F        - REAL (IX,2,KM) FOURIER COEFFICIENTS COMBINED
C     SPC      - REAL (NC,KM) SPECTRAL COEFFICIENTS
C     SPCTOP   - REAL (NCTOP,KM) SPECTRAL COEFFICIENTS OVER TOP
C
C   OUTPUT ARGUMENT LIST:
C     SPC      - REAL (NC,KM) SPECTRAL COEFFICIENTS
C     SPCTOP   - REAL (NCTOP,KM) SPECTRAL COEFFICIENTS OVER TOP
C
C SUBPROGRAMS CALLED:
C   SGERX1       CRAY LIBRARY MATRIX RANK 1 UPDATE
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
</pre>

Docblock for spsynth.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPSYNTH     SYNTHESIZE FOURIER FROM SPECTRAL
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: SYNTHESIZES FOURIER COEFFICIENTS FROM SPECTRAL COEFFICIENTS
C           FOR A LATITUDE PAIR (NORTHERN AND SOUTHERN HEMISPHERES).
C           VECTOR COMPONENTS ARE DIVIDED BY COSINE OF LATITUDE.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPSYNTH(I,M,IM,IX,NC,NCTOP,KM,CLAT,PLN,PLNTOP,MP,
C    &                   SPC,SPCTOP,F)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     IM       - INTEGER EVEN NUMBER OF FOURIER COEFFICIENTS
C     IX       - INTEGER DIMENSION OF FOURIER COEFFICIENTS (IX>=IM+2)
C     NC       - INTEGER DIMENSION OF SPECTRAL COEFFICIENTS
C                (NC>=(M+1)*((I+1)*M+2))
C     NCTOP    - INTEGER DIMENSION OF SPECTRAL COEFFICIENTS OVER TOP
C                (NCTOP>=2*(M+1))
C     KM       - INTEGER NUMBER OF FIELDS
C     CLAT     - REAL COSINE OF LATITUDE
C     PLN      - REAL ((M+1)*((I+1)*M+2)/2) LEGENDRE POLYNOMIAL
C     PLNTOP   - REAL (M+1) LEGENDRE POLYNOMIAL OVER TOP
C     SPC      - REAL (NC,KM) SPECTRAL COEFFICIENTS
C     SPCTOP   - REAL (NCTOP,KM) SPECTRAL COEFFICIENTS OVER TOP
C     MP       - INTEGER (KM) IDENTIFIERS (0 FOR SCALAR, 1 FOR VECTOR)
C
C   OUTPUT ARGUMENT LIST:
C     F        - REAL (IX,2,KM) FOURIER COEFFICIENTS FOR LATITUDE PAIR
C
C SUBPROGRAMS CALLED:
C   SGEMVX1      CRAY LIBRARY MATRIX TIMES VECTOR
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
</pre>

Docblock for spdz2uv.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPDZ2UV     COMPUTE WINDS FROM DIVERGENCE AND VORTICITY
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES THE WIND COMPONENTS FROM DIVERGENCE AND VORTICITY
C           IN SPECTRAL SPACE.
C           SUBPROGRAM SPEPS SHOULD BE CALLED ALREADY.
C           IF L IS THE ZONAL WAVENUMBER, N IS THE TOTAL WAVENUMBER,
C           EPS(L,N)=SQRT((N**2-L**2)/(4*N**2-1)) AND A IS EARTH RADIUS,
C           THEN THE ZONAL WIND COMPONENT U IS COMPUTED AS
C             U(L,N)=-I*L/(N*(N+1))*A*D(L,N)
C                    +EPS(L,N+1)/(N+1)*A*Z(L,N+1)-EPS(L,N)/N*A*Z(L,N-1)
C           AND THE MERIDIONAL WIND COMPONENT V IS COMPUTED AS
C             V(L,N)=-I*L/(N*(N+1))*A*Z(L,N)
C                    -EPS(L,N+1)/(N+1)*A*D(L,N+1)+EPS(L,N)/N*A*D(L,N-1)
C           WHERE D IS DIVERGENCE AND Z IS VORTICITY.
C           U AND V ARE WEIGHTED BY THE COSINE OF LATITUDE.
C           EXTRA TERMS ARE COMPUTED OVER TOP OF THE SPECTRAL DOMAIN.
C           ADVANTAGE IS TAKEN OF THE FACT THAT EPS(L,L)=0
C           IN ORDER TO VECTORIZE OVER THE ENTIRE SPECTRAL DOMAIN.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPDZ2UV(I,M,ENN1,ELONN1,EON,EONTOP,D,Z,U,V,UTOP,VTOP)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     ENN1     - REAL ((M+1)*((I+1)*M+2)/2) N*(N+1)/A**2
C     ELONN1   - REAL ((M+1)*((I+1)*M+2)/2) L/(N*(N+1))*A
C     EON      - REAL ((M+1)*((I+1)*M+2)/2) EPSILON/N*A
C     EONTOP   - REAL (M+1) EPSILON/N*A OVER TOP
C     D        - REAL ((M+1)*((I+1)*M+2)) DIVERGENCE
C     Z        - REAL ((M+1)*((I+1)*M+2)) VORTICITY
C
C   OUTPUT ARGUMENT LIST:
C     U        - REAL ((M+1)*((I+1)*M+2)) ZONAL WIND (TIMES COSLAT)
C     V        - REAL ((M+1)*((I+1)*M+2)) MERID WIND (TIMES COSLAT)
C     UTOP     - REAL (2*(M+1)) ZONAL WIND (TIMES COSLAT) OVER TOP
C     VTOP     - REAL (2*(M+1)) MERID WIND (TIMES COSLAT) OVER TOP
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
</pre>

Docblock for spuv2dz.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPUV2DZ     COMPUTE DIVERGENCE AND VORTICITY FROM WINDS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES THE DIVERGENCE AND VORTICITY FROM WIND COMPONENTS
C           IN SPECTRAL SPACE.
C           SUBPROGRAM SPEPS SHOULD BE CALLED ALREADY.
C           IF L IS THE ZONAL WAVENUMBER, N IS THE TOTAL WAVENUMBER,
C           EPS(L,N)=SQRT((N**2-L**2)/(4*N**2-1)) AND A IS EARTH RADIUS,
C           THEN THE DIVERGENCE D IS COMPUTED AS
C             D(L,N)=I*L*A*U(L,N)
C                    +EPS(L,N+1)*N*A*V(L,N+1)-EPS(L,N)*(N+1)*A*V(L,N-1)
C           AND THE VORTICITY Z IS COMPUTED AS
C             Z(L,N)=I*L*A*V(L,N)
C                    -EPS(L,N+1)*N*A*U(L,N+1)+EPS(L,N)*(N+1)*A*U(L,N-1)
C           WHERE U IS THE ZONAL WIND AND V IS THE MERIDIONAL WIND.
C           U AND V ARE WEIGHTED BY THE SECANT OF LATITUDE.
C           EXTRA TERMS ARE USED OVER TOP OF THE SPECTRAL DOMAIN.
C           ADVANTAGE IS TAKEN OF THE FACT THAT EPS(L,L)=0
C           IN ORDER TO VECTORIZE OVER THE ENTIRE SPECTRAL DOMAIN.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPUV2DZ(I,M,ENN1,ELONN1,EON,EONTOP,U,V,UTOP,VTOP,D,Z)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     ENN1     - REAL ((M+1)*((I+1)*M+2)/2) N*(N+1)/A**2
C     ELONN1   - REAL ((M+1)*((I+1)*M+2)/2) L/(N*(N+1))*A
C     EON      - REAL ((M+1)*((I+1)*M+2)/2) EPSILON/N*A
C     EONTOP   - REAL (M+1) EPSILON/N*A OVER TOP
C     U        - REAL ((M+1)*((I+1)*M+2)) ZONAL WIND (OVER COSLAT)
C     V        - REAL ((M+1)*((I+1)*M+2)) MERID WIND (OVER COSLAT)
C     UTOP     - REAL (2*(M+1)) ZONAL WIND (OVER COSLAT) OVER TOP
C     VTOP     - REAL (2*(M+1)) MERID WIND (OVER COSLAT) OVER TOP
C
C   OUTPUT ARGUMENT LIST:
C     D        - REAL ((M+1)*((I+1)*M+2)) DIVERGENCE
C     Z        - REAL ((M+1)*((I+1)*M+2)) VORTICITY
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
</pre>

Docblock for spgradq.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPGRADQ     COMPUTE GRADIENT IN SPECTRAL SPACE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES THE HORIZONTAL VECTOR GRADIENT OF A SCALAR FIELD
C           IN SPECTRAL SPACE.
C           SUBPROGRAM SPEPS SHOULD BE CALLED ALREADY.
C           IF L IS THE ZONAL WAVENUMBER, N IS THE TOTAL WAVENUMBER,
C           EPS(L,N)=SQRT((N**2-L**2)/(4*N**2-1)) AND A IS EARTH RADIUS,
C           THEN THE ZONAL GRADIENT OF Q(L,N) IS SIMPLY I*L/A*Q(L,N)
C           WHILE THE MERIDIONAL GRADIENT OF Q(L,N) IS COMPUTED AS
C           EPS(L,N+1)*(N+2)/A*Q(L,N+1)-EPS(L,N+1)*(N-1)/A*Q(L,N-1).
C           EXTRA TERMS ARE COMPUTED OVER TOP OF THE SPECTRAL DOMAIN.
C           ADVANTAGE IS TAKEN OF THE FACT THAT EPS(L,L)=0
C           IN ORDER TO VECTORIZE OVER THE ENTIRE SPECTRAL DOMAIN.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPGRADQ(I,M,ENN1,ELONN1,EON,EONTOP,Q,QDX,QDY,QDYTOP)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     ENN1     - REAL ((M+1)*((I+1)*M+2)/2) N*(N+1)/A**2
C     ELONN1   - REAL ((M+1)*((I+1)*M+2)/2) L/(N*(N+1))*A
C     EON      - REAL ((M+1)*((I+1)*M+2)/2) EPSILON/N*A
C     EONTOP   - REAL (M+1) EPSILON/N*A OVER TOP
C     Q        - REAL ((M+1)*((I+1)*M+2)) SCALAR FIELD
C
C   OUTPUT ARGUMENT LIST:
C     QDX      - REAL ((M+1)*((I+1)*M+2)) ZONAL GRADIENT (TIMES COSLAT)
C     QDY      - REAL ((M+1)*((I+1)*M+2)) MERID GRADIENT (TIMES COSLAT)
C     QDYTOP   - REAL (2*(M+1)) MERID GRADIENT (TIMES COSLAT) OVER TOP
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
</pre>

Docblock for splaplac.

<pre>
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPLAPLAC    COMPUTE LAPLACIAN IN SPECTRAL SPACE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES THE LAPLACIAN OR THE INVERSE LAPLACIAN
C           OF A SCALAR FIELD IN SPECTRAL SPACE.
C           SUBPROGRAM SPEPS SHOULD BE CALLED ALREADY.
C           THE LAPLACIAN OF Q(L,N) IS SIMPLY -N*(N+1)/A**2*Q(L,N)
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPLAPLAC(I,M,ENN1,Q,QD2,IDIR)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     ENN1     - REAL ((M+1)*((I+1)*M+2)/2) N*(N+1)/A**2
C     Q        - IF IDIR > 0, REAL ((M+1)*((I+1)*M+2)) SCALAR FIELD
C     QD2      - IF IDIR < 0, REAL ((M+1)*((I+1)*M+2)) LAPLACIAN
C     IDIR     - INTEGER FLAG
C                IDIR > 0 TO TAKE LAPLACIAN
C                IDIR < 0 TO TAKE INVERSE LAPLACIAN
C
C   OUTPUT ARGUMENT LIST:
C     Q        - IF IDIR < 0, REAL ((M+1)*((I+1)*M+2)) SCALAR FIELD
C                (Q(0,0) IS NOT COMPUTED)
C     QD2      - IF IDIR > 0, REAL ((M+1)*((I+1)*M+2)) LAPLACIAN
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
</pre>