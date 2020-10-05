C> @file
C>
C> Perform a vector spherical transform
C> @author IREDELL @date 96-02-29

C> THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C> BETWEEN SPECTRAL COEFFICIENTS OF DIVERGENCES AND CURLS
C> AND VECTOR FIELDS ON A GLOBAL CYLINDRICAL GRID.
C> THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C> THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C> (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C> THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C> BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C> I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C> TRANSFORMS ARE DONE IN LATITUDE PAIRS FOR EFFICIENCY;
C> THUS GRID ARRAYS FOR EACH HEMISPHERE MUST BE PASSED.
C> IF SO REQUESTED, JUST A SUBSET OF THE LATITUDE PAIRS
C> MAY BE TRANSFORMED IN EACH INVOCATION OF THE SUBPROGRAM.
C> THE TRANSFORMS ARE ALL MULTIPROCESSED OVER LATITUDE EXCEPT
C> THE TRANSFORM FROM FOURIER TO SPECTRAL IS MULTIPROCESSED
C> OVER ZONAL WAVENUMBER TO ENSURE REPRODUCIBILITY.
C> TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C> SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C>
C> PROGRAM HISTORY LOG:
C> -  96-02-29  IREDELL
C> - 1998-12-15  IREDELL  GENERIC FFT USED, OPENMP DIRECTIVES INSERTED
C>
C> @param IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C>                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param MAXWV    - INTEGER SPECTRAL TRUNCATION
C> @param IDRT     - INTEGER GRID IDENTIFIER
C>                (IDRT=4 FOR GAUSSIAN GRID,
C>                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C>                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C> @param IMAX     - INTEGER EVEN NUMBER OF LONGITUDES.
C> @param JMAX     - INTEGER NUMBER OF LATITUDES.
C> @param KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C> @param IPRIME   - INTEGER LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C>                (DEFAULTS TO 1 IF IPRIME=0)
C> @param ISKIP    - INTEGER SKIP NUMBER BETWEEN LONGITUDES
C>                (DEFAULTS TO 1 IF ISKIP=0)
C> @param JNSKIP   - INTEGER SKIP NUMBER BETWEEN N.H. LATITUDES FROM NORTH
C>                (DEFAULTS TO IMAX IF JNSKIP=0)
C> @param JSSKIP   - INTEGER SKIP NUMBER BETWEEN S.H. LATITUDES FROM SOUTH
C>                (DEFAULTS TO -IMAX IF JSSKIP=0)
C> @param KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C>                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C> @param KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C>                (DEFAULTS TO IMAX*JMAX IF KGSKIP=0)
C> @param JBEG     - INTEGER LATITUDE INDEX (FROM POLE) TO BEGIN TRANSFORM
C>                (DEFAULTS TO 1 IF JBEG=0)
C>                (IF JBEG=0 AND IDIR<0, WAVE IS ZEROED BEFORE TRANSFORM)
C> @param JEND     - INTEGER LATITUDE INDEX (FROM POLE) TO END TRANSFORM
C>                (DEFAULTS TO (JMAX+1)/2 IF JEND=0)
C> @param JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C> @param[out] WAVED    - REAL (*) WAVE DIVERGENCE FIELDS IF IDIR>0
C>                [WAVED=(D(GRIDU)/DLAM+D(CLAT*GRIDV)/DPHI)/(CLAT*RERTH)]
C> @param[out] WAVEZ    - REAL (*) WAVE VORTICITY FIELDS IF IDIR>0
C>                [WAVEZ=(D(GRIDV)/DLAM-D(CLAT*GRIDU)/DPHI)/(CLAT*RERTH)]
C> @param[out] GRIDUN   - REAL (*) N.H. GRID U-WINDS (STARTING AT JBEG) IF IDIR<0
C> @param[out] GRIDUS   - REAL (*) S.H. GRID U-WINDS (STARTING AT JBEG) IF IDIR<0
C> @param[out] GRIDVN   - REAL (*) N.H. GRID V-WINDS (STARTING AT JBEG) IF IDIR<0
C> @param[out] GRIDVS   - REAL (*) S.H. GRID V-WINDS (STARTING AT JBEG) IF IDIR<0
C> @param IDIR     - INTEGER TRANSFORM FLAG
C>                (IDIR>0 FOR WAVE TO GRID, IDIR<0 FOR GRID TO WAVE)
C>
C> SUBPROGRAMS CALLED:
C>  - SPTRANFV     PERFORM A VECTOR SPHERICAL TRANSFORM
C>
C> REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C>   DIMENSION                    |LINEAR              |QUADRATIC
C>   -----------------------      |---------           |-------------
C>   IMAX                         |2*MAXWV+2           |3*MAXWV/2*2+2
C>   JMAX (IDRT=4,IROMB=0)        |1*MAXWV+1           |3*MAXWV/2+1
C>   JMAX (IDRT=4,IROMB=1)        |2*MAXWV+1           |5*MAXWV/2+1
C>   JMAX (IDRT=0,IROMB=0)        |2*MAXWV+3           |3*MAXWV/2*2+3
C>   JMAX (IDRT=0,IROMB=1)        |4*MAXWV+3           |5*MAXWV/2*2+3
C>   JMAX (IDRT=256,IROMB=0)      |2*MAXWV+1           |3*MAXWV/2*2+1
C>   JMAX (IDRT=256,IROMB=1)      |4*MAXWV+1           |5*MAXWV/2*2+1
      SUBROUTINE SPTRANV(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAX,
     &                   IPRIME,ISKIP,JNSKIP,JSSKIP,KWSKIP,KGSKIP,
     &                   JBEG,JEND,JCPU,
     &                   WAVED,WAVEZ,GRIDUN,GRIDUS,GRIDVN,GRIDVS,IDIR)

      REAL WAVED(*),WAVEZ(*),GRIDUN(*),GRIDUS(*),GRIDVN(*),GRIDVS(*)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      IP=IPRIME
      IS=ISKIP
      JN=JNSKIP
      JS=JSSKIP
      KW=KWSKIP
      KG=KGSKIP
      JB=JBEG
      JE=JEND
      JC=JCPU
      IF(IP.EQ.0) IP=1
      IF(IS.EQ.0) IS=1
      IF(JN.EQ.0) JN=IMAX
      IF(JS.EQ.0) JS=-JN
      IF(KW.EQ.0) KW=2*MX
      IF(KG.EQ.0) KG=IMAX*JMAX
      IF(JB.EQ.0) JB=1
      IF(JE.EQ.0) JE=(JMAX+1)/2
      IF(JC.EQ.0) JC=NCPUS()
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(IDIR.LT.0.AND.JBEG.EQ.0) THEN
        DO K=1,KMAX
          KWS=(K-1)*KW
          WAVED(KWS+1:KWS+2*MX)=0
          WAVEZ(KWS+1:KWS+2*MX)=0
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL SPTRANFV(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAX,
     &              IP,IS,JN,JS,KW,KG,JB,JE,JC,
     &              WAVED,WAVEZ,GRIDUN,GRIDUS,GRIDVN,GRIDVS,IDIR)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
