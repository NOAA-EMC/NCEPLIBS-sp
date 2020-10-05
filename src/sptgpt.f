C> @file
C>
C> Transform spectral scalar to station points
C> @author IREDELL @date 96-02-29

C> This subprogram performs a spherical transform
c> from spectral coefficients of scalar quantities
c> to specified sets of station points on the globe.
C> The wave-space can be either triangular or rhomboidal.
C> The wave and point fields may have general indexing,
c> but each wave field is in sequential 'ibm order',
c> i.e. with zonal wavenumber as the slower index.
C> The transforms are all multiprocessed over stations.
C> Transform several fields at a time to improve vectorization.
C> Subprogram can be called from a multiprocessing environment.
C>
C> PROGRAM HISTORY LOG:
C> -  96-02-29  IREDELL
C> - 1998-12-15  IREDELL  OPENMP DIRECTIVES INSERTED
C> - 2003-06-30  IREDELL  USE SPFFTPT
C>
C> @param IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C>                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param MAXWV    - INTEGER SPECTRAL TRUNCATION
C> @param KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C> @param NMAX     - INTEGER NUMBER OF STATION POINTS TO RETURN
C> @param KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C>                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C> @param KGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINT SETS
C>                (DEFAULTS TO NMAX IF KGSKIP=0)
C> @param NRSKIP   - INTEGER SKIP NUMBER BETWEEN STATION LATS AND LONS
C>                (DEFAULTS TO 1 IF NRSKIP=0)
C> @param NGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINTS
C>                (DEFAULTS TO 1 IF NGSKIP=0)
C> @param RLAT     - REAL (*) STATION LATITUDES IN DEGREES
C> @param RLON     - REAL (*) STATION LONGITUDES IN DEGREES
C> @param WAVE     - REAL (*) WAVE FIELDS
C> @param GP       - REAL (*) STATION POINT SETS
C>
C> SUBPROGRAMS CALLED:
C>   - SPWGET()       GET WAVE-SPACE CONSTANTS
C>   - SPLEGEND()     COMPUTE LEGENDRE POLYNOMIALS
C>   - SPSYNTH()      SYNTHESIZE FOURIER FROM SPECTRAL
C>   - SPFFTPT()      POINTWISE FOURIER TRANSFORM
      SUBROUTINE SPTGPT(IROMB,MAXWV,KMAX,NMAX,
     &                  KWSKIP,KGSKIP,NRSKIP,NGSKIP,
     &                  RLAT,RLON,WAVE,GP)

      REAL RLAT(*),RLON(*),WAVE(*),GP(*)
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      INTEGER MP(KMAX)
      REAL WTOP(2*(MAXWV+1),KMAX)
      REAL PLN((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),PLNTOP(MAXWV+1)
      REAL F(2*MAXWV+3,2,KMAX)
      PARAMETER(PI=3.14159265358979)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE PRELIMINARY CONSTANTS
      CALL SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MXTOP=MAXWV+1
      IDIM=2*MAXWV+3
      KW=KWSKIP
      KG=KGSKIP
      NR=NRSKIP
      NG=NGSKIP
      IF(KW.EQ.0) KW=2*MX
      IF(KG.EQ.0) KG=NMAX
      IF(NR.EQ.0) NR=1
      IF(NG.EQ.0) NG=1
      MP=0
C$OMP PARALLEL DO
      DO K=1,KMAX
        WTOP(1:2*MXTOP,K)=0
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE STATION FIELDS
C$OMP PARALLEL DO PRIVATE(RADLAT,SLAT1,CLAT1)
C$OMP&            PRIVATE(PLN,PLNTOP,F,NK)
      DO N=1,NMAX
        RADLAT=PI/180*RLAT((N-1)*NR+1)
        IF(RLAT((N-1)*NR+1).GE.89.9995) THEN
          SLAT1=1.
          CLAT1=0.
        ELSEIF(RLAT((N-1)*NR+1).LE.-89.9995) THEN
          SLAT1=-1.
          CLAT1=0.
        ELSE
          SLAT1=SIN(RADLAT)
          CLAT1=COS(RADLAT)
        ENDIF
        CALL SPLEGEND(IROMB,MAXWV,SLAT1,CLAT1,EPS,EPSTOP,
     &                PLN,PLNTOP)
        CALL SPSYNTH(IROMB,MAXWV,2*MAXWV,IDIM,KW,2*MXTOP,KMAX,
     &               CLAT1,PLN,PLNTOP,MP,WAVE,WTOP,F)
        CALL SPFFTPT(MAXWV,1,2*MAXWV+3,KG,KMAX,RLON((N-1)*NR+1),
     &               F,GP((N-1)*NG+1))
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
