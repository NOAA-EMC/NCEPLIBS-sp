C> @file
C>
C> Transform spectral to station point gradients
C> @author IREDELL @date 96-02-29

C> This subprogram performs a spherical transform
c> from spectral coefficients of scalar fields
c> to specified sets of station point gradients on the globe.
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
C> @param XP       - REAL (*) STATION POINT X-GRADIENT SETS
C> @param YP       - REAL (*) STATION POINT Y-GRADIENT SETS
C>
C> SUBPROGRAMS CALLED:
C>   - SPWGET()       GET WAVE-SPACE CONSTANTS
C>   - SPLAPLAC()     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C>   - SPTGPTV()      TRANSFORM SPECTRAL VECTOR TO STATION POINTS
      SUBROUTINE SPTGPTD(IROMB,MAXWV,KMAX,NMAX,
     &                   KWSKIP,KGSKIP,NRSKIP,NGSKIP,
     &                   RLAT,RLON,WAVE,XP,YP)

      REAL RLAT(*),RLON(*),WAVE(*),XP(*),YP(*)
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      REAL WD((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,KMAX)
      REAL WZ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,KMAX)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE PRELIMINARY CONSTANTS
      CALL SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MDIM=2*MX+1
      KW=KWSKIP
      IF(KW.EQ.0) KW=2*MX
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE STATION FIELDS
C$OMP PARALLEL DO PRIVATE(KWS)
      DO K=1,KMAX
        KWS=(K-1)*KW
        CALL SPLAPLAC(IROMB,MAXWV,ENN1,WAVE(KWS+1),WD(1,K),1)
        WZ(1:2*MX,K)=0.
      ENDDO
      CALL SPTGPTV(IROMB,MAXWV,KMAX,NMAX,MDIM,KGSKIP,NRSKIP,NGSKIP,
     &             RLAT,RLON,WD,WZ,XP,YP)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
