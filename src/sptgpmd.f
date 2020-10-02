C> @file
C>
C> Transform spectral to mercator gradients
C> @author IREDELL @date 96-02-29
C>
C> This subprogram performs a spherical transform
C> from spectral coefficients of scalar fields
C> to gradient fields on a mercator grid.
C> The wave-space can be either triangular or rhomboidal.
C> The wave and grid fields may have general indexing,
C> but each wave field is in sequential 'ibm order',
C> i.e. with zonal wavenumber as the slower index.
C> The mercator grid is identified by the location
C> of its first point and by its respective increments.
C> The transforms are all multiprocessed over sector points.
C> Transform several fields at a time to improve vectorization.
C> subprogram can be called from a multiprocessing environment.
C>
C> @param IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C>                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param MAXWV    - INTEGER SPECTRAL TRUNCATION
C> @param KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C> @param MI       - INTEGER NUMBER OF POINTS IN THE FASTER ZONAL DIRECTION
C> @param MJ       - INTEGER NUMBER OF POINTS IN THE SLOWER MERID DIRECTION
C> @param KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C>                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C> @param KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C>                (DEFAULTS TO MI*MJ IF KGSKIP=0)
C> @param NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C>                (DEFAULTS TO 1 IF NISKIP=0)
C> @param NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C>                (DEFAULTS TO MI IF NJSKIP=0)
C> @param RLAT1    - REAL LATITUDE OF THE FIRST GRID POINT IN DEGREES
C> @param RLON1    - REAL LONGITUDE OF THE FIRST GRID POINT IN DEGREES
C> @param DLAT     - REAL LATITUDE INCREMENT IN DEGREES SUCH THAT
C>                D(PHI)/D(J)=DLAT*COS(PHI) WHERE J IS MERIDIONAL INDEX.
C>                DLAT IS NEGATIVE FOR GRIDS INDEXED SOUTHWARD.
C>                (IN TERMS OF GRID INCREMENT DY VALID AT LATITUDE RLATI,
C>                 THE LATITUDE INCREMENT DLAT IS DETERMINED AS
C>                 DLAT=DPR*DY/(RERTH*COS(RLATI/DPR))
C>                 WHERE DPR=180/PI AND RERTH IS EARTH'S RADIUS)
C> @param DLON     - REAL LONGITUDE INCREMENT IN DEGREES SUCH THAT
C>                D(LAMBDA)/D(I)=DLON WHERE I IS ZONAL INDEX.
C>                DLON IS NEGATIVE FOR GRIDS INDEXED WESTWARD.
C> @param WAVE     - REAL (*) WAVE FIELDS
C> @param XM       - REAL (*) MERCATOR X-GRADIENTS
C> @param YM       - REAL (*) MERCATOR Y-GRADIENTS
C>
C> SUBPROGRAMS CALLED:
C>   - SPWGET()       GET WAVE-SPACE CONSTANTS
C>   - SPLAPLAC()     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C>   - SPTGPMV()      TRANSFORM SPECTRAL VECTOR TO MERCATOR
      SUBROUTINE SPTGPMD(IROMB,MAXWV,KMAX,MI,MJ,
     &                   KWSKIP,KGSKIP,NISKIP,NJSKIP,
     &                   RLAT1,RLON1,DLAT,DLON,WAVE,XM,YM)

      REAL WAVE(*),XM(*),YM(*)
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
C  CALCULATE GRADIENTS
C$OMP PARALLEL DO PRIVATE(KWS)
      DO K=1,KMAX
        KWS=(K-1)*KW
        CALL SPLAPLAC(IROMB,MAXWV,ENN1,WAVE(KWS+1),WD(1,K),1)
        WZ(1:2*MX,K)=0.
      ENDDO
      CALL SPTGPMV(IROMB,MAXWV,KMAX,MI,MJ,MDIM,KGSKIP,NISKIP,NJSKIP,
     &             RLAT1,RLON1,DLAT,DLON,WD,WZ,XM,YM)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
