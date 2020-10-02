C> @file
C>
C> Transform spectral scalar to mercator
C> @author IREDELL @date 96-02-29

C> This subprogram performs a spherical transform
C> from spectral coefficients of scalar quantities
C> to scalar fields on a mercator grid.
C> The wave-space can be either triangular or rhomboidal.
C> The wave and grid fields may have general indexing,
C> but each wave field is in sequential 'ibm order',
C> i.e. with zonal wavenumber as the slower index.
C> The mercator grid is identified by the location
C> of its first point and by its respective increments.
C> The transforms are all multiprocessed over sector points.
C> Transform several fields at a time to improve vectorization.
C> Subprogram can be called from a multiprocessing environment.
C>
C> PROGRAM HISTORY LOG:
C> -   96-02-29  IREDELL
C> - 1998-12-15  IREDELL  OPENMP DIRECTIVES INSERTED
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
C> @param GM       - REAL (*) MERCATOR FIELDS
C>
C> SUBPROGRAMS CALLED:
C>   - SPWGET()       GET WAVE-SPACE CONSTANTS
C>   - SPLEGEND()     COMPUTE LEGENDRE POLYNOMIALS
C>   - SPSYNTH()      SYNTHESIZE FOURIER FROM SPECTRAL
      SUBROUTINE SPTGPM(IROMB,MAXWV,KMAX,MI,MJ,
     &                  KWSKIP,KGSKIP,NISKIP,NJSKIP,
     &                  RLAT1,RLON1,DLAT,DLON,WAVE,GM)

      REAL WAVE(*),GM(*)
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      INTEGER MP(KMAX)
      REAL WTOP(2*(MAXWV+1),KMAX)
      REAL PLN((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),PLNTOP(MAXWV+1)
      REAL F(2*MAXWV+3,2,KMAX)
      REAL CLAT(MJ),SLAT(MJ),CLON(MAXWV,MI),SLON(MAXWV,MI)
      PARAMETER(RERTH=6.3712E6)
      PARAMETER(PI=3.14159265358979,DPR=180./PI)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE PRELIMINARY CONSTANTS
      CALL SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MXTOP=MAXWV+1
      IDIM=2*MAXWV+3
      KW=KWSKIP
      KG=KGSKIP
      NI=NISKIP
      NJ=NJSKIP
      IF(KW.EQ.0) KW=2*MX
      IF(KG.EQ.0) KG=MI*MJ
      IF(NI.EQ.0) NI=1
      IF(NJ.EQ.0) NJ=MI
      DO I=1,MI
        RLON=MOD(RLON1+DLON*(I-1)+3600,360.)
        DO L=1,MAXWV
          CLON(L,I)=COS(L*RLON/DPR)
          SLON(L,I)=SIN(L*RLON/DPR)
        ENDDO
      ENDDO
      YE=1-LOG(TAN((RLAT1+90)/2/DPR))*DPR/DLAT
      DO J=1,MJ
        RLAT=ATAN(EXP(DLAT/DPR*(J-YE)))*2*DPR-90
        CLAT(J)=COS(RLAT/DPR)
        SLAT(J)=SIN(RLAT/DPR)
      ENDDO
      MP=0
C$OMP PARALLEL DO
      DO K=1,KMAX
        WTOP(1:2*MXTOP,K)=0
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM TO GRID
C$OMP PARALLEL DO PRIVATE(PLN,PLNTOP,F,IJK)
      DO J=1,MJ
        CALL SPLEGEND(IROMB,MAXWV,SLAT(J),CLAT(J),EPS,EPSTOP,
     &                PLN,PLNTOP)
        CALL SPSYNTH(IROMB,MAXWV,2*MAXWV,IDIM,KW,2*MXTOP,KMAX,
     &               CLAT(J),PLN,PLNTOP,MP,WAVE,WTOP,F)
        DO K=1,KMAX
          DO I=1,MI
            IJK=(I-1)*NI+(J-1)*NJ+(K-1)*KG+1
            GM(IJK)=F(1,1,K)
          ENDDO
          DO L=1,MAXWV
            DO I=1,MI
              IJK=(I-1)*NI+(J-1)*NJ+(K-1)*KG+1
              GM(IJK)=GM(IJK)+2.*(F(2*L+1,1,K)*CLON(L,I)
     &                           -F(2*L+2,1,K)*SLON(L,I))
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
