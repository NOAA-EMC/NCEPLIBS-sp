C> @file
C>
C> Spectrally interpolate vectors to Mercator
C> @author IREDELL @date 96-02-29

C> THIS SUBPROGRAM SPECTRALLY TRUNCATES VECTOR FIELDS
C>           ON A GLOBAL CYLINDRICAL GRID, RETURNING THE FIELDS
C>           TO A MERCATOR GRID.
C>           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C>           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C>           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C>           THE GRID FIELDS MAY HAVE GENERAL INDEXING.
C>           THE TRANSFORMS ARE ALL MULTIPROCESSED.
C>           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C>           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C>
C> PROGRAM HISTORY LOG:
C>   96-02-29  IREDELL
C> 1998-12-15  IREDELL  OPENMP DIRECTIVES INSERTED
C>
C> @param IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C>                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param MAXWV    - INTEGER SPECTRAL TRUNCATION
C> @param IDRTI    - INTEGER INPUT GRID IDENTIFIER
C>                (IDRTI=4 FOR GAUSSIAN GRID,
C>                 IDRTI=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C>                 IDRTI=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C> @param IMAXI    - INTEGER EVEN NUMBER OF INPUT LONGITUDES.
C> @param JMAXI    - INTEGER NUMBER OF INPUT LATITUDES.
C> @param KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C> @param MI       - INTEGER NUMBER OF POINTS IN THE FASTER ZONAL DIRECTION
C> @param MJ       - INTEGER NUMBER OF POINTS IN THE SLOWER MERID DIRECTION
C> @param IPRIME   - INTEGER INPUT LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C>                (DEFAULTS TO 1 IF IPRIME=0)
C>                (OUTPUT LONGITUDE INDEX FOR PRIME MERIDIAN ASSUMED 1.)
C> @param ISKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LONGITUDES
C>                (DEFAULTS TO 1 IF ISKIPI=0)
C> @param JSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LATITUDES FROM SOUTH
C>                (DEFAULTS TO -IMAXI IF JSKIPI=0)
C> @param KSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS
C>                (DEFAULTS TO IMAXI*JMAXI IF KSKIPI=0)
C> @param KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C>                (DEFAULTS TO MI*MJ IF KGSKIP=0)
C> @param NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C>                (DEFAULTS TO 1 IF NISKIP=0)
C> @param NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C>                (DEFAULTS TO MI IF NJSKIP=0)
C> @param JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C>                (DEFAULTS TO ENVIRONMENT NCPUS IF JCPU=0)
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
C> @param GRIDUI   - REAL (*) INPUT GRID U-WINDS
C> @param GRIDVI   - REAL (*) INPUT GRID V-WINDS
C> @param LUV      - LOGICAL FLAG WHETHER TO RETURN WINDS
C> @param LDZ      - LOGICAL FLAG WHETHER TO RETURN DIVERGENCE AND VORTICITY
C> @param LPS      - LOGICAL FLAG WHETHER TO RETURN POTENTIAL AND STREAMFCN
C> @param UM       - REAL (*) MERCATOR U-WINDS IF LUV
C> @param VM       - REAL (*) MERCATOR V-WINDS IF LUV
C> @param DM       - REAL (*) MERCATOR DIVERGENCES IF LDZ
C> @param ZM       - REAL (*) MERCATOR VORTICITIES IF LDZ
C> @param PM       - REAL (*) MERCATOR POTENTIALS IF LPS
C> @param SM       - REAL (*) MERCATOR STREAMFCNS IF LPS
C>
C> SUBPROGRAMS CALLED:
C>  - SPWGET       GET WAVE-SPACE CONSTANTS
C>  - SPLAPLAC     COMPUTE LAPLACIAN IN SPECTRAL SPACE
C>  - SPTRANV      PERFORM A VECTOR SPHERICAL TRANSFORM
C>  - SPTGPM       TRANSFORM SPECTRAL SCALAR TO MERCATOR
C>  - SPTGPMV      TRANSFORM SPECTRAL VECTOR TO MERCATOR
C>  - NCPUS        GETS ENVIRONMENT NUMBER OF CPUS
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
      SUBROUTINE SPTRUNMV(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,MI,MJ,
     &                    IPRIME,ISKIPI,JSKIPI,KSKIPI,KGSKIP,
     &                    NISKIP,NJSKIP,JCPU,RLAT1,RLON1,DLAT,DLON,
     &                    GRIDUI,GRIDVI,LUV,UM,VM,LDZ,DM,ZM,LPS,PM,SM)

      LOGICAL LUV,LDZ,LPS
      REAL GRIDUI(*),GRIDVI(*)
      REAL UM(*),VM(*),DM(*),ZM(*),PM(*),SM(*)
      REAL W((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,KMAX)
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      REAL WD((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,KMAX)
      REAL WZ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,KMAX)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM INPUT GRID TO WAVE
      JC=JCPU
      IF(JC.EQ.0) JC=NCPUS()
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MDIM=2*MX+1
      JN=-JSKIPI
      IF(JN.EQ.0) JN=IMAXI
      JS=-JN
      INP=(JMAXI-1)*MAX(0,-JN)+1
      ISP=(JMAXI-1)*MAX(0,-JS)+1
      CALL SPTRANV(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,
     &             IPRIME,ISKIPI,JN,JS,MDIM,KSKIPI,0,0,JC,
     &             WD,WZ,
     &             GRIDUI(INP),GRIDUI(ISP),GRIDVI(INP),GRIDVI(ISP),-1)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM WAVE TO OUTPUT WINDS
      IF(LUV) THEN
        CALL SPTGPMV(IROMB,MAXWV,KMAX,MI,MJ,MDIM,KGSKIP,NISKIP,NJSKIP,
     &               RLAT1,RLON1,DLAT,DLON,WD,WZ,UM,VM)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM WAVE TO OUTPUT DIVERGENCE AND VORTICITY
      IF(LDZ) THEN
        CALL SPTGPM(IROMB,MAXWV,KMAX,MI,MJ,MDIM,KGSKIP,NISKIP,NJSKIP,
     &              RLAT1,RLON1,DLAT,DLON,WD,DM)
        CALL SPTGPM(IROMB,MAXWV,KMAX,MI,MJ,MDIM,KGSKIP,NISKIP,NJSKIP,
     &              RLAT1,RLON1,DLAT,DLON,WZ,ZM)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM WAVE TO OUTPUT POTENTIAL AND STREAMFUNCTION
      IF(LPS) THEN
        CALL SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
C$OMP PARALLEL DO
        DO K=1,KMAX
          CALL SPLAPLAC(IROMB,MAXWV,ENN1,WD(1,K),WD(1,K),-1)
          CALL SPLAPLAC(IROMB,MAXWV,ENN1,WZ(1,K),WZ(1,K),-1)
          WD(1:2,K)=0.
          WZ(1:2,K)=0.
        ENDDO
        CALL SPTGPM(IROMB,MAXWV,KMAX,MI,MJ,MDIM,KGSKIP,NISKIP,NJSKIP,
     &              RLAT1,RLON1,DLAT,DLON,WD,PM)
        CALL SPTGPM(IROMB,MAXWV,KMAX,MI,MJ,MDIM,KGSKIP,NISKIP,NJSKIP,
     &              RLAT1,RLON1,DLAT,DLON,WZ,SM)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
