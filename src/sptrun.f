C> @file
C>
C> truncate gridded scalar fields.
C> @author: IREDELL @date: 96-02-29
C>
C> This subprogram spectrally truncates scalar fields on a global
C> cylindrical grid, returning the fields to a possibly different
C> global cylindrical grid. The wave-space can be either triangular
C> or rhomboidal. either grid-space can be either an equally-spaced
C> grid (with or without pole points) or a gaussian grid. the grid
C> fields may have general indexing. the transforms are all
C> multiprocessed. Transform several fields at a time to improve
C> vectorization. Subprogram can be called from a multiprocessing
C> environment.
C>
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
C> @param IDRTO    - INTEGER OUTPUT GRID IDENTIFIER
C>                (IDRTO=4 FOR GAUSSIAN GRID,
C>                 IDRTO=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C>                 IDRTO=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C> @param IMAXO    - INTEGER EVEN NUMBER OF OUTPUT LONGITUDES.
C> @param JMAXO    - INTEGER NUMBER OF OUTPUT LATITUDES.
C> @param KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C> @param IPRIME   - INTEGER INPUT LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C>                (DEFAULTS TO 1 IF IPRIME=0)
C>                (OUTPUT LONGITUDE INDEX FOR PRIME MERIDIAN ASSUMED 1.)
C> @param ISKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LONGITUDES
C>                (DEFAULTS TO 1 IF ISKIPI=0)
C> @param JSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LATITUDES FROM SOUTH
C>                (DEFAULTS TO -IMAXI IF JSKIPI=0)
C> @param KSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS
C>                (DEFAULTS TO IMAXI*JMAXI IF KSKIPI=0)
C> @param ISKIPO   - INTEGER SKIP NUMBER BETWEEN OUTPUT LONGITUDES
C>                (DEFAULTS TO 1 IF ISKIPO=0)
C> @param JSKIPO   - INTEGER SKIP NUMBER BETWEEN OUTPUT LATITUDES FROM SOUTH
C>                (DEFAULTS TO -IMAXO IF JSKIPO=0)
C> @param KSKIPO   - INTEGER SKIP NUMBER BETWEEN OUTPUT GRID FIELDS
C>                (DEFAULTS TO IMAXO*JMAXO IF KSKIPO=0)
C> @param JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C>                (DEFAULTS TO ENVIRONMENT NCPUS IF JCPU=0)
C> @param GRIDI    - REAL (*) INPUT GRID FIELDS
C> @param GRIDO    - REAL (*) OUTPUT GRID FIELDS
C>                (MAY OVERLAY INPUT FIELDS IF GRID SHAPE IS APPROPRIATE)
C
C>    CALLED:
C>   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM
C>   NCPUS        GETS ENVIRONMENT NUMBER OF CPUS
C
C> REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C>   DIMENSION                    LINEAR              QUADRATIC
C>   -----------------------      ---------           -------------
C>   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C>   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C>   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C>   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C>   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C>   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C>   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
      SUBROUTINE SPTRUN(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,IDRTO,IMAXO,JMAXO,
     &                  KMAX,IPRIME,ISKIPI,JSKIPI,KSKIPI,
     &                  ISKIPO,JSKIPO,KSKIPO,JCPU,GRIDI,GRIDO)
      REAL GRIDI(*),GRIDO(*)
      REAL W((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,KMAX)
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
      CALL SPTRAN(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,
     &            IPRIME,ISKIPI,JN,JS,MDIM,KSKIPI,0,0,JC,
     &            W,GRIDI(INP),GRIDI(ISP),-1)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM WAVE TO OUTPUT
      JN=-JSKIPO
      IF(JN.EQ.0) JN=IMAXO
      JS=-JN
      INP=(JMAXO-1)*MAX(0,-JN)+1
      ISP=(JMAXO-1)*MAX(0,-JS)+1
      CALL SPTRAN(IROMB,MAXWV,IDRTO,IMAXO,JMAXO,KMAX,
     &            0,ISKIPO,JN,JS,MDIM,KSKIPO,0,0,JC,
     &            W,GRIDO(INP),GRIDO(ISP),1)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
