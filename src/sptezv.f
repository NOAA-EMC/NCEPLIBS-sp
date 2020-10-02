C> @file
C>
C> Perform a simple vector spherical transform
C> @author IREDELL @date 96-02-29
C>
C> This subprogram performs a spherical transform
C> between spectral coefficients of divergence and curl
C> and a vector field on a global cylindrical grid.
C> The wave-space can be either triangular or rhomboidal.
C> The grid-space can be either an equally-spaced grid
C> (with or without pole points) or a gaussian grid.
C> The wave field is in sequential 'IBM ORDER'.
C> The grid fiels is indexed east to west, then north to south.
C> For more flexibility and efficiency, call SPTRAN().
C> Subprogram can be called from a multiprocessing environment.
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
C> @param WAVED    - REAL (2*MX) WAVE DIVERGENCE FIELD IF IDIR>0
C>                WHERE MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
C> @param WAVEZ    - REAL (2*MX) WAVE VORTICITY FIELD IF IDIR>0
C>                WHERE MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
C> @param GRIDU    - REAL (IMAX,JMAX) GRID U-WIND (E->W,N->S) IF IDIR<0
C> @param GRIDV    - REAL (IMAX,JMAX) GRID V-WIND (E->W,N->S) IF IDIR<0
C> @param IDIR     - INTEGER TRANSFORM FLAG
C>                (IDIR>0 FOR WAVE TO GRID, IDIR<0 FOR GRID TO WAVE)
C> @param WAVED    - REAL (2*MX) WAVE DIVERGENCE FIELD IF IDIR<0
C>                WHERE MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
C> @param WAVEZ    - REAL (2*MX) WAVE VORTICITY FIELD IF IDIR>0
C>                WHERE MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
C> @param GRIDU    - REAL (IMAX,JMAX) GRID U-WIND (E->W,N->S) IF IDIR>0
C> @param GRIDV    - REAL (IMAX,JMAX) GRID V-WIND (E->W,N->S) IF IDIR>0
C>
C> SUBPROGRAMS CALLED:
C>   - SPTRANFV()     PERFORM A VECTOR SPHERICAL TRANSFORM
C>   - NCPUS()        GETS ENVIRONMENT NUMBER OF CPUS
C>
C> @note MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C>   DIMENSION                    |LINEAR              |QUADRATIC
C>   -----------------------      |---------           |-------------
C>   IMAX                         |2*MAXWV+2           |3*MAXWV/2*2+2
C>   JMAX (IDRT=4,IROMB=0)        |1*MAXWV+1           |3*MAXWV/2+1
C>   JMAX (IDRT=4,IROMB=1)        |2*MAXWV+1           |5*MAXWV/2+1
C>   JMAX (IDRT=0,IROMB=0)        |2*MAXWV+3           |3*MAXWV/2*2+3
C>   JMAX (IDRT=0,IROMB=1)        |4*MAXWV+3           |5*MAXWV/2*2+3
C>   JMAX (IDRT=256,IROMB=0)      |2*MAXWV+1           |3*MAXWV/2*2+1
C>   JMAX (IDRT=256,IROMB=1)      |4*MAXWV+1           |5*MAXWV/2*2+1
      SUBROUTINE SPTEZV(IROMB,MAXWV,IDRT,IMAX,JMAX,
     &                  WAVED,WAVEZ,GRIDU,GRIDV,IDIR)

      REAL WAVED((MAXWV+1)*((IROMB+1)*MAXWV+2))
      REAL WAVEZ((MAXWV+1)*((IROMB+1)*MAXWV+2))
      REAL GRIDU(IMAX,JMAX)
      REAL GRIDV(IMAX,JMAX)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      IP=1
      IS=1
      JN=IMAX
      JS=-JN
      KW=2*MX
      KG=IMAX*JMAX
      JB=1
      JE=(JMAX+1)/2
      JC=NCPUS()
      IF(IDIR.LT.0) WAVED=0
      IF(IDIR.LT.0) WAVEZ=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL SPTRANFV(IROMB,MAXWV,IDRT,IMAX,JMAX,1,
     &              IP,IS,JN,JS,KW,KG,JB,JE,JC,
     &              WAVED,WAVEZ,
     &              GRIDU,GRIDU(1,JMAX),GRIDV,GRIDV(1,JMAX),IDIR)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
