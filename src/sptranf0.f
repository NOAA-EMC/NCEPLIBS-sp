C> @file
C>
C> Sptranf spectral initialization
C> @author IREDELL @date 96-02-29

C> This subprogram performs an initialization for
C> subprogram sptranf(). Use this subprogram outside
C> the sptranf family context at your own risk.
C>
C> @param IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C>                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param MAXWV    - INTEGER SPECTRAL TRUNCATION
C> @param IDRT     - INTEGER GRID IDENTIFIER
C>                (IDRT=4 FOR GAUSSIAN GRID,
C>                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C>                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C> @param IMAX     - INTEGER EVEN NUMBER OF LONGITUDES
C> @param JMAX     - INTEGER NUMBER OF LATITUDES
C> @param JB       - INTEGER LATITUDE INDEX (FROM POLE) TO BEGIN TRANSFORM
C> @param JE       - INTEGER LATITUDE INDEX (FROM POLE) TO END TRANSFORM
C> @param EPS      - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C> @param EPSTOP   - REAL (MAXWV+1)
C> @param ENN1     - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C> @param ELONN1   - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C> @param EON      - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C> @param EONTOP   - REAL (MAXWV+1)
C> @param AFFT     - REAL(8) (50000+4*IMAX) AUXILIARY ARRAY IF IDIR=0
C> @param CLAT     - REAL (JB:JE) COSINES OF LATITUDE
C> @param SLAT     - REAL (JB:JE) SINES OF LATITUDE
C> @param WLAT     - REAL (JB:JE) GAUSSIAN WEIGHTS
C> @param PLN      - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2,JB:JE)
C>                LEGENDRE POLYNOMIALS
C> @param PLNTOP   - REAL (MAXWV+1,JB:JE) LEGENDRE POLYNOMIAL OVER TOP
C>
C> SUBPROGRAMS CALLED:
C>  - spwget()       GET WAVE-SPACE CONSTANTS
C>  - spffte()       PERFORM FAST FOURIER TRANSFORM
C>  - splat()        COMPUTE LATITUDE FUNCTIONS
C>  - splegend()     COMPUTE LEGENDRE POLYNOMIALS
C>
      SUBROUTINE SPTRANF0(IROMB,MAXWV,IDRT,IMAX,JMAX,JB,JE,
     &                    EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP,
     &                    AFFT,CLAT,SLAT,WLAT,PLN,PLNTOP)

      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      REAL(8) AFFT(50000+4*IMAX)
      REAL CLAT(JB:JE),SLAT(JB:JE),WLAT(JB:JE)
      REAL PLN((MAXWV+1)*((IROMB+1)*MAXWV+2)/2,JB:JE)
      REAL PLNTOP(MAXWV+1,JB:JE)
      REAL SLATX(JMAX),WLATX(JMAX)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      CALL SPFFTE(IMAX,(IMAX+2)/2,IMAX,2,0.,0.,0,AFFT)
      CALL SPLAT(IDRT,JMAX,SLATX,WLATX)
      JHE=(JMAX+1)/2
      IF(JHE.GT.JMAX/2) WLATX(JHE)=WLATX(JHE)/2
      DO J=JB,JE
        CLAT(J)=SQRT(1.-SLATX(J)**2)
        SLAT(J)=SLATX(J)
        WLAT(J)=WLATX(J)
      ENDDO
C$OMP PARALLEL DO
      DO J=JB,JE
        CALL SPLEGEND(IROMB,MAXWV,SLAT(J),CLAT(J),EPS,EPSTOP,
     &                PLN(1,J),PLNTOP(1,J))
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
