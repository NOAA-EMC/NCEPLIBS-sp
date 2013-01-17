!-----------------------------------------------------------------------
      SUBROUTINE SPLAT(IDRT,JMAX,SLAT,WLAT)
!$$$ SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: SPLAT COMPUTE LATITUDE FUNCTIONS
! PRGMMR: IREDELL ORG: W/NMC23 DATE: 96-02-20
!
! ABSTRACT: COMPUTES COSINES OF COLATITUDE AND GAUSSIAN WEIGHTS
! FOR ONE OF THE FOLLOWING SPECIFIC GLOBAL SETS OF LATITUDES.
! GAUSSIAN LATITUDES (IDRT=4)
! EQUALLY-SPACED LATITUDES INCLUDING POLES (IDRT=0)
! EQUALLY-SPACED LATITUDES EXCLUDING POLES (IDRT=256)
! THE GAUSSIAN LATITUDES ARE LOCATED AT THE ZEROES OF THE
! LEGENDRE POLYNOMIAL OF THE GIVEN ORDER. THESE LATITUDES
! ARE EFFICIENT FOR REVERSIBLE TRANSFORMS FROM SPECTRAL SPACE.
! (ABOUT TWICE AS MANY EQUALLY-SPACED LATITUDES ARE NEEDED.)
! THE WEIGHTS FOR THE EQUALLY-SPACED LATITUDES ARE BASED ON
! ELLSAESSER (JAM,1966). (NO WEIGHT IS GIVEN THE POLE POINT.)
! NOTE THAT WHEN ANALYZING GRID TO SPECTRAL IN LATITUDE PAIRS,
! IF AN EQUATOR POINT EXISTS, ITS WEIGHT SHOULD BE HALVED.
! THIS VERSION INVOKES THE IBM ESSL MATRIX SOLVER.
!
! PROGRAM HISTORY LOG:
! 96-02-20 IREDELL
! 97-10-20 IREDELL ADJUST PRECISION
! 98-06-11 IREDELL GENERALIZE PRECISION USING FORTRAN 90 INTRINSIC
! 1998-12-03 IREDELL GENERALIZE PRECISION FURTHER
! 1998-12-03 IREDELL USE BLAS CALLS
! 2009-12-27 DSTARK updated to switch between ESSL calls on an AIX
! platform, and Numerical Recipies calls elsewise.
! 2012-09-21 JUANG Use higher precision for best accuracy in Gaussian
! weighting
C 2012-09-01  E.Mirvis & M.Iredell merging & debugging linux errors
C                     of _d and _8 using generic LU factorization.
C 2012-11-05  E.Mirvis  generic FFTPACK and LU lapack were removed
C----------------------------------------------------------------

! USAGE: CALL SPLAT(IDRT,JMAX,SLAT,WLAT)
!
! INPUT ARGUMENT LIST:
! IDRT - INTEGER GRID IDENTIFIER
! (IDRT=4 FOR GAUSSIAN GRID,
! IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
! IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
! JMAX - INTEGER NUMBER OF LATITUDES.
!
! OUTPUT ARGUMENT LIST:
! SLAT - REAL (JMAX) SINES OF LATITUDE.
! WLAT - REAL (JMAX) GAUSSIAN WEIGHTS.
!
! SUBPROGRAMS CALLED:
! DGEF MATRIX FACTORIZATION
! DGES MATRIX SOLVER
! LUDCMP LU factorization - numerical recipies
! LUBKSB Matrix solver - numerical recipies
!
! ATTRIBUTES:
! LANGUAGE: FORTRAN 90
!
!$$$
      REAL SLAT(JMAX),WLAT(JMAX)
      INTEGER,PARAMETER:: KD=SELECTED_REAL_KIND(30,90)
! INTEGER,PARAMETER:: KD=SELECTED_REAL_KIND(20,60)
      REAL(KIND=KD):: PK(JMAX/2),PKM1(JMAX/2),PKM2(JMAX/2)
      REAL(KIND=KD):: SLATD(JMAX/2),SP,SPMAX,EPS=10.*EPSILON(SP)
      PARAMETER(JZ=50)
      REAL BZ(JZ)
      DATA BZ / 2.4048255577, 5.5200781103, 
     & 8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679, 
     & 21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684, 
     & 33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132, 
     & 46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550, 
     & 58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299, 
     & 71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711, 
     & 84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819, 
     & 96.6052679510, 99.7468198587, 102.888374254, 106.029930916, 
     & 109.171489649, 112.313050280, 115.454612653, 118.596176630, 
     & 121.737742088, 124.879308913, 128.020877005, 131.162446275, 
     & 134.304016638, 137.445588020, 140.587160352, 143.728733573, 
     & 146.870307625, 150.011882457, 153.153458019, 156.295034268 /
!==EM==    vv  REAL(8):: DLT,D1=1.
	REAL:: DLT,D1=1.
      REAL AWORK((JMAX+1)/2,((JMAX+1)/2)),BWORK(((JMAX+1)/2))
!==EM==   ^^   REAL(8) AWORK((JMAX+1)/2,((JMAX+1)/2)),BWORK(((JMAX+1)/2))
!==EM==        vvv       INTEGER(4):: JHE,JHO,J0=0
        INTEGER:: JHE,JHO,J0=0
!==EM==            INTEGER(4) IPVT((JMAX+1)/2)
        INTEGER:: IPVT((JMAX+1)/2)
      PARAMETER(PI=3.14159265358979,C=(1.-(2./PI)**2)*0.25)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! GAUSSIAN LATITUDES
      IF(IDRT.EQ.4) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        R=1./SQRT((JMAX+0.5)**2+C)
        DO J=1,MIN(JH,JZ)
          SLATD(J)=COS(BZ(J)*R)
        ENDDO
        DO J=JZ+1,JH
          SLATD(J)=COS((BZ(JZ)+(J-JZ)*PI)*R)
        ENDDO
        SPMAX=1.
        DO WHILE(SPMAX.GT.EPS)
          SPMAX=0.
          DO J=1,JH
            PKM1(J)=1.
            PK(J)=SLATD(J)
          ENDDO
          DO N=2,JMAX
            DO J=1,JH
              PKM2(J)=PKM1(J)
              PKM1(J)=PK(J)
              PK(J)=((2*N-1)*SLATD(J)*PKM1(J)-(N-1)*PKM2(J))/N
            ENDDO
          ENDDO
          DO J=1,JH
            SP=PK(J)*(1.-SLATD(J)**2)/(JMAX*(PKM1(J)-SLATD(J)*PK(J)))
            SLATD(J)=SLATD(J)-SP
            SPMAX=MAX(SPMAX,ABS(SP))
          ENDDO
        ENDDO
!DIR$ IVDEP
        DO J=1,JH
          SLAT(J)=SLATD(J)
          WLAT(J)=(2.*(1.-SLATD(J)**2))/(JMAX*PKM1(J))**2
          SLAT(JMAX+1-J)=-SLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          SLAT(JHE)=0.
          WLAT(JHE)=2./JMAX**2
          DO N=2,JMAX,2
            WLAT(JHE)=WLAT(JHE)*N**2/(N-1)**2
          ENDDO
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! EQUALLY-SPACED LATITUDES INCLUDING POLES
      ELSEIF(IDRT.EQ.0) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE-1
        DLT=PI/(JMAX-1)
        SLAT(1)=1.
        DO J=2,JH
          SLAT(J)=COS((J-1)*DLT)
        ENDDO
        DO JS=1,JHO
          DO J=1,JHO
            AWORK(JS,J)=COS(2*(JS-1)*J*DLT)
          ENDDO
        ENDDO
        DO JS=1,JHO
          BWORK(JS)=-D1/(4*(JS-1)**2-1)
        ENDDO





        call ludcmp(awork,jho,jhe,ipvt)
        call lubksb(awork,jho,jhe,ipvt,bwork)

        WLAT(1)=0.
        DO J=1,JHO
          WLAT(J+1)=BWORK(J)
        ENDDO
!DIR$ IVDEP
        DO J=1,JH
          SLAT(JMAX+1-J)=-SLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          SLAT(JHE)=0.
          WLAT(JHE)=2.*WLAT(JHE)
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! EQUALLY-SPACED LATITUDES EXCLUDING POLES
      ELSEIF(IDRT.EQ.256) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE
        DLT=PI/JMAX
        SLAT(1)=1.
        DO J=1,JH
          SLAT(J)=COS((J-0.5)*DLT)
        ENDDO
        DO JS=1,JHO
          DO J=1,JHO
            AWORK(JS,J)=COS(2*(JS-1)*(J-0.5)*DLT)
          ENDDO
        ENDDO
        DO JS=1,JHO
          BWORK(JS)=-D1/(4*(JS-1)**2-1)
        ENDDO
        
	call ludcmp(awork,jho,jhe,ipvt,d)
        call lubksb(awork,jho,jhe,ipvt,bwork)

        WLAT(1)=0.
        DO J=1,JHO
          WLAT(J)=BWORK(J)
        ENDDO
!DIR$ IVDEP
        DO J=1,JH
          SLAT(JMAX+1-J)=-SLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          SLAT(JHE)=0.
          WLAT(JHE)=2.*WLAT(JHE)
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
