C> @file
C> @brief Perform a vector spherical transform
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|---------
C> 96-02-29 | IREDELL | Initial.
C> 1998-12-15 | IREDELL | Generic fft used, openmp directives inserted
C> 2013-01-16 | IREDELL & MIRVIS | Fixing afft negative sharing effect during omp loops
C>
C> @author IREDELL @date 96-02-29

C> This subprogram performs a spherical transform
c> between spectral coefficients of divergences and curls
c> and vector fields on a global cylindrical grid.
C>
C> The wave-space can be either triangular or rhomboidal.
C>
C> The grid-space can be either an equally-spaced grid
c> (with or without pole points) or a gaussian grid.
C>
C> The wave and grid fields may have general indexing,
c> but each wave field is in sequential 'ibm order',
c> i.e. with zonal wavenumber as the slower index.
C>
C> Transforms are done in latitude pairs for efficiency;
c> thus grid arrays for each hemisphere must be passed.
c> if so requested, just a subset of the latitude pairs
c> may be transformed in each invocation of the subprogram.
C>
C> The transforms are all multiprocessed over latitude except
c> the transform from fourier to spectral is multiprocessed
c> over zonal wavenumber to ensure reproducibility.
C>
C> Transform several fields at a time to improve vectorization.
c> subprogram can be called from a multiprocessing environment.
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
C>
C> @param IROMB SPECTRAL DOMAIN SHAPE
C> (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param MAXWV SPECTRAL TRUNCATION
C> @param IDRT GRID IDENTIFIER
C> - IDRT=4 FOR GAUSSIAN GRID,
C> - IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C> - IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES
C> @param IMAX EVEN NUMBER OF LONGITUDES.
C> @param JMAX NUMBER OF LATITUDES.
C> @param KMAX NUMBER OF FIELDS TO TRANSFORM.
C> @param IP LONGITUDE INDEX FOR THE PRIME MERIDIAN
C> @param IS SKIP NUMBER BETWEEN LONGITUDES
C> @param JN SKIP NUMBER BETWEEN N.H. LATITUDES FROM NORTH
C> @param JS SKIP NUMBER BETWEEN S.H. LATITUDES FROM SOUTH
C> @param KW SKIP NUMBER BETWEEN WAVE FIELDS
C> @param KG SKIP NUMBER BETWEEN GRID FIELDS
C> @param JB LATITUDE INDEX (FROM POLE) TO BEGIN TRANSFORM
C> @param JE LATITUDE INDEX (FROM POLE) TO END TRANSFORM
C> @param JC NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C> @param[out] WAVED WAVE DIVERGENCE FIELDS IF IDIR>0
C> [WAVED=(D(GRIDU)/DLAM+D(CLAT*GRIDV)/DPHI)/(CLAT*RERTH)]
C> @param[out] WAVEZ WAVE VORTICITY FIELDS IF IDIR>0
C> [WAVEZ=(D(GRIDV)/DLAM-D(CLAT*GRIDU)/DPHI)/(CLAT*RERTH)]      
C> @param[out] GRIDUN N.H. GRID U-WINDS (STARTING AT JB) IF IDIR<0
C> @param[out] GRIDUS S.H. GRID U-WINDS (STARTING AT JB) IF IDIR<0
C> @param[out] GRIDVN N.H. GRID V-WINDS (STARTING AT JB) IF IDIR<0
C> @param[out] GRIDVS S.H. GRID V-WINDS (STARTING AT JB) IF IDIR<0
C> @param IDIR TRANSFORM FLAG
C> (IDIR>0 FOR WAVE TO GRID, IDIR<0 FOR GRID TO WAVE)
C>
C> @author IREDELL @date 96-02-29
      SUBROUTINE SPTRANFV(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAX,
     &                    IP,IS,JN,JS,KW,KG,JB,JE,JC,
     &                    WAVED,WAVEZ,GRIDUN,GRIDUS,GRIDVN,GRIDVS,IDIR)

      REAL WAVED(*),WAVEZ(*),GRIDUN(*),GRIDUS(*),GRIDVN(*),GRIDVS(*)
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      REAL(8) AFFT(50000+4*IMAX), AFFT_TMP(50000+4*IMAX)
      REAL CLAT(JB:JE),SLAT(JB:JE),WLAT(JB:JE)
      REAL PLN((MAXWV+1)*((IROMB+1)*MAXWV+2)/2,JB:JE)
      REAL PLNTOP(MAXWV+1,JB:JE)
      INTEGER MP(2)
      REAL W((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2,2)
      REAL WTOP(2*(MAXWV+1),2)
      REAL G(IMAX,2,2)
      REAL WINC((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2,2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET PARAMETERS
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MP=1
      CALL SPTRANF0(IROMB,MAXWV,IDRT,IMAX,JMAX,JB,JE,
     &              EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP,
     &              AFFT,CLAT,SLAT,WLAT,PLN,PLNTOP)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM WAVE TO GRID
      IF(IDIR.GT.0) THEN
C$OMP PARALLEL DO PRIVATE(AFFT_TMP,KWS,W,WTOP,G,IJKN,IJKS)
        DO K=1,KMAX
			  AFFT_TMP=AFFT
          KWS=(K-1)*KW
          CALL SPDZ2UV(IROMB,MAXWV,ENN1,ELONN1,EON,EONTOP,
     &                 WAVED(KWS+1),WAVEZ(KWS+1),
     &                 W(1,1),W(1,2),WTOP(1,1),WTOP(1,2))
          DO J=JB,JE
            CALL SPTRANF1(IROMB,MAXWV,IDRT,IMAX,JMAX,J,J,
     &                    EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP,
     &                    AFFT_TMP,CLAT(J),SLAT(J),WLAT(J),
     &                    PLN(1,J),PLNTOP(1,J),MP,
     &                    W(1,1),WTOP(1,1),G(1,1,1),IDIR)
            CALL SPTRANF1(IROMB,MAXWV,IDRT,IMAX,JMAX,J,J,
     &                    EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP,
     &                    AFFT_TMP,CLAT(J),SLAT(J),WLAT(J),
     &                    PLN(1,J),PLNTOP(1,J),MP,
     &                    W(1,2),WTOP(1,2),G(1,1,2),IDIR)
            IF(IP.EQ.1.AND.IS.EQ.1) THEN
              DO I=1,IMAX
                IJKN=I+(J-JB)*JN+(K-1)*KG
                IJKS=I+(J-JB)*JS+(K-1)*KG
                GRIDUN(IJKN)=G(I,1,1)
                GRIDUS(IJKS)=G(I,2,1)
                GRIDVN(IJKN)=G(I,1,2)
                GRIDVS(IJKS)=G(I,2,2)
              ENDDO
            ELSE
              DO I=1,IMAX
                IJKN=MOD(I+IP-2,IMAX)*IS+(J-JB)*JN+(K-1)*KG+1
                IJKS=MOD(I+IP-2,IMAX)*IS+(J-JB)*JS+(K-1)*KG+1
                GRIDUN(IJKN)=G(I,1,1)
                GRIDUS(IJKS)=G(I,2,1)
                GRIDVN(IJKN)=G(I,1,2)
                GRIDVS(IJKS)=G(I,2,2)
              ENDDO
            ENDIF
          ENDDO
        ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM GRID TO WAVE
      ELSE
C$OMP PARALLEL DO PRIVATE(AFFT_TMP,KWS,W,WTOP,G,IJKN,IJKS,WINC)
        DO K=1,KMAX
			  AFFT_TMP=AFFT
          KWS=(K-1)*KW
          W=0
          WTOP=0
          DO J=JB,JE
            IF(WLAT(J).GT.0.) THEN
              IF(IP.EQ.1.AND.IS.EQ.1) THEN
                DO I=1,IMAX
                  IJKN=I+(J-JB)*JN+(K-1)*KG
                  IJKS=I+(J-JB)*JS+(K-1)*KG
                  G(I,1,1)=GRIDUN(IJKN)/CLAT(J)**2
                  G(I,2,1)=GRIDUS(IJKS)/CLAT(J)**2
                  G(I,1,2)=GRIDVN(IJKN)/CLAT(J)**2
                  G(I,2,2)=GRIDVS(IJKS)/CLAT(J)**2
                ENDDO
              ELSE
                DO I=1,IMAX
                  IJKN=MOD(I+IP-2,IMAX)*IS+(J-JB)*JN+(K-1)*KG+1
                  IJKS=MOD(I+IP-2,IMAX)*IS+(J-JB)*JS+(K-1)*KG+1
                  G(I,1,1)=GRIDUN(IJKN)/CLAT(J)**2
                  G(I,2,1)=GRIDUS(IJKS)/CLAT(J)**2
                  G(I,1,2)=GRIDVN(IJKN)/CLAT(J)**2
                  G(I,2,2)=GRIDVS(IJKS)/CLAT(J)**2
                ENDDO
              ENDIF
              CALL SPTRANF1(IROMB,MAXWV,IDRT,IMAX,JMAX,J,J,
     &                      EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP,
     &                      AFFT_TMP,CLAT(J),SLAT(J),WLAT(J),
     &                      PLN(1,J),PLNTOP(1,J),MP,
     &                      W(1,1),WTOP(1,1),G(1,1,1),IDIR)
              CALL SPTRANF1(IROMB,MAXWV,IDRT,IMAX,JMAX,J,J,
     &                      EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP,
     &                      AFFT_TMP,CLAT(J),SLAT(J),WLAT(J),
     &                      PLN(1,J),PLNTOP(1,J),MP,
     &                      W(1,2),WTOP(1,2),G(1,1,2),IDIR)
            ENDIF
          ENDDO
          CALL SPUV2DZ(IROMB,MAXWV,ENN1,ELONN1,EON,EONTOP,
     &                 W(1,1),W(1,2),WTOP(1,1),WTOP(1,2),
     &                 WINC(1,1),WINC(1,2))
          WAVED(KWS+1:KWS+2*MX)=WAVED(KWS+1:KWS+2*MX)+WINC(1:2*MX,1)
          WAVEZ(KWS+1:KWS+2*MX)=WAVEZ(KWS+1:KWS+2*MX)+WINC(1:2*MX,2)
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
