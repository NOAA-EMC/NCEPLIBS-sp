C> @file
C>
C> Compute fourier transform to gridpoints
C> @author IREDELL @date 96-02-20
C>
C> THIS SUBPROGRAM COMPUTES A SLOW FOURIER TRANSFORM
C> FROM FOURIER SPACE TO A SET OF GRIDPOINTS.
C>
C> @param M        - INTEGER FOURIER WAVENUMBER TRUNCATION
C> @param N        - INTEGER NUMBER OF GRIDPOINTS
C> @param INCW     - INTEGER FIRST DIMENSION OF THE COMPLEX AMPLITUDE ARRAY
C>                (INCW >= M+1)
C> @param INCG     - INTEGER FIRST DIMENSION OF THE GRIDPOINT ARRAY
C>                (INCG >= N)
C> @param KMAX     - INTEGER NUMBER OF FOURIER FIELDS
C> @param RLON     - REAL(N) GRID LONGITUDES IN DEGREES
C> @param W        - COMPLEX(INCW,KMAX) FOURIER AMPLITUDES
C> @param G        - REAL(INCG,KMAX) GRIDPOINT VALUES
C>
C> @note THIS SUBPROGRAM IS THREAD-SAFE.
      SUBROUTINE SPFFTPT(M,N,INCW,INCG,KMAX,RLON,W,G)

        IMPLICIT NONE
        INTEGER,INTENT(IN):: M,N,INCW,INCG,KMAX
        REAL,INTENT(IN):: RLON(N)
        REAL,INTENT(IN):: W(2*INCW,KMAX)
        REAL,INTENT(OUT):: G(INCG,KMAX)
        INTEGER I,K,L
        REAL RADLON,SLON(M),CLON(M)
        REAL,PARAMETER:: PI=3.14159265358979
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        DO I=1,N
          RADLON=PI/180*RLON(I)
          DO L=1,M
            SLON(L)=SIN(L*RADLON)
            CLON(L)=COS(L*RADLON)
          ENDDO
          DO K=1,KMAX
            G(I,K)=W(1,K)
          ENDDO
          DO L=1,M
            DO K=1,KMAX
              G(I,K)=G(I,K)+2.*(W(2*L+1,K)*CLON(L)-W(2*L+2,K)*SLON(L))
            ENDDO
          ENDDO
        ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE
