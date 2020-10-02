C> @file
C>
C> Perform multiple fast fourier transforms
C> @author IREDELL @date 96-02-20
C>
C> This subprogram performs multiple fast fourier transforms
C> between complex amplitudes in fourier space and real values
C> in cyclic physical space.
C> Subprogram SPFFT1 initializes trigonometric data each call.
C> Use subprogram SPFFT to save time and initialize once.
C> this version invokes the ibm essl fft.
C>
C>     IMAX     - INTEGER NUMBER OF VALUES IN THE CYCLIC> PHYSICAL SPACE
C>                (SEE LIMITATIONS ON IMAX IN REMARKS BELOW.)
C>     INCW     - INTEGER FIRST DIMENSION OF THE COMPLEX AMPLITUDE ARRAY
C>                (INCW >= IMAX/2+1)
C>     INCG     - INTEGER FIRST DIMENSION OF THE REAL VALUE ARRAY
C>                (INCG >= IMAX)
C>     KMAX     - INTEGER NUMBER OF TRANSFORMS TO PERFORM
C>     W        - COMPLEX(INCW,KMAX) COMPLEX AMPLITUDES IF IDIR>0
C>     G        - REAL(INCG,KMAX) REAL VALUES IF IDIR<0
C>     IDIR     - INTEGER DIRECTION FLAG
C>                IDIR>0 TO TRANSFORM FROM FOURIER TO PHYSICAL SPACE
C>                IDIR<0 TO TRANSFORM FROM PHYSICAL TO FOURIER SPACE
C>     W        - COMPLEX(INCW,KMAX) COMPLEX AMPLITUDES IF IDIR<0
C>     G        - REAL(INCG,KMAX) REAL VALUES IF IDIR>0
C>
C> SUBPROGRAMS CALLED:
C>   - SCRFT        IBM ESSL COMPLEX TO REAL FOURIER TRANSFORM
C>   - SRCFT        IBM ESSL REAL TO COMPLEX FOURIER TRANSFORM
C>
C> @note The restrictions on IMAX are that it must be a multiple of 1
C> to 25 factors of two, up to 2 factors of three, and up to 1 factor of
C> five, seven and eleven.
C>
C> @note This subprogram is thread-safe.
      SUBROUTINE SPFFT1(IMAX,INCW,INCG,KMAX,W,G,IDIR)
        IMPLICIT NONE
        INTEGER,INTENT(IN):: IMAX,INCW,INCG,KMAX,IDIR
        COMPLEX,INTENT(INOUT):: W(INCW,KMAX)
        REAL,INTENT(INOUT):: G(INCG,KMAX)
        REAL:: AUX1(25000+INT(0.82*IMAX))
        REAL:: AUX2(20000+INT(0.57*IMAX))
        INTEGER:: NAUX1,NAUX2
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        NAUX1=25000+INT(0.82*IMAX)
        NAUX2=20000+INT(0.57*IMAX)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FOURIER TO PHYSICAL TRANSFORM.
        SELECT CASE(IDIR)
          CASE(1:)
            CALL SCRFT(1,W,INCW,G,INCG,IMAX,KMAX,-1,1.,
     &                 AUX1,NAUX1,AUX2,NAUX2,0.,0)
            CALL SCRFT(0,W,INCW,G,INCG,IMAX,KMAX,-1,1.,
     &                 AUX1,NAUX1,AUX2,NAUX2,0.,0)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PHYSICAL TO FOURIER TRANSFORM.
          CASE(:-1)
            CALL SRCFT(1,G,INCG,W,INCW,IMAX,KMAX,+1,1./IMAX,
     &               AUX1,NAUX1,AUX2,NAUX2,0.,0)
            CALL SRCFT(0,G,INCG,W,INCW,IMAX,KMAX,+1,1./IMAX,
     &               AUX1,NAUX1,AUX2,NAUX2,0.,0)
        END SELECT
      END SUBROUTINE
