C> @file
C>
C> Perform multiple fast fourier transforms
C> @author IREDELL @date 96-02-20

C> This subprogram performs multiple fast fourier transforms
C> between complex amplitudes in fourier space and real values
C> in cyclic physical space.
C> Subprogram SPFFTE must be invoked first with IDIR=0
C> to initialize trigonemetric data.  use subprogram SPFFT1
C> to perform an fft without previous initialization.
C> This version invokes the ibm essl fft.
C>
C> PROGRAM HISTORY LOG:
C> - 1998-12-18  IREDELL
C> - 2012-11-12  MIRVIS -fixing hard-wired types problem on Intel/Linux 
C>
C> @param IMAX     - INTEGER NUMBER OF VALUES IN THE CYCLIC PHYSICAL SPACE
C>                (SEE LIMITATIONS ON IMAX IN REMARKS BELOW.)
C> @param INCW     - INTEGER FIRST DIMENSION OF THE COMPLEX AMPLITUDE ARRAY
C>                (INCW >= IMAX/2+1)
C> @param INCG     - INTEGER FIRST DIMENSION OF THE REAL VALUE ARRAY
C>                (INCG >= IMAX)
C> @param KMAX     - INTEGER NUMBER OF TRANSFORMS TO PERFORM
C> @param[out] W        - COMPLEX(INCW,KMAX) COMPLEX AMPLITUDES IF IDIR>0
C> @param[out] G        - REAL(INCG,KMAX) REAL VALUES IF IDIR<0
C> @param IDIR     - INTEGER DIRECTION FLAG
C> - IDIR=0 TO INITIALIZE TRIGONOMETRIC DATA
C> - IDIR>0 TO TRANSFORM FROM FOURIER TO PHYSICAL SPACE
C> - IDIR<0 TO TRANSFORM FROM PHYSICAL TO FOURIER SPACE
C> @param[out] AFFT       REAL(8) (50000+4*IMAX) AUXILIARY ARRAY IF IDIR<>0
C>
C> SUBPROGRAMS CALLED:
C>   -scrft()        IBM ESSL COMPLEX TO REAL FOURIER TRANSFORM
C>   -dcrft()        IBM ESSL COMPLEX TO REAL FOURIER TRANSFORM
C>   -srcft()        IBM ESSL REAL TO COMPLEX FOURIER TRANSFORM
C>   -drcft()        IBM ESSL REAL TO COMPLEX FOURIER TRANSFORM
C>
C> @note The restrictions on IMAX are that it must be a multiple
C> of 1 to 25 factors of two, up to 2 factors of three,
C> and up to 1 factor of five, seven and eleven.
C>
C> If IDIR=0, then W and G need not contain any valid data.
C> The other parameters must be supplied and cannot change
C> in succeeding calls until the next time it is called with IDIR=0.
C>
C> This subprogram is thread-safe.
C>      
      SUBROUTINE SPFFTE(IMAX,INCW,INCG,KMAX,W,G,IDIR,AFFT)
        IMPLICIT NONE
        INTEGER,INTENT(IN):: IMAX,INCW,INCG,KMAX,IDIR
        REAL,INTENT(INOUT):: W(2*INCW,KMAX)
        REAL,INTENT(INOUT):: G(INCG,KMAX)
        REAL(8),INTENT(INOUT):: AFFT(50000+4*IMAX)
        INTEGER:: INIT,INC2X,INC2Y,N,M,ISIGN,NAUX1,NAUX2,NAUX3
C ==EM==       ^(4)
        REAL:: SCALE
        REAL(8):: AUX2(20000+2*IMAX),AUX3
        INTEGER:: IACR,IARC
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        NAUX1=25000+2*IMAX
        NAUX2=20000+2*IMAX
        NAUX3=1
        IACR=1
        IARC=1+NAUX1
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INITIALIZATION.
C  FILL AUXILIARY ARRAYS WITH TRIGONOMETRIC DATA
        SELECT CASE(IDIR)
          CASE(0)
            INIT=1
            INC2X=INCW
            INC2Y=INCG
            N=IMAX
            M=KMAX
            ISIGN=-1
            SCALE=1.
            IF(DIGITS(1.).LT.DIGITS(1._8)) THEN
              CALL SCRFT(INIT,W,INC2X,G,INC2Y,N,M,ISIGN,SCALE,
     &                   AFFT(IACR),NAUX1,AUX2,NAUX2,AUX3,NAUX3)
            ELSE
              CALL DCRFT(INIT,W,INC2X,G,INC2Y,N,M,ISIGN,SCALE,
     &                   AFFT(IACR),NAUX1,AUX2,NAUX2)
            ENDIF
            INIT=1
            INC2X=INCG
            INC2Y=INCW
            N=IMAX
            M=KMAX
            ISIGN=+1
            SCALE=1./IMAX
            IF(DIGITS(1.).LT.DIGITS(1._8)) THEN
              CALL SRCFT(INIT,G,INC2X,W,INC2Y,N,M,ISIGN,SCALE,
     &                   AFFT(IARC),NAUX1,AUX2,NAUX2,AUX3,NAUX3)
            ELSE
              CALL DRCFT(INIT,G,INC2X,W,INC2Y,N,M,ISIGN,SCALE,
     &                   AFFT(IARC),NAUX1,AUX2,NAUX2)
            ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FOURIER TO PHYSICAL TRANSFORM.
          CASE(1:)
            INIT=0
            INC2X=INCW
            INC2Y=INCG
            N=IMAX
            M=KMAX
            ISIGN=-1
            SCALE=1.
            IF(DIGITS(1.).LT.DIGITS(1._8)) THEN
              CALL SCRFT(INIT,W,INC2X,G,INC2Y,N,M,ISIGN,SCALE,
     &                   AFFT(IACR),NAUX1,AUX2,NAUX2,AUX3,NAUX3)
            ELSE
              CALL DCRFT(INIT,W,INC2X,G,INC2Y,N,M,ISIGN,SCALE,
     &                   AFFT(IACR),NAUX1,AUX2,NAUX2)
            ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PHYSICAL TO FOURIER TRANSFORM.
          CASE(:-1)
            INIT=0
            INC2X=INCG
            INC2Y=INCW
            N=IMAX
            M=KMAX
            ISIGN=+1
            SCALE=1./IMAX
            IF(DIGITS(1.).LT.DIGITS(1._8)) THEN
              CALL SRCFT(INIT,G,INC2X,W,INC2Y,N,M,ISIGN,SCALE,
     &                   AFFT(IARC),NAUX1,AUX2,NAUX2,AUX3,NAUX3)
            ELSE
              CALL DRCFT(INIT,G,INC2X,W,INC2Y,N,M,ISIGN,SCALE,
     &                   AFFT(IARC),NAUX1,AUX2,NAUX2)
            ENDIF
        END SELECT
      END SUBROUTINE
