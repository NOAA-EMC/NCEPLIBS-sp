C> @file
C> @brief Get wave-space constants.
C> @author IREDELL @date 96-02-29

C> This subprogram gets wave-space constants.
C>
C> @param IROMB SPECTRAL DOMAIN SHAPE (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param MAXWV SPECTRAL TRUNCATION
C> @param EPS 
C> @param EPSTOP 
C> @param ENN1 
C> @param ELONN1 
C> @param EON 
C> @param EONTOP 
C>
C> @author IREDELL @date 96-02-29      
      SUBROUTINE SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MXTOP=MAXWV+1
      CALL SPEPS(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      END
