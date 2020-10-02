C> @file
C>
C> Pad or truncate a spectral field
C> @author IREDELL @date 92-10-31

C> Pad or truncate a spectral field.
C>
C> @param I1       - INTEGER INPUT SPECTRAL DOMAIN SHAPE
C>                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param M1       - INTEGER INPUT SPECTRAL TRUNCATION
C> @param Q1       - REAL ((M+1)*((I+1)*M+2)) INPUT FIELD
C> @param I2       - INTEGER OUTPUT SPECTRAL DOMAIN SHAPE
C>                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param M2       - INTEGER OUTPUT SPECTRAL TRUNCATION
C>
C> @param Q2       - REAL ((M+1)*((I+1)*M+2)) OUTPUT FIELD
      SUBROUTINE SPPAD(I1,M1,Q1,I2,M2,Q2)

      REAL Q1((M1+1)*((I1+1)*M1+2))
      REAL Q2((M2+1)*((I2+1)*M2+2))
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      DO L=0,M2
        DO N=L,I2*L+M2
          KS2=L*(2*M2+(I2-1)*(L-1))+2*N
          IF(L.LE.M1.AND.N.LE.I1*L+M1) THEN
            KS1=L*(2*M1+(I1-1)*(L-1))+2*N
            Q2(KS2+1)=Q1(KS1+1)
            Q2(KS2+2)=Q1(KS1+2)
          ELSE
            Q2(KS2+1)=0
            Q2(KS2+2)=0
          ENDIF
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
