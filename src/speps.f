C>@file
C>
C> Compute utility spectral fields
C> @author IREDELL  @date 92-10-31

C> Computes constant fields indexed in the spectral domain
C> in "IBM ORDER" (Zonal wavenumber is the slower index).
C> If L is the zonal wavenumber and N is the total wavenumber
C> and A is the earth radius, then the fields returned are:
C> - (1) NORMALIZING FACTOR EPSILON=SQRT((N**2-L**2)/(4*N**2-1))
C> - (2) LAPLACIAN FACTOR N*(N+1)/A**2
C> - (3) ZONAL DERIVATIVE/LAPLACIAN FACTOR L/(N*(N+1))*A
C> - (4) MERIDIONAL DERIVATIVE/LAPLACIAN FACTOR EPSILON/N*A
C>
C> @param I        - INTEGER SPECTRAL DOMAIN SHAPE
C>               (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param M        - INTEGER SPECTRAL TRUNCATION
C> @param EPS      - REAL ((M+1)*((I+1)*M+2)/2) SQRT((N**2-L**2)/(4*N**2-1))
C> @param EPSTOP   - REAL (M+1) SQRT((N**2-L**2)/(4*N**2-1)) OVER TOP
C> @param ENN1     - REAL ((M+1)*((I+1)*M+2)/2) N*(N+1)/A**2
C> @param ELONN1   - REAL ((M+1)*((I+1)*M+2)/2) L/(N*(N+1))*A
C> @param EON      - REAL ((M+1)*((I+1)*M+2)/2) EPSILON/N*A
C> @param EONTOP   - REAL (M+1) EPSILON/N*A OVER TOP
C>
      SUBROUTINE SPEPS(I,M,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      REAL EPS((M+1)*((I+1)*M+2)/2),EPSTOP(M+1)
      REAL ENN1((M+1)*((I+1)*M+2)/2),ELONN1((M+1)*((I+1)*M+2)/2)
      REAL EON((M+1)*((I+1)*M+2)/2),EONTOP(M+1)
      PARAMETER(RERTH=6.3712E6,RA2=1./RERTH**2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      DO L=0,M
        K=L*(2*M+(I-1)*(L-1))/2+L+1
        EPS(K)=0.
        ENN1(K)=RA2*L*(L+1)
        ELONN1(K)=RERTH/(L+1)
        EON(K)=0.
      ENDDO
      DO L=0,M
        DO N=L+1,I*L+M
          K=L*(2*M+(I-1)*(L-1))/2+N+1
          EPS(K)=SQRT(FLOAT(N**2-L**2)/FLOAT(4*N**2-1))
          ENN1(K)=RA2*N*(N+1)
          ELONN1(K)=RERTH*L/(N*(N+1))
          EON(K)=RERTH/N*EPS(K)
        ENDDO
      ENDDO
      DO L=0,M
        N=I*L+M+1
        EPSTOP(L+1)=SQRT(FLOAT(N**2-L**2)/FLOAT(4*N**2-1))
        EONTOP(L+1)=RERTH/N*EPSTOP(L+1)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
