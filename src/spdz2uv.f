C> @file
C
C> Compute winds from divergence and vorticity
C> @author IREDELL          ORG: W/NMC23     @date 92-10-31
C>
C> Computes the wind components from divergence and vorticity
c> in spectral space.
C> Subprogram speps should be called already.
C> If L is the zonal wavenumber, N is the total wavenumber,
C> <pre>      
C> EPS(L,N) = SQRT((N**2-L**2)/(4*N**2-1))
C> </pre>
C> and A is earth radius,
C> then the zonal wind component U is computed as
C> <pre>
C> U(L,N)=-I*L/(N*(N+1))*A*D(L,N)
C> +EPS(L,N+1)/(N+1)*A*Z(L,N+1)-EPS(L,N)/N*A*Z(L,N-1)
C> </pre>
C> and the meridional wind component V is computed as
C> <pre>
C> V(L,N)=-I*L/(N*(N+1))*A*Z(L,N)
C> -EPS(L,N+1)/(N+1)*A*D(L,N+1)+EPS(L,N)/N*A*D(L,N-1)
C> </pre>
C> where D is divergence and Z is vorticity.
C> U and V are weighted by the cosine of latitude.
C> Cxtra terms are computed over top of the spectral domain.
C> Advantage is taken of the fact that EPS(L,L)=0
C> in order to vectorize over the entire spectral domain.
C>
C> @param I        - INTEGER SPECTRAL DOMAIN SHAPE
C>               (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param M        - INTEGER SPECTRAL TRUNCATION
C> @param ENN1     - REAL ((M+1)*((I+1)*M+2)/2) N*(N+1)/A**2
C> @param ELONN1   - REAL ((M+1)*((I+1)*M+2)/2) L/(N*(N+1))*A
C> @param EON      - REAL ((M+1)*((I+1)*M+2)/2) EPSILON/N*A
C> @param EONTOP   - REAL (M+1) EPSILON/N*A OVER TOP
C> @param D        - REAL ((M+1)*((I+1)*M+2)) DIVERGENCE
C> @param Z        - REAL ((M+1)*((I+1)*M+2)) VORTICITY
C> @param U        - REAL ((M+1)*((I+1)*M+2)) ZONAL WIND (TIMES COSLAT)
C> @param V        - REAL ((M+1)*((I+1)*M+2)) MERID WIND (TIMES COSLAT)
C> @param UTOP     - REAL (2*(M+1)) ZONAL WIND (TIMES COSLAT) OVER TOP
C> @param VTOP     - REAL (2*(M+1)) MERID WIND (TIMES COSLAT) OVER TOP
C>
      SUBROUTINE SPDZ2UV(I,M,ENN1,ELONN1,EON,EONTOP,D,Z,U,V,UTOP,VTOP)
      REAL ENN1((M+1)*((I+1)*M+2)/2),ELONN1((M+1)*((I+1)*M+2)/2)
      REAL EON((M+1)*((I+1)*M+2)/2),EONTOP(M+1)
      REAL D((M+1)*((I+1)*M+2)),Z((M+1)*((I+1)*M+2))
      REAL U((M+1)*((I+1)*M+2)),V((M+1)*((I+1)*M+2))
      REAL UTOP(2*(M+1)),VTOP(2*(M+1))
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE WINDS IN THE SPECTRAL DOMAIN
      K=1
      U(2*K-1)=EON(K+1)*Z(2*K+1)
      U(2*K)=EON(K+1)*Z(2*K+2)
      V(2*K-1)=-EON(K+1)*D(2*K+1)
      V(2*K)=-EON(K+1)*D(2*K+2)
      DO K=2,(M+1)*((I+1)*M+2)/2-1
        U(2*K-1)=ELONN1(K)*D(2*K)+EON(K+1)*Z(2*K+1)-EON(K)*Z(2*K-3)
        U(2*K)=-ELONN1(K)*D(2*K-1)+EON(K+1)*Z(2*K+2)-EON(K)*Z(2*K-2)
        V(2*K-1)=ELONN1(K)*Z(2*K)-EON(K+1)*D(2*K+1)+EON(K)*D(2*K-3)
        V(2*K)=-ELONN1(K)*Z(2*K-1)-EON(K+1)*D(2*K+2)+EON(K)*D(2*K-2)
      ENDDO
      K=(M+1)*((I+1)*M+2)/2
      U(2*K-1)=ELONN1(K)*D(2*K)-EON(K)*Z(2*K-3)
      U(2*K)=-ELONN1(K)*D(2*K-1)-EON(K)*Z(2*K-2)
      V(2*K-1)=ELONN1(K)*Z(2*K)+EON(K)*D(2*K-3)
      V(2*K)=-ELONN1(K)*Z(2*K-1)+EON(K)*D(2*K-2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE WINDS OVER TOP OF THE SPECTRAL DOMAIN
      DO L=0,M
        K=L*(2*M+(I-1)*(L-1))/2+I*L+M+1
        UTOP(2*L+1)=-EONTOP(L+1)*Z(2*K-1)
        UTOP(2*L+2)=-EONTOP(L+1)*Z(2*K)
        VTOP(2*L+1)=EONTOP(L+1)*D(2*K-1)
        VTOP(2*L+2)=EONTOP(L+1)*D(2*K)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
