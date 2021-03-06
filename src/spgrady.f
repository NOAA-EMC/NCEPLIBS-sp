C> @file
C>
C> Compute y-gradient in spectral space
C> @author IREDELL @date 92-10-31

C> COMPUTES THE HORIZONTAL VECTOR Y-GRADIENT OF A SCALAR FIELD
C> IN SPECTRAL SPACE.
C> SUBPROGRAM SPEPS SHOULD BE CALLED ALREADY.
C> IF L IS THE ZONAL WAVENUMBER, N IS THE TOTAL WAVENUMBER,
C> EPS(L,N)=SQRT((N**2-L**2)/(4*N**2-1)) AND A IS EARTH RADIUS,
C> THEN THE MERIDIONAL GRADIENT OF Q(L,N) IS COMPUTED AS
C> EPS(L,N+1)*(N+2)/A*Q(L,N+1)-EPS(L,N+1)*(N-1)/A*Q(L,N-1).
C> EXTRA TERMS ARE COMPUTED OVER TOP OF THE SPECTRAL DOMAIN.
C> ADVANTAGE IS TAKEN OF THE FACT THAT EPS(L,L)=0
C> IN ORDER TO VECTORIZE OVER THE ENTIRE SPECTRAL DOMAIN.
C>
C> @param I        - INTEGER SPECTRAL DOMAIN SHAPE
C>                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C> @param M        - INTEGER SPECTRAL TRUNCATION
C> @param ENN1     - REAL ((M+1)*((I+1)*M+2)/2) N*(N+1)/A**2
C> @param EON      - REAL ((M+1)*((I+1)*M+2)/2) EPSILON/N*A
C> @param EONTOP   - REAL (M+1) EPSILON/N*A OVER TOP
C> @param Q        - REAL ((M+1)*((I+1)*M+2)) SCALAR FIELD
C> @param QDY      - REAL ((M+1)*((I+1)*M+2)) MERID GRADIENT (TIMES COSLAT)
C> @param QDYTOP   - REAL (2*(M+1)) MERID GRADIENT (TIMES COSLAT) OVER TOP
      SUBROUTINE SPGRADY(I,M,ENN1,EON,EONTOP,Q,QDY,QDYTOP)

      REAL ENN1((M+1)*((I+1)*M+2)/2)
      REAL EON((M+1)*((I+1)*M+2)/2),EONTOP(M+1)
      REAL Q((M+1)*((I+1)*M+2))
      REAL QDY((M+1)*((I+1)*M+2))
      REAL QDYTOP(2*(M+1))
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TAKE MERIDIONAL GRADIENT
      K=1
      QDY(2*K-1)=EON(K+1)*ENN1(K+1)*Q(2*K+1)
      QDY(2*K)=EON(K+1)*ENN1(K+1)*Q(2*K+2)
      DO K=2,(M+1)*((I+1)*M+2)/2-1
        QDY(2*K-1)=EON(K+1)*ENN1(K+1)*Q(2*K+1)-EON(K)*ENN1(K-1)*Q(2*K-3)
        QDY(2*K)=EON(K+1)*ENN1(K+1)*Q(2*K+2)-EON(K)*ENN1(K-1)*Q(2*K-2)
      ENDDO
      K=(M+1)*((I+1)*M+2)/2
      QDY(2*K-1)=-EON(K)*ENN1(K-1)*Q(2*K-3)
      QDY(2*K)=-EON(K)*ENN1(K-1)*Q(2*K-2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TAKE MERIDIONAL GRADIENT OVER TOP
      DO L=0,M
        K=L*(2*M+(I-1)*(L-1))/2+I*L+M+1
        QDYTOP(2*L+1)=-EONTOP(L+1)*ENN1(K)*Q(2*K-1)
        QDYTOP(2*L+2)=-EONTOP(L+1)*ENN1(K)*Q(2*K)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
