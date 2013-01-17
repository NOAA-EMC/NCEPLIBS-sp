      SUBROUTINE SPLEGEND(I,M,SLAT,CLAT,EPS,EPSTOP,PLN,PLNTOP)
!$$$ SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: SPLEGEND COMPUTE LEGENDRE POLYNOMIALS
! PRGMMR: IREDELL ORG: W/NMC23 DATE: 92-10-31
!
! ABSTRACT: EVALUATES THE ORTHONORMAL ASSOCIATED LEGENDRE POLYNOMIALS
! IN THE SPECTRAL DOMAIN AT A GIVEN LATITUDE.
! SUBPROGRAM SPLEGEND SHOULD BE CALLED ALREADY.
! IF L IS THE ZONAL WAVENUMBER, N IS THE TOTAL WAVENUMBER,
! AND EPS(L,N)=SQRT((N**2-L**2)/(4*N**2-1)) THEN
! THE FOLLOWING BOOTSTRAPPING FORMULAS ARE USED:
! PLN(0,0)=SQRT(0.5)
! PLN(L,L)=PLN(L-1,L-1)*CLAT*SQRT(FLOAT(2*L+1)/FLOAT(2*L))
! PLN(L,N)=(SLAT*PLN(L,N-1)-EPS(L,N-1)*PLN(L,N-2))/EPS(L,N)
! SYNTHESIS AT THE POLE NEEDS ONLY TWO ZONAL WAVENUMBERS.
! SCALAR FIELDS ARE SYNTHESIZED WITH ZONAL WAVENUMBER 0 WHILE
! VECTOR FIELDS ARE SYNTHESIZED WITH ZONAL WAVENUMBER 1.
! (THUS POLAR VECTOR FIELDS ARE IMPLICITLY DIVIDED BY CLAT.)
! THE FOLLOWING BOOTSTRAPPING FORMULAS ARE USED AT THE POLE:
! PLN(0,0)=SQRT(0.5)
! PLN(1,1)=SQRT(0.75)
! PLN(L,N)=(PLN(L,N-1)-EPS(L,N-1)*PLN(L,N-2))/EPS(L,N)
!
! PROGRAM HISTORY LOG:
! 91-10-31 MARK IREDELL
! 98-06-10 MARK IREDELL GENERALIZE PRECISION
! 2012-09-01 Henry Juang add x number to avoid underflow through xf_module
!
! USAGE: CALL SPLEGEND(I,M,SLAT,CLAT,EPS,EPSTOP,PLN,PLNTOP)
!
! INPUT ARGUMENT LIST:
! I - INTEGER SPECTRAL DOMAIN SHAPE
! (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
! M - INTEGER SPECTRAL TRUNCATION
! SLAT - REAL SINE OF LATITUDE
! CLAT - REAL COSINE OF LATITUDE
! EPS - REAL ((M+1)*((I+1)*M+2)/2) SQRT((N**2-L**2)/(4*N**2-1))
! EPSTOP - REAL (M+1) SQRT((N**2-L**2)/(4*N**2-1)) OVER TOP
!
! OUTPUT ARGUMENT LIST:
! PLN - REAL ((M+1)*((I+1)*M+2)/2) LEGENDRE POLYNOMIAL
! PLNTOP - REAL (M+1) LEGENDRE POLYNOMIAL OVER TOP
!
! ATTRIBUTES:
! LANGUAGE: FORTRAN 90
!
!$$$
!FPP$ NOCONCUR R
      use xf_module
      REAL EPS((M+1)*((I+1)*M+2)/2),EPSTOP(M+1)
      REAL PLN((M+1)*((I+1)*M+2)/2),PLNTOP(M+1)
      REAL(KIND=SELECTED_REAL_KIND(15,45)):: DLN((M+1)*((I+1)*M+2)/2)
      REAL(KIND=SELECTED_REAL_KIND(15,45)):: aa, bb, ptop
      integer IDLN((M+1)*((I+1)*M+2)/2)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! INITIATE X NUMBER
      call xf_init()
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! ITERATIVELY COMPUTE PLN WITHIN SPECTRAL DOMAIN AT POLE
! print *,' use x number legend method with index of ',IN_F
      M1=M+1
      M2=2*M+I+1
      MX=(M+1)*((I+1)*M+2)/2
      IF(CLAT.EQ.0.) THEN
        DLN(1)=SQRT(0.5)
        iDLN(1)=0
        call xf_norm(dln(1),idln(1))
        IF(M.GT.0) THEN
          DLN(M1+1)=SQRT(0.75)
          iDLN(M1+1)=0
          call xf_norm(dln(m1+1),idln(m1+1))
          DLN(2)=SLAT*DLN(1)/EPS(2)
          iDLN(2)=iDLN(1)
          call xf_norm(dln(2),idln(2))
        ENDIF
        IF(M.GT.1) THEN
          DLN(M1+2)=SLAT*DLN(M1+1)/EPS(M1+2)
          iDLN(M1+2)=iDLN(M1+1)
          call xf_norm(dln(m1+2),idln(m1+2))
! DLN(3)=(SLAT*DLN(2)-EPS(2)*DLN(1))/EPS(3)
          aa = slat/eps(3)
          bb = -eps(2)/eps(3)
          call xf_lsum(aa,bb,dln(2),idln(2),dln(1),idln(1),
     *dln(3),idln(3))
          call xf_norm(dln(3),idln(3))
          DO N=3,M
            K=1+N
! DLN(K)=(SLAT*DLN(K-1)-EPS(K-1)*DLN(K-2))/EPS(K)
            aa = slat/eps(k)
            bb = -eps(k-1)/eps(k)
            call xf_lsum(aa,bb,dln(k-1),idln(k-1),dln(k-2),
     *idln(k-2),dln(k),idln(k))
            call xf_norm(dln(k),idln(k))
            K=M1+N
! DLN(K)=(SLAT*DLN(K-1)-EPS(K-1)*DLN(K-2))/EPS(K)
            aa = slat/eps(k)
            bb = -eps(k-1)/eps(k)
            call xf_lsum(aa,bb,dln(k-1),idln(k-1),dln(k-2),
     *idln(k-2),dln(k),idln(k))
            call xf_norm(dln(k),idln(k))
          ENDDO
          IF(I.EQ.1) THEN
            K=M2
! DLN(K)=(SLAT*DLN(K-1)-EPS(K-1)*DLN(K-2))/EPS(K)
            aa = slat/eps(k)
            bb = -eps(k-1)/eps(k)
            call xf_lsum(aa,bb,dln(k-1),idln(k-1),dln(k-2),
     *idln(k-2),dln(k),idln(k))
            call xf_norm(dln(k),idln(k))
          ENDIF
          DO K=M2+1,MX
            DLN(K)=0.
            idln(k)=0
          ENDDO
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! COMPUTE POLYNOMIALS OVER TOP OF SPECTRAL DOMAIN
        K=M1+1
! PLNTOP(1)=(SLAT*DLN(K-1)-EPS(K-1)*DLN(K-2))/EPSTOP(1)
        aa = slat/epstop(1)
        bb = -eps(k-1)/epstop(1)
        call xf_lsum(aa,bb,dln(k-1),idln(k-1),dln(k-2),idln(k-2),
     & ptop,iptop)
        call xf_norm(ptop,iptop)
        call xf_real(ptop,iptop,plntop(1))
        IF(M.GT.0) THEN
          K=M2+1
! PLNTOP(2)=(SLAT*DLN(K-1)-EPS(K-1)*DLN(K-2))/EPSTOP(2)
          aa = slat/epstop(2)
          bb = -eps(k-1)/epstop(2)
          call xf_lsum(aa,bb,dln(k-1),idln(k-1),dln(k-2),idln(k-2),
     & ptop,iptop)
            call xf_norm(ptop,iptop)
            call xf_real(ptop,iptop,plntop(2))
          DO L=2,M
            PLNTOP(L+1)=0.
          ENDDO
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! ITERATIVELY COMPUTE PLN(L,L) (BOTTOM HYPOTENUSE OF DOMAIN)
      ELSE
        NML=0
        K=1
        DLN(K)=SQRT(0.5)
        iDLN(K)=0
        call xf_norm(dln(k),idln(k))
        DO L=1,M+(I-1)*NML
          KP=K
          K=L*(2*M+(I-1)*(L-1))/2+L+NML+1
          DLN(K)=DLN(KP)*CLAT*SQRT(FLOAT(2*L+1)/FLOAT(2*L))
          iDLN(K)=iDLN(KP)
          call xf_norm(dln(k),idln(k))
        ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! COMPUTE PLN(L,L+1) (DIAGONAL NEXT TO BOTTOM HYPOTENUSE OF DOMAIN)
        NML=1
!DIR$ IVDEP
        DO L=0,M+(I-1)*NML
          K=L*(2*M+(I-1)*(L-1))/2+L+NML+1
          DLN(K)=SLAT*DLN(K-1)/EPS(K)
          iDLN(K)=iDLN(K-1)
          call xf_norm(dln(k),idln(k))
        ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! COMPUTE REMAINING PLN IN SPECTRAL DOMAIN
        DO NML=2,M
!DIR$ IVDEP
          DO L=0,M+(I-1)*NML
            K=L*(2*M+(I-1)*(L-1))/2+L+NML+1
! DLN(K)=(SLAT*DLN(K-1)-EPS(K-1)*DLN(K-2))/EPS(K)
            aa = slat/eps(k)
            bb = -eps(k-1)/eps(k)
            call xf_lsum(aa,bb,dln(k-1),idln(k-1),dln(k-2),idln(k-2),
     & dln(k),idln(k))
            call xf_norm(dln(k),idln(k))
          ENDDO
        ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! COMPUTE POLYNOMIALS OVER TOP OF SPECTRAL DOMAIN
        DO L=0,M
          NML=M+1+(I-1)*L
          K=L*(2*M+(I-1)*(L-1))/2+L+NML+1
! PLNTOP(L+1)=(SLAT*DLN(K-1)-EPS(K-1)*DLN(K-2))/EPSTOP(L+1)
          aa = slat/epstop(L+1)
          bb = -eps(k-1)/epstop(L+1)
          call xf_lsum(aa,bb,dln(k-1),idln(k-1),dln(k-2),idln(k-2),
     & ptop,iptop)
          call xf_norm(ptop,iptop)
          call xf_real(ptop,iptop,plntop(L+1))
        ENDDO
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! RETURN VALUES
      DO K=1,MX
! PLN(K)=DLN(K)
        call xf_real(dln(k),idln(k),PLN(K))
      ENDDO
      RETURN
      END
