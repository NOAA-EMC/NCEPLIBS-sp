! 			Eugene Mirvis, NCAR@NOAA   
!
! Septiember 2012, EMC/NOAA, NCWCP				
!				     	
!    A block of look-alike fftpack library interfaces,  
!    but utilizing scalable Intel MKL FFTW3 wrappers instead 
!	targeted for NCEP SP-lib
!
!		Contact:       Eugene.Mirvis@noaa.gov
!				or    Mirvis@ucar.edu
!----------------------------------------------------
! 
!				
      SUBROUTINE dcrft(init,x,ldx,y,ldy,n,m,isign,scale,
     &                 table,n1,wrk,n2,z,nz)
 
      implicit none
      double complex x(2*ldx,*) 
	integer init,ldx,ldy,n,m,isign,n1,n2,nz,i,j
      real y(ldy,*),scale,table(*),wrk,z
!==EM==-----------------------------------------vvvvvvvvvvvvv
	FTTW3:
	Double precision complex to real transform
	based on Intel MKL  wrappers.
!==EM==-----------------------------------------vvvvvvvvvvvvv
      INCLUDE 'fftw3.f'
      INCLUDE 'mkl_fftw_examples.fi'

      INTEGER N,RANK,IN_N,I,DIM
      PARAMETER (RANK=1)
      PARAMETER (IN_N=N/2+1)
      INTEGER*8 MY_PLAN
	

      REAL*8 IN(N),EXP_X(N)
  	COMPLEX*16 OUT(out_N)
      REAL*8 ERR,SCALE
      DIMENSION DIM(RANK)
!
!     Initialize IN and copy to expected exp_X
!
      DIM(1) = N


!==EM==-----------------------------------------^^^^^^^^^^^^^ 
! 			_________________Initializing
!
!              Initialize IN and copy to expected EXP_X
      IF (init.ne.0) THEN
!
!     Initialize IN and copy to expected EXP_X
!
      PRINT *, ' Initialize data array'
      DO I=1,N
         IN(I)=DSIN(DFLOAT(I))
         EXP_X(I)=IN(I)
      END DO
!                       _________________Compuation      
	ELSE

!OCL NOVREC ----------------- OMP DO-LOOP over serious
        DO j=1,m
          DO i=1,n
            IN(i)=x(i,j)
          ENDDO
!==EM==
!
!
      PRINT *, ' Create FFTW plan for 1D complex to real transform'
      CALL DFFTW_PLAN_DFT_C2R_1D(MY_PLAN,N,OUT,IN,FFTW_ESTIMATE)

!
!     Compute 1D complex to real transform
!
      PRINT *, ' Compute 1D complex to real transform'
      CALL DFFTW_EXECUTE(MY_PLAN)

!
!     Destroy FFTW plan
!
      PRINT *, ' Destroy FFTW plan'
      CALL DFFTW_DESTROY_PLAN(MY_PLAN)

!
!     Scale result. FFTW can't do this itself.
!
      PRINT *, ' Scale result by 1/N'
      SCALE=1.0D0/N
      DO I=1,N
         OUT(I)=SCALE*OUT(I)
      END DO

!
!     Check results
!
!==EM==
          DO i=1,n
            y(i,j)= OUT(i)
          ENDDO
        ENDDO
      ENDIF
!==EM==              vvvvvvvvvv debug vvvvvvvvvvvvvvvvvvv           
	print *, "N OUT after DblC->R -----vvvvvvv',(out(i),i1,n)
	print *, "N OUT after DblC->R -----^^^^^^^' 
      RETURN
      END

      SUBROUTINE scrft(init,x,ldx,y,ldy,n,m,isign,scale,
     &                 table,n1,wrk,n2,z,nz)
 
!==EM==-----------------------------------------vvvvvvvvvvvvv
        FTTW3  based on Intel MKL  wrappers :
        Single precision real to complex transform
!==EM==-----------------------------------------^^^^^^^^^^^^^
      implicit none
      integer init,ldx,ldy,n,m,isign,n1,n2,nz,i,j
      real x(2*ldx,*),y(ldy,*),scale,table(*),wrk,z
 
      INCLUDE 'fftw3.f'
      INCLUDE 'mkl_fftw_examples.fi'

      INTEGER N,RANK,IN_N,I,DIM
      PARAMETER (RANK=1)
!      PARAMETER (N=5500)
      PARAMETER (IN_N=N/2+1)
      INTEGER*8 MY_PLAN


      REAL*8 OUT(N),EXP_X(N)
        COMPLEX*16 IN(in_N)
      REAL*8 ERR,SCALE
      DIMENSION DIM(RANK)
!
!     Initialize IN and copy to expected EXP_X
!
      DIM(1) = N


!==EM==-----------------------------------------^^^^^^^^^^^^^
!                       _________________Initializing
!
!              Initialize IN and copy to expected EXP_X
      IF (init.ne.0) THEN
!
!     Initialize IN and copy to expected EXP_X
!
      PRINT *, ' Initialize data array'
      DO I=1,N
         IN(I)=DSIN(DFLOAT(I))
         EXP_X(I)=IN(I)
      END DO
!                       _________________Computaion
	ELSE
 
!OCL NOVREC
        DO j=1,m
          y(1,j)=x(1,j)
          DO i=2,n
            y(i,j)=x(i+1,j)
          ENDDO
          !
!     Create FFTW plan for 1D complex to real transform
!
      PRINT *, ' Create FFTW plan for 1D complex to real transform'
      CALL SFFTW_PLAN_DFT_C2R_1D(MY_PLAN,N,OUT,IN,FFTW_ESTIMATE)

!
!     Compute 1D complex to real transform
!
      PRINT *, ' Compute 1D complex to real transform'
      CALL SFFTW_EXECUTE(MY_PLAN)

!
!     Destroy FFTW plan
!
      PRINT *, ' Destroy FFTW plan'
      CALL SFFTW_DESTROY_PLAN(MY_PLAN)

!
!     Scale result. FFTW can't do this itself.
!
      PRINT *, ' Scale result by 1/N'
      SCALE=1.0/N
      DO I=1,N
         IN(I)=SCALE*IN(I)
      END DO

	DO i=1,n
            y(i,j)=scale*y(i,j)
          ENDDO
        ENDDO
      ENDIF
 
      RETURN
      END
c
c***********************************************************************

c
      SUBROUTINE drcft(init,x,ldx,y,ldy,n,m,isign,scale,
     &                 table,n1,wrk,n2,z,nz)
 
      implicit none
      integer init,ldx,ldy,n,m,isign,n1,n2,nz,i,j
      real x(ldx,*),y(2*ldy,*),scale,table(44002),wrk,z
!==EM==---------------------------vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      INCLUDE 'fftw3.f'
      INCLUDE 'mkl_fftw_examples.fi'

      INTEGER N,RANK,OUT_N,I,DIM
      PARAMETER (RANK=1)
!      PARAMETER (N=5500)
      PARAMETER (OUT_N=N/2+1)
      INTEGER*8 MY_PLAN


      REAL*8 IN(N),EXP_X(N)
      COMPLEX*16 OUT(OUT_N)
      REAL*8 ERR,SCALE
      DIMENSION DIM(RANK)
!
!     Initialize IN and copy to expected EXP_X
!
      DIM(1) = N

!
!     Create FFTW plan for 1D real to complex transform
!
      PRINT *, ' Create FFTW plan for 1D real to complex transform'
      CALL DFFTW_PLAN_DFT_R2C_1D(MY_PLAN,N,IN,OUT,FFTW_ESTIMATE)

!
!     Compute 1D real to complex transform
!
      PRINT *, ' Compute 1D real to complex transform'
      CALL DFFTW_EXECUTE(MY_PLAN)

!
!     Destroy FFTW plan
!
      PRINT *, ' Destroy FFTW plan'
      CALL DFFTW_DESTROY_PLAN(MY_PLAN)

!

!==EM==-----------------------------------------^^^^^^^^^^^^^
!                       _________________Initialing
!              Initialize IN and copy to expected EXP_X
      IF (init.ne.0) THEN
!
!     Initialize IN and copy to expected EXP_X
!
      PRINT *, ' Initialize data array'
      DO I=1,N
         IN(I)=DSIN(DFLOAT(I))
         EXP_X(I)=IN(I)
      END DO
!                       _________________Compuation
        ELSE

!OCL NOVREC ----------------- OMP DO-LOOP over serious
        DO j=1,m
          DO i=1,n
            IN(i)=x(i,j)
          ENDDO
!==EM==
!
!     Create FFTW plan for 1D real to complex transform
!
      PRINT *, ' Create FFTW plan for 1D real to complex transform'
      CALL DFFTW_PLAN_DFT_R2C_1D(MY_PLAN,N,IN,OUT,FFTW_ESTIMATE)

!
!     Compute 1D real to complex transform
!
      PRINT *, ' Compute 1D real to complex transform'
      CALL DFFTW_EXECUTE(MY_PLAN)

!
!     Destroy FFTW plan
!
      PRINT *, ' Destroy FFTW plan'
      CALL DFFTW_DESTROY_PLAN(MY_PLAN)

!
!==EM==
          DO i=1,n
            y(i,j)= OUT(i)
          ENDDO
        ENDDO
      ENDIF
!==EM==              vvvvvvvvvv debug vvvvvvvvvvvvvvvvvvv
        print *, "N OUT after DblC->R -----vvvvvvv',(out(i),i1,n)
        print *, "N OUT after DblC->R -----^^^^^^^'

      RETURN
      END
!==EM==---------------------------^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      RETURN
      END

      SUBROUTINE srcft(init,x,ldx,y,ldy,n,m,isign,scale,
     &                 table,n1,wrk,n2,z,nz)
 
      implicit none
      integer init,ldx,ldy,n,m,isign,n1,n2,nz,i,j
      real x(ldx,*),y(2*ldy,*),scale,table(44002),wrk,z
!==EM==-----------------------------------------------vvvvvvvvvvvvvvv
      INCLUDE 'fftw3.f'
      INCLUDE 'mkl_fftw_examples.fi'

      INTEGER N,RANK,OUT_N,I
      PARAMETER (RANK=1)
!      PARAMETER (N=20000)
      PARAMETER (OUT_N=N/2+1)
      INTEGER*8 MY_PLAN

      REAL*4 IN(N),EXP_X(N)
      COMPLEX*8 OUT(OUT_N)
      REAL*4 ERR,SCALE

        DO j=1,m
          DO i=1,n
            IN(i)=x(i,j)
          ENDDO
!==EM==-------------------------------	^^^^^^^^^^^^^^^^^^^^^^^^^^ 
      IF (init.ne.0) THEN
!
!     Initialize IN and copy to expected EXP_X
!
      PRINT *, ' Initialize data array'
      DO I=1,N
         IN(I)=SIN(FLOAT(I))
         EXP_X(I)=IN(I)
      END DO
 

	ELSE              ! in case of computing     
        DO j=1,m
          DO i=1,n
            y(i,j)=x(i,j)
          ENDDO
          
!
!     Create FFTW plan for 1D real to complex transform
!
      PRINT *, ' Create FFTW plan for 1D real to complex transform'
      CALL SFFTW_PLAN_DFT_R2C_1D(MY_PLAN,N,IN,OUT,FFTW_ESTIMATE)

!
!     Compute 1D real to complex transform
!
      PRINT *, ' Compute 1D real to complex transform'
      CALL SFFTW_EXECUTE(MY_PLAN)

!
!     Destroy FFTW plan
!
      PRINT *, ' Destroy FFTW plan'
      CALL SFFTW_DESTROY_PLAN(MY_PLAN)

		DO i=1,n
            y(i,j)=scale*y(i,j)
          ENDDO
          DO i=n,2,-1
            y(i+1,j)=y(i,j)
          ENDDO
          y(2,j)=0.
        ENDDO
      ENDIF
!==EM==---------------------------vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!==EM==              vvvvvvvvvv debug vvvvvvvvvvvvvvvvvvv
        print *, "N OUT after DblC->R -----vvvvvvv',(out(i),i1,n)
        print *, "N OUT after DblC->R -----^^^^^^^'

      RETURN
      END
