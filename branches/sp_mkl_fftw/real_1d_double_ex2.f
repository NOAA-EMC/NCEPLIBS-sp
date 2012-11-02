!*****************************************************************************
! Copyright(C) 2006-2011 Intel Corporation. All Rights Reserved.
! 
! The source code, information  and  material ("Material") contained herein is
! owned  by Intel Corporation or its suppliers or licensors, and title to such
! Material remains  with Intel Corporation  or its suppliers or licensors. The
! Material  contains proprietary information  of  Intel or  its  suppliers and
! licensors. The  Material is protected by worldwide copyright laws and treaty
! provisions. No  part  of  the  Material  may  be  used,  copied, reproduced,
! modified, published, uploaded, posted, transmitted, distributed or disclosed
! in any way  without Intel's  prior  express written  permission. No  license
! under  any patent, copyright  or  other intellectual property rights  in the
! Material  is  granted  to  or  conferred  upon  you,  either  expressly,  by
! implication, inducement,  estoppel or  otherwise.  Any  license  under  such
! intellectual  property  rights must  be express  and  approved  by  Intel in
! writing.
! 
! *Third Party trademarks are the property of their respective owners.
! 
! Unless otherwise  agreed  by Intel  in writing, you may not remove  or alter
! this  notice or  any other notice embedded  in Materials by Intel or Intel's
! suppliers or licensors in any way.
!
!*****************************************************************************
! Content:
!      MKL DFTI implementation through FFTW interface (via wrappers) example
!      program (Fortran-interface)
!
! Real-to-complex and complex-to-real 1D transform for REAL*8 and COMPLEX*16
! data not inplace.
!
! Configuration parameters for MKL DFTI:
!         DFTI_FORWARD_DOMAIN = DFTI_REAL        (obligatory)
!         DFTI_PRECISION      = DFTI_DOUBLE      (obligatory)
!         DFTI_DIMENSION      = 1                (obligatory)
!         DFTI_LENGTHS        = N                (obligatory)
!         DFTI_PLACEMENT      = DFTI_NOT_INPLACE (default=DFTI_INPLACE)
!         DFTI_FORWARD_SCALE  = 1.0              (default)
!         DFTI_BACKWARD_SCALE = 1.0/N            (default=1.0)
!
! Other default configuration parameters are in the mkl_dfti.f90 interface file
!*****************************************************************************

      PROGRAM REAL_1D_DOUBLE_EX2

      INCLUDE 'fftw3.f'
      INCLUDE 'mkl_fftw_examples.fi'

      INTEGER N,RANK,OUT_N,I,DIM
      PARAMETER (RANK=1)
      PARAMETER (N=5500)
      PARAMETER (OUT_N=N/2+1)
      INTEGER*8 MY_PLAN

      REAL*8 IN(N),EXP_X(N)
      COMPLEX*16 OUT(OUT_N)
      REAL*8 ERR,SCALE
      DIMENSION DIM(RANK)
!
!     Initialize IN and copy to expected EXP_X
!
      PRINT *, ' Initialize data array'
      DO I=1,N
         IN(I)=DSIN(DFLOAT(I))
         EXP_X(I)=IN(I)
      END DO

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
!     Create FFTW plan for 1D complex to real transform
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
         IN(I)=SCALE*IN(I)
      END DO

!
!     Check results
!
      PRINT *, ' Check results'
      CALL CHECK_RESULT_D(IN,EXP_X,N,ERR)

      PRINT *, ' Accuracy=', ERR
      IF (ERR .GT. MAX_SINGLE_ERR) THEN
         PRINT *, ' TEST FAILED'
         STOP 1
      ELSE
         PRINT *, ' TEST PASSED'
      END IF

      PRINT *, ' END OF TEST'

      END PROGRAM
