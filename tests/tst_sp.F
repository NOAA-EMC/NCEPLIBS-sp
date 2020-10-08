C This is a test program for NCEPLIBS-sp.
C
C Ed Hartnett, Kyle Gerheiser

C     Test of ncpus(). 
      subroutine test_ncpus()
      implicit none
      integer :: n, ncpus

      n = ncpus()
#ifndef OPENMP
      if (n .ne. 1) stop 2
#endif
      end subroutine test_ncpus

C     Test of splat(). 
      subroutine test_splat()
      integer :: j, jmax
      real*8 slat(384), wlat(384)
      
      jmax = 384                ! t382 grid

      call splat(0, jmax,slat,wlat)
      
      do j = 2, jmax-1
          if (slat(j) < slat(j+1)) stop 2
      end do

      end subroutine test_splat

C     Run all the tests. 
      program tst_sp
      implicit none

      print *, ''
      print *,'*** Testing NCEPLIBS-sp.'

      call test_ncpus()
      call test_splat()
      
      print *,'*** SUCCESS!'
      end program 
