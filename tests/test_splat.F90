! This is a test from the NCEPLIBS-sp project.
!
! This test tests the splat() subrroutine.
!
! Kyle Gerheiser
program test_splat
  use iso_fortran_env, only: real64
  implicit none

  integer :: j, jmax
  real :: slat(584), wlat(584)
  
  jmax = 584  ! t382 grid

  call splat(0, jmax, slat, wlat)

  if (slat(1) /= 1d0) then
     error stop "slat(1) should equal 1.0"
  endif

  if(slat(jmax) /= -1d0) then
     error stop "slat(jmax) should equal -1.0"
  endif

  if(wlat(1) /= 0d0) then
     error stop "wlat(1) should equal 0.0"
  endif

  if(wlat(jmax) /= 0d0) then
     error stop "wlat(jmax) should equal 0.0"
  endif

  do j = 2, jmax-1
     if (slat(j) < slat(j+1)) then
        error stop "slat should be monotonically decreasing"
     endif
  end do

  do j = 1, jmax
     print*,'J/SLAT/WLAT ',j, slat(j), wlat(j)
  enddo

end program test_splat
