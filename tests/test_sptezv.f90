program test_sptezv
  use iso_fortran_env, only: real64
  implicit none

  integer,parameter:: iromb=0,maxwv=7
  integer,parameter:: idrtg=4,idrte=0,imax=16,jmaxg=8,jmaxe=17
  
  call tests(iromb,maxwv,idrtg,imax,jmaxg)
  call tests(iromb,maxwv,idrte,imax,jmaxe)
  call testv(iromb,maxwv,idrtg,imax,jmaxg)
  call testv(iromb,maxwv,idrte,imax,jmaxe)
  
  call tests(0,126,4,256,128)
  call tests(0,126,0,256,257)
  call testv(0,126,4,256,128)
  call testv(0,126,0,256,257)

contains
  
  subroutine tests(iromb,maxwv,idrt,imax,jmax)
    implicit none
    integer,intent(in):: iromb,maxwv,idrt,imax,jmax
    real(real64) :: wave((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
    real(real64) :: wave2((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
    real(real64) :: grid(imax,jmax)
    wave=1d0
    wave(2:2*maxwv+2:2)=0d0
    call sptez(iromb,maxwv,idrt,imax,jmax,wave,grid,+1)
    call sptez(iromb,maxwv,idrt,imax,jmax,wave2,grid,-1)
    print *,sqrt(sum((wave2-wave)**2)/size(wave))/epsilon(wave)
  end subroutine tests
  
  subroutine testv(iromb,maxwv,idrt,imax,jmax)
    implicit none
    integer,intent(in):: iromb,maxwv,idrt,imax,jmax
    real(real64) :: waved((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
    real(real64) :: wavez((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
    real(real64) :: waved2((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
    real(real64) :: wavez2((maxwv+1)*((iromb+1)*maxwv+2)/2*2)
    real(real64) :: gridu(imax,jmax)
    real(real64) :: gridv(imax,jmax)
    waved=1d0
    waved(2:2*maxwv+2:2)=0d0
    waved(1)=0d0
    wavez=1d0
    wavez(2:2*maxwv+2:2)=0d0
    wavez(1)=0d0
    call sptezv(iromb,maxwv,idrt,imax,jmax,waved,wavez,gridu,gridv,+1)
    call sptezv(iromb,maxwv,idrt,imax,jmax,waved2,wavez2,gridu,gridv,-1)
    print *,sqrt((sum((waved2-waved)**2)+sum((wavez2-wavez)**2))/(2*size(waved)))/epsilon(waved)
  end subroutine testv
  
end program test_sptezv
