program test_fft
  use iso_fortran_env, only: real32, real64
  implicit none

  real, parameter :: PI = 3.14159265358979
  integer, parameter :: precision = real64
  integer, parameter :: imax = 256
  integer, parameter :: incw = (imax / 2) + 1
  integer, parameter :: incg = imax
  integer, parameter :: kmax = 1
  
  integer, parameter :: IDIR_INITIALIZE = 0
  integer, parameter :: IDIR_C2R = 1
  integer, parameter :: IDIR_R2C = -1

  call test_fft_real_to_complex()
  call test_fft_complex_to_real()

contains

  subroutine test_fft_real_to_complex()
    real(precision) :: amplitude, freq_hz, t, cosine, dt, sample_rate_hz, dc_component, df, f, magnitude
    real(real64) :: AFFT(50000+4*IMAX)
    real(precision), allocatable :: w(:,:), g(:,:)
    integer :: i
    complex :: dft

    real :: max_freq, max_magnitude

    ! Setup the test wave
    amplitude = 1.0
    freq_hz = 12.0
    sample_rate_hz = 64
    dc_component = 42.0

    df = sample_rate_hz / imax
    dt = 1.0 / sample_rate_hz
    
    allocate(w(2*incw, kmax), g(incg, kmax))

    t = 0.0
    do i = 1, imax
       cosine = amplitude * cos(2.0 * pi * freq_hz * t) + dc_component
       g(i,1) = cosine
       t = t + dt
    end do

    ! Initialize spfft by passing 0 to IDIR
    call spffte(imax, incw, incg, kmax, w, g, 0, afft)
    ! Calculate FFT
    call spffte(imax, incw, incg, kmax, w, g, IDIR_R2C, afft)

    max_magnitude = 0.0
    do i = 2, imax, 2
       ! Turn real array into complex numbers
       dft = complex(w(i,1), w(i+1,1))
       magnitude = abs(dft)
       f = i * df / 2
       if (magnitude > max_magnitude) then
          max_freq = f
          max_magnitude = magnitude
       end if
    end do

    ! The frequency bin with the largest amplitude should match the test wave
    if (abs(max_freq - freq_hz) > 0.05) then
       error stop "FFT frequency does not match test wave"
    end if

    ! The first component of the FFT is the DC component of the wave
    if (abs(w(1,1) - dc_component) > 0.05) then
       error stop "DC component of FFT does not match test wave"
    end if

  end subroutine test_fft_real_to_complex

  subroutine test_fft_complex_to_real()
    real(real64) :: AFFT(50000+4*IMAX)
    real(precision), allocatable :: w(:,:), g(:,:), g_new(:,:)
    integer :: i

    allocate(w(2*incw, kmax), g(incg, kmax), g_new(incg, kmax))

    do i = 1, imax
       g(i,1) = i
    end do

    ! Initialize spfft by passing 0 to IDIR
    call spffte(imax, incw, incg, kmax, w, g, 0, afft)
    ! Calculate FFT
    call spffte(imax, incw, incg, kmax, w, g, IDIR_R2C, afft)
    ! Calculate inverse on to get original 'g' back
    call spffte(imax, incw, incg, kmax, w, g_new, IDIR_C2R, afft)

    do i = 1, imax
       if (abs(g(i,1) - g_new(i,1)) > 0.05) then
          error stop "Inverse FFT failed"
       end if
    end do

  end subroutine test_fft_complex_to_real
  
end program test_fft
