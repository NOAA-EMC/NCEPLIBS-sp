module spfft_mod
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env
  implicit none

  ! FFT direction: real-to-complex, and complex-to-real
  integer, public, parameter :: SPFFT_R2C = 0
  integer, public, parameter :: SPFFT_C2R = 1

  private
  public spfft

  interface spfft
     module procedure spfft_1d
     module procedure spfft_2d
  end interface spfft
  
contains

  subroutine spfft_1d(g, w, fft_dir, fft_plan_in)
    include 'fftw3.f03'
    real(real64), intent(inout) :: g(:)
    complex(real64), intent(inout) :: w(:)
    integer, intent(in) :: fft_dir
    type(c_ptr), optional :: fft_plan_in
    integer :: n
    type(c_ptr) :: fft_plan

    if (n /= size(g)) then
       error stop "given number of samples, n, does not match array size"
    end if

    ! fftw returns the complex FFT array of size n / 2 + 1, omitting the (redundant) negative frequencies
    if (size(w) /= (size(g) / 2 + 1)) then
       print *, size(w), size(g)
       error stop "real and complex array sizes are not compatible"
    end if

    if (present(fft_plan_in)) then
       fft_plan = fft_plan_in
    end if

    ! Create fft_plan if fft_plan is not given
    select case(fft_dir)
    case(SPFFT_C2R)
       fft_plan = fftw_plan_dft_c2r_1d(n, w, g, FFTW_ESTIMATE)
    case(SPFFT_R2C)
       fft_plan = fftw_plan_dft_r2c_1d(n, g, w, FFTW_ESTIMATE)
    case default
       error stop "Not a supported fft_dir"
    end select

    select case(fft_dir)
    case(SPFFT_C2R)
       call fftw_execute_dft_c2r(fft_plan, w, g)
    case(SPFFT_R2C)
       call fftw_execute_dft_r2c(fft_plan, g, w)
    case default
       error stop "Not a supported fft_dir"
    end select

  end subroutine spfft_1d

  subroutine spfft_2d(g, w, fft_dir, fft_plan_in)
    include 'fftw3.f03'
    real(real64), intent(inout) :: g(:,:)
    complex(real64), intent(inout) :: w(:,:)
    integer, intent(in) :: fft_dir
    type(c_ptr), optional :: fft_plan_in

    integer :: i, n

    do i = 1, size(g, 2)
       call spfft_1d(g(:,i), w(:,i), fft_dir, fft_plan_in)
    end do
    
    
  end subroutine spfft_2d

end module spfft_mod

