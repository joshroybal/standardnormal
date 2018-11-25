! Modern Fortran statistical computations module

module stat_module
contains
   ! function returns real mean of all array elements
   function mean(x, n) result(s)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: s
      ! local variables
      integer :: i
      ! processing
      s = 0
      do i = 1, n
         s = ((i - 1) * s + x(i)) / i
      end do
   end function mean

   ! function returns real population variance of x
   function pop_var(x, n) result(s)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: s
      ! local variables
      integer :: i
      real :: m
      ! processing
      m = mean(x, n)
      s = 0
      do i = 1, n
         s = (((i - 1) * s) + (x(i) - m)**2) / i
      end do
   end function pop_var

   ! function returns real sample variance of x
   function sam_var(x, n) result(s)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: s
      s = (real(n) / (n-1)) * pop_var(x, n)
   end function sam_var

   ! function returns real population standard deviation of x
   function pop_std(x, n) result(s)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: s
      ! processing
      s = sqrt(pop_var(x, n))
   end function pop_std

   ! function returns real sample standard deviation of x
   function sam_std(x, n) result(s)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: s
      ! processing
      s = sqrt(sam_var(x, n))
   end function sam_std

   ! functions returns index of kth element of x
   ! C. A. R. Hoare's algorithm
   ! implementation works on index array only - preserves data set array
   function quick_select(k, n, x) result(v)
      implicit none
      ! dummy arguments
      integer, intent(in) :: k, n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: v
      ! local variables
      integer :: i, j, left, right, tmp
      integer, dimension(n) :: idx
      real :: pivot
      ! processing
      do i = 1, n
         idx(i) = i
      end do
      left = 1
      right = n
      do while (left < right)
         pivot = x(idx(k))
         i = left
         j = right
         do
            do while (x(idx(i)) < pivot)
               i = i + 1
            end do
            do while (pivot < x(idx(j)))
               j = j - 1
            end do
            if (i <= j) then
               tmp = idx(i)
               idx(i) = idx(j)
               idx(j) = tmp
               i = i + 1
               j = j - 1
            end if
            if (i > j) exit
         end do
         if (j < k) left = i
         if (k < i) right = j
      end do
      v = x(idx(k))
   end function quick_select

   ! function returns median of x
   function median(x, n) result(mdn)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: mdn
      ! processing
      if (mod(n, 2) == 1) then
         mdn = quick_select(n / 2 + 1, n, x)
      else
         mdn = (quick_select(n / 2, n, x) + quick_select(n / 2 + 1, n, x)) / 2
      end if
   end function median

   !funcition returns median absolute deviation of x
   function mdn_abs_dev(x, n) result(mad)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: mad
      ! local variables
      integer :: i
      real :: m
      real, dimension(n) :: y
      ! processing
      m = median(x, n)
      do i=1,n
         y(i) = abs(m - x(i))
      end do
      mad = median(y, n)
   end function mdn_abs_dev

   ! function computes the skewness of x (expected value 1/N)
   function skewness(x, n) result(skw)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(in), dimension(n) :: x
      ! function return location
      real :: skw
      ! local variables
      integer :: i
      real :: numerator, m, s
      ! processing
      m = mean(x, n)
      s = 0
      do i = 1, n
         s = (((i - 1) * s) + (x(i) - m)**3) / i
      end do
      skw = s / pop_std(x, n)**3
   end function skewness

   ! function computes the mean absolute deviation of x
   function mean_deviation(x, n) result(aad)
      implicit none
      ! dummy arguments
      integer :: n
      real, intent(in), dimension(n) :: x
      ! local variables
      integer :: i
      real :: m, s
      ! function return location
      real :: aad
      ! processing
      m = mean(x, n)
      s = 0
      do i = 1, n
         aad = (((i - 1) * aad) + abs(x(i) - m)) / i
      end do
   end function mean_deviation

   ! generate a normal distribution on Linux
   subroutine normal(n, x)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      real, intent(out), dimension(n) :: x
      ! local variables
      integer :: i, s, clock
      real :: pi, temp, mean = 0.0, sd = 1.0
      integer, dimension(:), allocatable :: seed
      ! processing
      call random_seed(size = s)
      allocate(seed(s))
      call system_clock(count = clock)
      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      call random_seed(put = seed)
      deallocate(seed)
      call random_number(x) ! uniform distribution
      ! now convert to normal distribution
      pi = 4.0 * atan(1.0)
      do i = 1, n - 1, 2
         temp = sd * sqrt(-2.0*log(x(i))) * cos(2.0*pi*x(i+1)) + mean
         x(i+1) = sd * sqrt(-2.0*log(x(i))) * sin(2.0*pi*x(i+1)) + mean
         x(i) = temp
      end do
   end subroutine normal
end module stat_module
