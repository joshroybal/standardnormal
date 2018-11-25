! program computes standard normal distribution and displays report
program normal_distribution
   use stat_module
   implicit none
   ! interface definitions
   interface
      ! subroutine computes canonical statistics whilst avoiding repetition
      subroutine compute(x, n, m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw)
         ! dummy arguments
         integer, intent(in) :: n
         real, intent(in), dimension(n) :: x
         real, intent(out) :: m, pv, ps, sv, ss, mdn, mad, aad, lo, hi, skw
      end subroutine compute
      ! subroutine emits standard normal distribution figures and graph
      subroutine report(x, n, m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw)
         ! dummy arguments
         integer, intent(in) :: n
         real, intent(in), dimension(n) :: x
         real, intent(in) :: m, pv, ps, sv, ss, mdn, mad, aad, lo, hi, skw
      end subroutine report
   end interface
   ! variable declarations
   integer :: n
   real :: m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw, v
   real, dimension(:), allocatable :: x, y
   ! processing
   n = 1000
   allocate (x(n))
   call normal(n, x)
   call compute(x, n, m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw)
   call report(x, n, m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw)
   deallocate (x)
   n = 1000000
   allocate (y(n))
   call normal(n, y)
   call compute(y, n, m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw)
   call report(y, n, m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw)
   deallocate (y)
   stop
end program normal_distribution
