! subroutine emits standard normal distribution figures and graph
subroutine report(x, n, m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw)
   use stat_module
   implicit none
   ! dummy arguments
   integer, intent(in) :: n
   real, intent(in), dimension(n) :: x
   real, intent(in) :: m, pv, ps, sv, ss, mdn, mad, aad, lo, hi, skw
   ! local variables and arrays
   integer, parameter :: NS = 31
   real, dimension(NS) :: slices
   integer :: i, j, idx
   real :: v, xmax
   ! processing
   slices = 0.0
   do i = 1, n
      if (x(i) < 0) then
         idx = 15 + int(5 * x(i))
      else
         idx = 16 + int(5 * x(i))
      end if
      if (idx >= 1 .and. idx <= NS) slices(idx) = slices(idx) + 1
   end do
   ! output   
   write (*,*) 'n = ', n
   write (*,*) 'mean = ', m
   write (*,*) 'population variance = ', pv
   write (*,*) 'population standard deviation = ', ps
   write (*,*) 'sample variance = ', sv
   write (*,*) 'sample standard deviation = ', ss
   write (*,*) 'minimum = ', lo
   write (*,*) 'maximum = ', hi
   write (*,*) 'median = ', mdn
   write (*,*) 'median deviation = ', mad
   write (*,*) 'mean deviation = ', aad
   write (*,*) 'mean skewness = ', skw
   xmax = quick_select(NS, NS, slices)
   do i = 1, NS
     v = slices(i) / xmax
     idx = NS * v
     write (*,1000) -3.0+0.2*(i-1), ('=',j=1,idx)
   end do
   return
   1000 format (' ',F4.1,1X,72A1)
end subroutine report
