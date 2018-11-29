! subroutine emits standard normal distribution figures and graph
subroutine report(x, n, m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw)
   use stat_module
   implicit none
   ! dummy arguments
   integer, intent(in) :: n
   real, intent(in), dimension(n) :: x
   real, intent(in) :: m, pv, ps, sv, ss, mdn, mad, aad, lo, hi, skw
   ! local variables and arrays
   integer, parameter :: NS = 30
   integer, dimension(NS) :: slices
   integer :: i, j, idx, smax
   real :: v, xmax, xmin, span, interval_length
   ! processing
   xmin = m - 3.0 * ss
   xmax = m + 3.0 * ss
   span = xmax - xmin
   interval_length = span / NS
   slices = 0
   do i = 1, n
      v = x(i) - xmin
      if (v >= 0 .and. v <= span) then
         idx = int(v / interval_length) + 1
         slices(idx) = slices(idx) + 1
      end if
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
   smax = quick_select(NS, NS, slices)
   do i = 1, NS
     v = real(slices(i)) / real(smax)
     idx = NS * v
     write (*,1000) xmin + interval_length * (i - 1), ('=', j = 1, idx)
   end do
   return
   1000 format (F5.1,1X,30A1)
end subroutine report
