      subroutine plot_line(x, y, n, colind)
*
*    Draw a polyline in the specified colour

      implicit none
      integer  n, x(n), y(n), colind, i
      real xr(1000), yr(1000)

      do i = 1,n
         xr(i) = float(x(i))
         yr(i) = float(y(i))
      enddo

      call PGSCI(colind)
      call PGLINE(n, xr, yr)
      end
