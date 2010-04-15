	subroutine mapgl_fillsdf2( xdim, ydim, sarr, nx, ny, arr, status)

        implicit none

	integer
     :	  nx,
     :	  ny,
     :	  xdim,
     :	  ydim,
     :	  status,
     :	  j,
     :	  k

	real
     :	  arr( nx, ny),
     :	  sarr( xdim, ydim)

	do j = 1, ydim
	  do k = 1, xdim
	    arr( k, j) = sarr( k, j)
	  end do
	end do

	end

