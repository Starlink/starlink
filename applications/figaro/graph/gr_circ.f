      subroutine gr_circ(x,y,size)
*+
* Name:
*    GR_CIRC

* Invocation:
*    CALL GR_CIRC(X,Y,SIZE)

* Description:
*  To plot a filled marker of given size at a given position.
*
* Purpose:
*  To plot a filled marker of given size at a given position.
*    A 20-sided polygon is used to approximate to a circle.
*
* Arguments:
*      X = REAL (Given)
*        X position (world coords)
*      Y = REAL (Given)
*        Y position (world coords)
*      SIZE = REAL (Given)
*        Marker size (relative units)
* Subroutines/functions referenced:

* History:
*   T.N.Wilkins, Cambridge,  5-APR-1990
*        "           "      25-MAR-1991, Size obtained slightly
*                           differently
*-
      implicit none
      real x
      real y
      real size

*

      real vx1,vx2,vy1,vy2,asp1,asp2,xpts(20),ypts(20),ratio,xsize,ysize
      real ang,incr,wx1,wx2,wy1,wy2
      parameter (incr = 0.314159265)
      integer i

* Inquire the limits of the display, so that, even though we are
* plotting in world coordinates, we end up with a circle

      call pgqvp(2,vx1,vx2,vy1,vy2)
      asp1 = (vy2-vy1)/(vx2-vx1)
      call pgqwin(wx1,wx2,wy1,wy2)
      asp2 = (wy2-wy1)/(wx2-wx1)
      ratio = asp2/asp1
      xsize = (wx2-wx1)*size
      ysize = xsize*ratio
      do i = 1, 20
        ang = real(i)*incr
        xpts(i) = x + cos(ang)*xsize
        ypts(i) = y + sin(ang)*ysize
      end do
      call pgpoly(20,xpts,ypts)
      end
