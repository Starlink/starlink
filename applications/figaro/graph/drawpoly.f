      subroutine drawpoly(x,y,n,datmin,datmax)
*+
* Name:
*    DRAWPOLY

* Invocation:
*    CALL DRAWPOLY(X,Y,N,DATMIN,DATMAX)

* Description:
*  To draw a line joining the X and Y values supplied, in a plot
* Purpose:
*  To draw a line joining the X and Y values supplied, in a plot
* with maximum and minimum supplied.

*  Arguments:
*    N = INTEGER (Given)
*        Number of points
*    X(N) = REAL ARRAY (Given)
*        X array
*    Y(N) = REAL ARRAY (Given)
*        Y array
*    DATMIN = REAL (Given)
*        Minimum Y data value
*    DATMAX = REAL (Given)
*        Maximum Y data value
* (DATMAX and DATMIN are used for setting up the zone window, and do
* not have to actually match the data passed).

* Author:
*   T.N.Wilkins Manchester
*-
      implicit none
      integer n
      real x(n),y(n)
      real datmin,datmax
      call pgwindow(x(1),x(n),datmin,datmax)
      call pgline(n,x,y)
      end
