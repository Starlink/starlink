      subroutine arrow(x1,x2,y1,y2)
*+
* Name:
*   ARROW

* Purpose:
*   Draw an arrow

* Invocation:
*   CALL ARROW(X1,X2,Y1,Y2)

* Description:
*   An arrow is drawn from x1,y1 to x2,y2.

* Arguments:
*   X1 = REAL (Given)
*     X coordinate of tail end of arrow
*   Y1 = REAL (Given)
*     Y coordinate of tail end of arrow
*   X2 = REAL (Given)
*     X coordinate of head end of arrow
*   Y2 = REAL (Given)
*     Y coordinate of head end of arrow

* Authors:
*   TNW: T.N.Wilkins, Durham

* History:
*   10-MAR-1994 TNW:
*     Original version
*-
      implicit none
      real x1,x2,y1,y2,xx,yy,xx1,yy1,x(7),y(7)
      xx = 0.4*(x2 - x1)
      xx1 = 0.25*xx
      yy = 0.4*(y2 - y1)
      yy1 = 0.25*yy

      x(1) = x1 + yy1
      y(1) = y1 - xx1

      x(2) = x1 - yy1
      y(2) = y1 + xx1

      x(3) = x2 - xx - yy1
      y(3) = y2 - yy + xx1

      x(4) = x2 - xx - yy
      y(4) = y2 - yy + xx

      x(5) = x2
      y(5) = y2

      x(6) = x2 - xx + yy
      y(6) = y2 - yy - xx

      x(7) = x2 - xx + yy1
      y(7) = y2 - yy - xx1

* Draw arrow using solid fill area style

      call pgsfs(1)
      call pgpoly(7,x,y)

      end
