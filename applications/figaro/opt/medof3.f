      subroutine medof3( x1 , x2 , x3 , xmed , CHANGE)
*+
* Name:
*    MEDOF3

* Invocation:
*    CALL MEDOF3( X1 , X2 , X3 , XMED , CHANGE)

* Purpose:
*   Put the median of x1 ,x2 ,x3 in xmed and set CHANGE .TRUE. if the
*   median is NOT x2.

* Description:
*   Put the median of x1 ,x2 ,x3 in xmed and set CHANGE .TRUE. if the
*   median is NOT x2.

      implicit none
      real x1 , x2, x3 , xmed
      logical CHANGE
*-
      real y1 , y2 , y3


      Y1 = x1
      Y2 = x2
      y3 = x3

      xmed = y2

      if ( ((y2 - y1 ) * ( y3 - y2 ) ).lt. 0.0 ) then
        CHANGE = .TRUE.
        xmed = y1
        if (((y3 - y1 ) * ( y3 - y2 )) .le. 0.0 ) then
          xmed = y3
        end if
      end if

      end
