      subroutine rectmark(x,y,size)
*+
* Name:
*    RECTMARK

* Invocation:
*    CALL RECTMARK(X,Y,SIZE)
* Purpose:
*  To plot a filled hexagon
*
*  Description:
*  To plot a filled hexagon
*
*  Arguments:
*    X,Y = REAL (Given)
*        coordinates
*    SIZE = REAL (Given)
*        Radius
*
*  Subroutine referenced:
*    PGRECT

* Author:
*  T.N.Wilkins Manchester 4/88
*-
      implicit none
      real x,y,size

      call pgrect(x-size,x+size,y-size,y+size)
      end
