      subroutine gr_vline(x)
*+
* Name:
*    GR_VLINE

* Invocation:
*    CALL GR_VLINE(X)

* Purpose:
*  To plot a vertical line at the point X.
*
* Description:
*  To plot a vertical line at the point X.
*
* Arguments:
*   X = REAL (Given)
*      X position
* Subroutines/functions referenced:
*      GR_POLYL         : Draw polyline
*      SNX_AGGUY        : Convert between coord systems

* Authors:
*   T.N.Wilkins, Cambridge,  5-DEC-1989
*-
      implicit none
      real x

*

      real ypos(2),xpos(2),d1,d2

      xpos(1) = x
      xpos(2) = x
      call pgqwin(d1,d2,ypos(1),ypos(2))
      call pgline(2,xpos,ypos)
      end
