      subroutine hexmark(x,y,size)
*+
* Name:
*    HEXMARK

* Invocation:
*    CALL HEXMARK(X,Y,SIZE)
* Purpose:
*  To plot a filled hexagon
*
* Description:
*  To plot a filled hexagon
*
* Arguments:
*    X,Y = REAL (Given)
*        coordinates
*    SIZE = REAL (Given)
*        Radius
*
* Subroutine referenced:
*    GFA = INTEGER (Given)
*        Fill area

* Author:
*  T.N.Wilkins Manchester 4/88
*-
      implicit none
      integer i
      real x1(6),y1(6),x,y,size,costhe(6),sinthe(6)
      data costhe,sinthe/0.86603,0.0,-0.86603,-0.86603,0.0,0.86603,0.5,
     :  1.0,0.5,-0.5,-1.0,-0.5/
      do i = 1,6
        x1(i)=size*costhe(i)+x
        y1(i)=size*sinthe(i)+y
      end do
      call pgpoly(6,x1,y1)
      end
