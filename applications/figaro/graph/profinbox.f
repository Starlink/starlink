      subroutine profinbox(x1,y1,del)
*+
* Name:
*   PROFINBOX

* Purpose:
*   Draw a profile in a box

* Invocation:
*   CALL PROFINBOX(X1,Y1,DEL)

* Description:
*   A profile is drawn in a box, the box being from x1,y1 to x1+del,y1
*   +del.

* Arguments:
*   X1 = REAL (Given)
*     X coordinate of lower left corner of box
*   Y1 = REAL (Given)
*     Y coordinate of lower left corner of box
*   DEL = REAL (Given)
*     Size of box

* Authors:
*   TNW: T.N.Wilkins, Durham

* History:
*   10-MAR-1994 TNW:
*     Original version
*-
      implicit none
      real x1,y1,del,ax(11),ay(11),ax1(11),ay1(11)
      integer i
      data ay/0.0,0.02,0.1,0.3,0.85,1.0,0.85,0.3,0.1,0.02,0.0/
      data ax/0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/

* Draw box

      call pgsfs(2)
      call pgrect(x1,x1+del,y1,y1+del)

* Draw profile, using a dotted line

      call pgsls(4)
      do i = 1, 11
         ax1(i) = ax(i)*del + x1
         ay1(i) = ay(i)*del + y1
      enddo
      call pgline(11,ax1,ay1)
      call pgsls(1)
      end
