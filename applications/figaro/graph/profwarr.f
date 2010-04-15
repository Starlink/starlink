      subroutine profwarr(x1,y1,del,style)
*+
* Name:
*   PROFWARR

* Purpose:
*   Draw a profile in a box with arrows

* Invocation:
*   CALL PROFWARR(X1,Y1,DEL,STYLE)

* Description:
*   A profile is drawn in a box, the box being from x1,y1 to x1+del,y1
*   +del.

* Arguments:
*   X1 = REAL (Given)
*     X coordinate of lower left corner of box
*   Y1 = REAL (Given)
*     Y coordinate of lower left corner of box
*   DEL = REAL (Given)
*      Size of box
*   STYLE = INTEGER (Given)
*      Style of plot-where the arrows go:
*         - 1 - Up
*         - 2 - Down
*         - 3 - Left
*         - 4 - Right
*         - 5 - Inwards
*         - 6 - Outwards
*         - 7 - Up, but straight line (for base) rather than profile
*         - 8 - Down, but straight line (for base) rather than profile

* Authors:
*   TNW: T.N.Wilkins, Durham

* History:
*   10-MAR-1994 TNW:
*     Original version
*-
      implicit none
      real x1,y1,del
      integer style

      integer UP, DOWN, LEFT, RIGHT, IN, OUT, UPBS, DOWNBS,istyle, FIT,
     :     HELP
      parameter (UP = 1, DOWN = 2, LEFT = 3, RIGHT = 4, IN = 5, OUT = 6,
     :     UPBS = 7, DOWNBS = 8, FIT = 9, HELP = 10)
      real y,value

      istyle = style

* Draw profile in box

      if(style.lt.UPBS) then
         call profinbox(x1,y1,del)
      else if(style.eq.FIT) then
         call pgsfs(2)
         call pgqch(value)
         if(del.gt.0.08) call pgsch(2.0*value)
         call pgrect(x1,x1+del,y1,y1+del)
         call pgtext(x1+del*0.25,y1+del*0.25,'FIT')
         call pgsch(value)
      else if(style.eq.HELP) then
         call pgsfs(2)
         call pgqch(value)
         if(del.gt.0.08) call pgsch(2.0*value)
         call pgrect(x1,x1+del,y1,y1+del)
         call pgtext(x1+del*0.3,y1+del*0.25,'?')
         call pgsch(value)
      else
         istyle = style - UPBS + 1
         call pgsfs(2)
         call pgrect(x1,x1+del,y1,y1+del)
         call pgsls(1)
         call pgmove(x1,y1+del*0.5)
         call pgdraw(x1+del,y1+del*0.5)
         call pgsls(1)
      endif


* Draw arrow(s)

      if(istyle.eq.UP) then
         call arrow(x1+del*0.5,x1+del*0.5,y1+del*0.1,y1+del*0.9)
      else if(istyle.eq.DOWN) then
         call arrow(x1+del*0.5,x1+del*0.5,y1+del*0.9,y1+del*0.1)
      else
         y = y1+del*0.5
         if(istyle.eq.LEFT) then
            call arrow(x1+del*0.9,x1+del*0.1,y,y)
         else if(istyle.eq.RIGHT) then
            call arrow(x1+del*0.1,x1+del*0.9,y,y)
         else if(istyle.eq.IN) then
            call arrow(x1+del*0.95,x1+del*0.6,y,y)
            call arrow(x1+del*0.05,x1+del*0.4,y,y)
         else if(istyle.eq.OUT) then
            call arrow(x1+del*0.6,x1+del*0.95,y,y)
            call arrow(x1+del*0.4,x1+del*0.05,y,y)
         endif
      endif
      end
