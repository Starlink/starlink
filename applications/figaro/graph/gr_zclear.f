      subroutine gr_zclear(xleft,xright,ybot,ytop)
*+
* Name:
*    GR_ZCLEAR

* Invocation:
*    CALL GR_ZCLEAR(XLEFT,XRIGHT,YBOT,YTOP)

* Purpose:
*   Clear a specfied rectangle on a PGPLOT graphic

* Description:
*   Clear a specfied rectangle on a PGPLOT graphic
*   The input coordiantes are the NDC of the region to be cleared.
*   Note that every thing is handled internally , and so on
*   exit everytihng SHOULD be as it was when the routine was called.!
*- ----------------------------------------------------------------
      implicit none
      real Xmin
      real Xmax
      real Ymin
      real Ymax
      real Xleft
      real Xright
      real Ybot
      real Ytop
      integer ci
      integer fs
      real x1,x2,y1,y2
      integer NORMALIZED_DEVICE
      PARAMETER ( NORMALIZED_DEVICE = 0)

*
* inquire the colour index and then set to 0 so we an overwrite the plot
*
      call pgqci(ci)
      call pgsci(0)

*
* inquire the fill area index and set fill area style to solid
*
      call pgqfs(fs)
      call pgsfs(1)

* set the size and position of the viewport
* to be cleared. First enquire for the current viewport
* settings which we resotre afterwards.

      call pgqvp(NORMALIZED_DEVICE,x1,x2,y1,y2)

* inquire fro the WORLD coordinates tm match this area of the screen

      call pgqwin(xmin,xmax,ymin,ymax)

* NOW set the region to be cleared

      call pgvport(xleft,xright,ybot,ytop)
*
* set the world coordiantes of the rectangle to be cleared
* to be the NDC for this viewport.
*
      call pgwindow(xleft,xright,ytop,ybot)
*
* draw a rectangle using the current fill area arributes
* Since we hve set the pen to ERASE and the FILL to SOLID
* this effectively clears this portion of the screen

      call pgrect(xleft,xright,ybot,ytop)
*
* force a complete screen update
*
      call pgupdt
*
* return the collour index to its previous value
* and resotore the original coordinates and viewport
*
      call pgsci(ci)
      call pgsfs(fs)
      call pgvport(x1,x2,y1,y2)
      call pgwindow(xmin,xmax,ymin,ymax)
      end
