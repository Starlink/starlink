      subroutine disp_window2(xws,xwe,x,y,in,xunits,small)
*+
* Name:
*    DISP_WINDOW2

* Invocation:
*    CALL DISP_WINDOW2(XWS,XWE,X,Y,IN,XUNITS,SMALL)

* Purpose:
*   Display the current line profile window

* Description:
*   This is for use when the residuals are also to be displayed.
*   The plot is auto-scaled, and has the xunits marked.

* Arguments:
*   XWS = REAL (Given)
*        Start of window to display (in units of X array)
*   XWE = REAL (Given)
*        End of window to display (in units of X array)
*   IN = INTEGER (Given)
*        Dimension of arrays
*   X(IN) = REAL ARRAY (Given)
*        X data array
*   Y(IN) = REAL ARRAY (Given)
*        Data array to be displayed
*   XUNITS = CHARACTER*(*) (Given)
*        X units for plotting
*   SMALL = LOGICAL (Given)
*        If plot is small (and therefore can't fit as many
*                    divisions of the X axis)

* History:
*        T.N.Wilkins Manchester
*    Altered so as not to use ARCDIMS, TNW 27/5/88
*    PGPLOT version, TNW/Cambridge 3/90
*    Combined so as to replace display_window3, TNW 11/4/90
*-
*___________________________________________________________________
      implicit none
      integer in
      real xws,xwe
      real x(in)
      real y(in)
      integer rx2chn,is,ie
      logical small
      character xunits*(*)
      real ymin,ymax,xint,xtest,xrange,xtest2,pgrnd
      integer nsub,status

      call gr_spen(1)
      is=rx2chn(x,in,xws)
      ie=rx2chn(x,in,xwe)
      call gr_range(y,is,ie,ymin,ymax,status)

* Make X tick intervals larger for small plots

      if(small) then
        xrange = xwe-xws
        xtest = xrange*0.2
        xtest2 = 10.0
        do while(xtest2.gt.3.0)
          xint = pgrnd(xtest,nsub)
          xtest2 = xrange/xint
          xtest = xtest*1.03
        end do
      else
        xint = 0.0
        nsub = 0
      end if
      call pgwindow(xws,xwe,ymin,ymax)
      call pgbox('BCNST',xint,nsub,'BCNST',0.0,0)
      call pglabel(xunits,' ',' ')

* Plot histogram-type line

      call pgbin(ie-is+1,x(is),y(is),.true.)
      end
