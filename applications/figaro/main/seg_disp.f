      subroutine seg_disp(limits,x,y,in,cscale,xlabel,xunits,yunits,
     :     yvalue)
*+
* Name:
*    SEG_DISP

* Invocation:
*    CALL SEG_DISP(LIMITS,X,Y,IN,CSCALE,XLABEL,XUNITS,YUNITS,
*                 YVALUE)

* Purpose:
*    Display data in segments

* Description:
*    The data in X/Y is plotted in the X range LIMITS(1) to LIMITS(2).

* Arguments:
*     LIMITS(2) = REAL ARRAY (Given)
*        X limits for plotting
*     X(IN) = REAL ARRAY (Given)
*        X data array to be displayed
*     Y(IN) = REAL ARRAY (Given)
*        Y data array to be displayed
*     IN = INTEGER (Given)
*        Dimensions of arrays
*     CSCALE = LOGICAL (Given)
*        If to use YVALUE for scaling
*     XLABEL = CHARACTER*(*) (Given)
*        X label
*     XUNITS = CHARACTER*(*) (Given)
*        X units
*     YUNITS = CHARACTER*(*) (Given)
*        Y units
*     YVALUE = REAL (Given)
*        Maximum Y value for plotting

* Authors:
*  TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then Durham

* History:
*  TNW: Altered so as not to use ARCDIMS 27/5/88
*  TNW: PGPLOT version 3/90
*  TNW: 6/8/93 Whole option from segments now just another segment.
*  TNW: 10/8/93 Abandon pre-determined segmentation altogether, just
*       allow limits of plot to be set as required.
*-
      implicit none
      real yvalue
*
* integer
*
      integer is,ie,len1
      integer rx2chn
      integer status
*
* character
*
      character*(*) xlabel,xunits,yunits
      character*14 legend,chars*62
*
* logical
*
      logical cscale
*
* dimension
*
      integer in
      real limits(2),x(in),y(in)
      real ymin,ymax
*
* anotation for diagram
*
      status=0

      call chr_fill(' ',legend)
*
* set up x-axix limits
*
      is=rx2chn(x,in,limits(1))
      ie=rx2chn(x,in,limits(2))
      is = max(1,is)
      ie = min(in,ie)
      call gr_range(y,is,ie,ymin,ymax,status)
      if (cscale) ymax = yvalue
      call gr_spen(1)
      call pgenv(limits(1),limits(2),ymin,ymax,0,0)
      call pgbin(in,x,y,.true.)
      len1 = 0
      call chr_fill(' ',chars)
      call chr_appnd(xlabel,chars,len1)
      len1 = len1 + 2
      call chr_appnd(xunits,chars,len1)
      call pglabel(chars,yunits,legend)
      end
