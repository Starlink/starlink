      subroutine plot_cut(xplot,yplot,ypref,yplt2,in,ncut,
     :    direction,xlabel)
*+
* Name:
*    PLOT_CUT

* Invocation:
*    CALL PLOT_CUT(XPLOT,YPLOT,YPREF,YPLT2,IN,NCUT,
*         DIRECTION,XLABEL)

* Purpose:
*  Plot cut through data

* Description:
*   Previous extracted data is plotted, corrected and uncorrected

* Arguments:
*    XPLOT(IN) = REAL ARRAY (Given)
*
*    YPLOT(IN) = REAL ARRAY (Given)
*
*    YPREF(IN) = REAL ARRAY (Given)
*
*    YPLT2(IN) = REAL ARRAY (Given)
*
*    IN = INTEGER (Given)
*
*    NCUT = INTEGER (Given)
*
*    DIRECTION = CHARACTER*(*) (Given)
*
*    XLABEL = CHARACTER*(*) (Given)
*
*
*-
      implicit none
      integer in
      real xplot(in),yplot(in),ypref(in),yplt2(in)
      integer ncut
*  ----------------------------------------------------------------
*  Labels for plot.

      real ymin,ymax
      character*(*) direction,xlabel
      integer status
      character*30 label
      character*5 cut
      if (ncut.ne.0) then
        write(cut,'(i5)') ncut
        label=direction//' '//cut
      else
        label=direction
      end if
      call gr_range(ypref,1,in,ymin,ymax,status)
      call pgenv(xplot(1),xplot(in),ymin,ymax,0.0,0)

*  Design graph & draw axis, scaling on YPREF, the value in the
* reference channel, which should be higher than elsewhere.

      call pglabel(xlabel,' ',label)
      call pgbin(in,xplot,ypref,.true.)
      call gr_spen(2)
      call pgbin(in,xplot,yplot,.true.)
      call gr_spen(3)
      call pgbin(in,xplot,yplt2,.true.)
      call gr_spen(1)
      end
