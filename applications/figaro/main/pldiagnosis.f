      subroutine pldiagnosis(npts,xplot,yplot,x_err,y_err,title,
     :       xlab,ylab,error_bars,leg1,bars,ifsoft,parlim)
*+
* Name:
*    PLDIAGNOSIS

* Invocation:
*    CALL PLDIAGNOSIS(NPTS,XPLOT,YPLOT,X_ERR,Y_ERR,TITLE,
*            XLAB,YLAB,ERROR_BARS,LEG1,BARS,IFSOFT,PARLIM)

* Purpose:
*   To plot the diagnosis and errors where applicable.

* Description:
*   To plot the diagnosis and errors where applicable.
*
* Arguments-
*   NPTS = INTEGER (Given)
*        Number of points in data
*   XPLOT(NPTS) = REAL ARRAY (Given)
*        X data points
*   YPLOT(NPTS) = REAL ARRAY (Given)
*        Y data points
*   X_ERR(NPTS) = REAL ARRAY (Given)
*        errors on X data points
*   Y_ERR(NPTS) = REAL ARRAY (Given)
*        errors on Y data points
*   TITLE = CHARACTER (Given)
*        title for plot
*   XLAB = CHARACTER (Given)
*        X axis label
*   YLAB = CHARACTER (Given)
*        Y axis label
*   ERROR_BARS = LOGICAL (Given)
*        If error bars to be plotted
*   LEG1 = CHARACTER (Given)
*        Legend (2nd line of heading at top of plot)
*   BARS = CHARACTER (Given)
*        If this includes "X" then x error bars plotted,
*                    likewise for "Y".
*   IFSOFT = LOGICAL (Given)
*        If plot is in softcopy (as opposed to hardcopy),
*                    this also affects the quality of the plot.
*   PARLIM = LOGICAL (Given)
*        If to take the plot limits from the parameter PLOTLIM
*                    (this is only done if the parameter is actually given
*                    on the command line).

* Authors:
*   TNW: T.N.Wilkins. Manchester until 1/89, then Cambridge

* History:
*     TNW: Original version
*     TNW: Allow for no labels, 7/6/88
*     TNW: 30/11/90 Allow to "fix" range of plots.
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'gr_inc'
      integer status
      logical ifsoft
      character*(*) title,xlab,ylab
      logical error_bars,parlim
      integer npts
      real xplot(npts),yplot(npts)
      real x_err(npts),y_err(npts)
      character*(*) leg1
      character*(*) bars
      character*2 e_bars
      real xmin,xmax,ymin,ymax
      integer lenleg1
      character*80 leg1scr
      real lims(4)
      integer chr_len
      logical iflab,usplim,par_given

C     print4999, npts
C4999 format(1x, 'PLDIAGNOSIS, npts: ', i10)

      e_bars=bars

* Ensure graphics device is open, and copy legend into string for
* plotting. If PWRITX is to be used, insert codes for Greek letters
* if required

      status = SAI__OK
      call gr_selct(ifsoft,status)
      if(status.ne.SAI__OK) return
      call greek_letters(leg1,leg1scr,lenleg1,.false.)

      call chr_ucase(e_bars)
      call gr_spen(1)

C     print5000, parlim
C5000 format(1x, 'parmlim: ', l5)

      if(parlim) then
        usplim = par_given('plotlim')
      else
        usplim = .false.
      end if

C     print5001, usplim
C5001 format(1x, 'usplim: ', l5)

      if(usplim) then
        call par_rdary('plotlim',VAL__MINR,VAL__MAXR,' ',' ',4,4,lims)
        xmin = lims(1)
        xmax = lims(2)
        ymin = lims(3)
        ymax = lims(4)
      else
C       print5002, 'before gr_range'
C5002   format(1x, a)
        call gr_range(xplot,1,npts,xmin,xmax,status)
        call gr_range(yplot,1,npts,ymin,ymax,status)
      end if
      call pgenv(xmin,xmax,ymin,ymax,0,0)
      if(iflab()) then
        call pgmtext('T',1.0,0.5,0.5,leg1scr(:lenleg1))
      end if
C     print5002, 'before pglabel'
      call pglabel(xlab(:chr_len(xlab)),ylab(:chr_len(ylab)),
     :              title(:chr_len(title)))
*
* Mark each data point, but only if we don't have both of X and Y error
* bars (these would be enough to mark the points by themselves).
*
      if( ((e_bars.ne.'XY').and.(e_bars.ne.'YX'))
     :       .or.(.not.error_bars)) then
C       print5002, 'before pgpoint'
        call pgpoint(npts,xplot,yplot,2)
      endif
      if (error_bars) then
*
*  Put in error bars
*
C       print5002, 'before gr_ebar'
C       print5003, e_bars
C5003   format(1x, 'e_bars: ', a)
        call gr_ebar(e_bars,xplot,yplot,x_err,y_err,1.0,npts)
      end if
      end
