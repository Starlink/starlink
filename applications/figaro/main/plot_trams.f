      subroutine plot_trams(npts,ntrams,x,y,ynew,start,end,plotX,ploty,
     :           plotnew,label,labelx,labely,xmin,xmax,ymin,ymax,nplots)
*+
* Name:
*    PLOT_TRAMS

* Invocation:
*    CALL PLOT_TRAMS(NPTS,NTRAMS,X,Y,YNEW,START,END,PLOTX,PLOTY,
*                PLOTNEW,LABEL,LABELX,LABELY,XMIN,XMAX,YMIN,YMAX,NPLOTS)

* Purpose:
*   For spline routines
* Description:
*   For spline routines

* History:
*   19/12/00 (ACD): Corrected the calling arguments for SPLINEXY_PLOT.

      implicit none
      integer ntrams
      integer npts
      integer nplots
      real start(ntrams)
      real end(ntrams)
      real PLotX(npts)
      real PLotY(npts)
      real PLotnew(npts)
      real x(npts)
      real ynew(npts)
      real y(npts)
      real xmin(nplots)
      real xmax(nplots)
      real ymin(nplots)
      real ymax(nplots)
      character*(*) label,labelx,labely
*-
* local
      integer i,istart,iend
      integer nin,curpos
      integer RIGHT
      parameter (RIGHT = 3)
      integer rx2chn
      do i = 1,ntrams

* start at first point and assigning number of points not excluded
* by trams to 0.

        nin    = 0

* for each point in Y loop over the TRAMS checking if the point is EXCLUDED
* since it is inside a TRAM.The loop continues unitl all TRAMS have been
* tested or the point is EXCLUDED. By doing it this way we do not need to
* worry if the TRAMS have been sorted.

        istart = rx2chn(x,npts,start(i))
        iend   = rx2chn(x,npts,end(i))
        istart = max(1,min(npts,istart))
        iend   = max(1,min(npts,iend))

* loop over the input number of channels testing each point as we go
* Remove from the work arrays all entries within the boundaries of the
* lines, shifting the array entries as required.

        do curpos = 1,npts

* if the point is within the current TRAM then set EXCLUDED = .TRUE.
* and terminate the loop

          if(curpos.ge.istart.and.curpos.le.iend) then
            nin = nin + 1
            plotX(nin) = X(curpos)
            plotY(nin) = Y(curpos)
            plotNew(nin) = Ynew(curpos)
          end if

        end do
        call splinexy_plot(nin,plotX,plotY,PlotNew,
     :                               ' ',labelX,labelY,RIGHT,xmin,
     :                               xmax,ymin,ymax,NPLOTS)
      end do  ! NTRAMS
      end
