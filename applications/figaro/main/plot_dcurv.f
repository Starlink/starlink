      subroutine plot_dcurv(x,y,npts)
*+
* Name:
*    PLOT_DCURV

* Invocation:
*    CALL PLOT_DCURV(X,Y,NPTS)

* Purpose:
*   Plot curve with double precision data

* Description:
*   Plot a curve on a Graph which has already be drawn Using PGPLOT.
*   This routine takes as its input Double Precision arrays fro X and Y
*   of the sort produced from Chebyshev Polynomial fits using NAG.
*   these are converted to real for plotting.

* Arguments:
*  X(NPTS) = DOUBLE PRECISION ARRAY (Given)
*      X coordiantes of the curve to be plotted
*  Y(NPTS) = DOUBLE PRECISION ARRAY (Given)
*      Y coordiantes of the curve to be plotted
*  NPTS = INTEGER (Given)
*      Number of points to be plotted

* History:
*  TNW/CAVAD 2/4/90 Change to drawing lines using pgmove/pgdraw
*     "      26/4/91 Renamed from plot_fit2 to plot_dcurv
*- ------------------------------------------------------------------
      implicit none
* import
*
      integer npts
      double precision x(npts)
      double precision y(npts)
*
* local
*
      integer i
*

* plot the line

      call gr_spen(2)
      call pgbbuf
      call pgmove(real(x(1)),real(y(1)))
      do i = 1, npts
        call pgdraw(real(x(i)),real(y(i)))
      end do
      call pgebuf
      call gr_spen(1)
      end
