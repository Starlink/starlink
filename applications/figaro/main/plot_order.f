      subroutine plot_order(in,ss)
*+
* Name:
*    PLOT_ORDER

* Invocation:
*    CALL PLOT_ORDER(IN,SS)

* Description:
*    Plots residual sum of squares against polynomial order
*   for chebyshev fit.

* Purpose:
*   Plots residual sum of squares against polynomial order
*   for chebyshev fit. The plot is made using the PGPLOT
*   graphics Library.

* Arguments:
*  IN = INTEGER (Given)
*       Number of Orders to be plotted
*  SS = DOUBLE PRECISION ARRAY (Given)
*       Residual Sum of Squares v Order
* History:
*   27/2/89 DJA Altered to pass work space from above
*   18/9/90 TNW Altered so as not to need workspace (made code above too
*   complicated, given that workspace no longer acceptably obtained in
*   SEEK_ORDER.
*
      implicit none
*
* import
*

* number of points [orders]

      integer in

* sum of squares for each order

      double precision ss(in)
*-
* local
*
      double precision ymin,ymax
      real rymin,rymax
      integer i
* ------------------------------------------------------------------
*

* Get range

      ymin = ss(1)
      ymax = ymin
      do i = 2, in
        ymax = max(ss(i),ymax)
        ymin = min(ss(i),ymin)
      end do

* Set x-axis limits.
*
      call gr_spen(1)
      call pgrnge(real(ymin),real(ymax),rymin,rymax)
      call pgenv(1.0,real(in),rymin,rymax,0,0)
*
* design graph and draw it
*
      call pglabel('Order','Sum-of-Squares',
     :     'Residual Sum-of-Squares V Order')
      call pgmove(1.0,real(ss(1)))
      do i=2,in
        call pgdraw(real(i),real(ss(i)))
      enddo
      end
