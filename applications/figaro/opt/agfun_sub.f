      subroutine agfun_sub(mpts,n,data,fc,dens,xc,gc,gc1,weight,start,
     :                  end,ngauss,the_sign)
*+
* Name:
*    AGFUN_SUB

* Invocation:
*    CALL AGFUN_SUB(MPTS,N,DATA,FC,DENS,XC,GC,GC1,WEIGHT,START,
*                       END,NGAUSS,THE_SIGN)

* Purpose:
*   Fit a single gaussian to all lines specified
*   simultaneously.

* Description:
*   To perform the functions of AGFUN which require direct access to
*   work arrays. This is to allow these to be passed as pointers in
*   common.
*
* Arguments:
*    MPTS = INTEGER (Given)
*        Number of points
*    DATA(MPTS) = DOUBLE PRECISION ARRAY (Given)
*        X array data
*    FC = DOUBLE PRECISION (Given)
*        Sum of Squares
*    DENS(MPTS) = DOUBLE PRECISION ARRAY (Given)
*        Y array (intensity) data
*    XC(N) = DOUBLE PRECISION ARRAY (Given)
*        Fit parametes
*    GC(N) = DOUBLE PRECISION ARRAY (Given)
*        Derivatives
*    GC1(N) = DOUBLE PRECISION ARRAY (Given)
*        Dervatives
*    WEIGHT(MPTS) = DOUBLE PRECISION ARRAY (Given)
*        Weights on points
* Subroutine called:
*    COPD2D          : Fast copy of data
* History:
*      T.N.Wilkins 23/11/88
*      TNW 5/12/88 Altered to use GEN_MOVE, later changed to COPD2D
*      DJA created from modification of mgfun_sub
*      TIMJ 15/8/05 Force do loop to use integeres
*-

      implicit none
      integer n,mpts
      integer ngauss
      integer the_sign
      double precision data(mpts),dens(mpts),xc(n),gc(n),gc1(n)
      double precision fc, weight(mpts)
      double precision start(ngauss), end(ngauss)
* Symbolic constants
      integer REDSHIFT
      integer WIDTH
      integer SHARED_OFFSET
      integer PARS_PER_G
      Parameter (WIDTH = 2)
      Parameter (REDSHIFT = 1)
      parameter (PARS_PER_G = 2)
      Parameter (SHARED_OFFSET = 2)

* local variables

      integer  i, k, i_base, i_height
      double precision rc, current_gaussian
      double precision red_mult, w_sq, g_exp
      double precision width_mult, z_score2, z_score

* Added to make it compile

      double precision g_redshift,g_width

* zero the residual sum of squares for current pass

      fc = 0.0d0

* main loop over gaussians and data points calculating
* fc and gc for each free parameter.

* gsum will contain sum of squares at current ith data point.

* loop over the gaussians

      do k = 1, ngauss

* break the data up into its start and end locations
* we need to do this because we must accumualte seperate
* derviatives for each HEIGHT and BASE

        do i = INT(start(k)) , INT(end(k))

          w_sq       = weight(i) * weight(i)
          z_score    = ( data(i) - xc(REDSHIFT) ) / xc(WIDTH)
          Z_score2   = - 0.5d0 * z_score * z_score
          g_exp      = exp ( z_score2 )

* work out which paramters corrspond to the HEIGHT and BASE
* of this particular line

          i_base    = (k-1) * PARS_PER_G + SHARED_OFFSET + 1
          i_height  = i_base + 1

* form total gaussian for this line

          current_gaussian    = abs(xc(i_height)) * g_exp
          current_gaussian    = the_sign * current_gaussian

* the residuals

          rc  = current_gaussian - dens(i) + xc( i_base )

* sum of squares

          fc = fc + w_sq * rc * rc

* form the gradient vector appropriate for least squares
* first we do the BASE and HEIGHT as these are different
* for EACH gaussian in this model.

*   base
          gc(i_base) = 2.0d0 * rc * w_sq

*  Height
          gc(i_height) =  gc(i_base) * g_exp

* derivative mulitiplication factor for REDSHIFT
* and redshift.

          red_mult   = z_score / xc(WIDTH)
          width_mult = red_mult * z_score

* we can now work out the contributions made by this line to
* the derivatives of REDSHIFT and WIDTH which are the same
* for each gaussian. We thus have to sum over gaussians
* to obtain the total partial derivatives.

* first get contribution from this gaussian

          g_REDSHIFT   = current_gaussian * red_mult
          g_WIDTH      = g_REDSHIFT * z_score

* and combine with the contributions from previous guassians

          gc(REDSHIFT) = gc(REDSHIFT) + gc(i_base) * g_REDSHIFT
          gc(WIDTH)    = gc(WIDTH)    + gc(i_base) * g_WIDTH

        end do

      end do

      call copd2d(n,gc,gc1)

      end
