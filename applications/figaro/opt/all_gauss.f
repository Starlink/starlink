      subroutine all_gauss(m,n,xc,rc,ajc,ajtjc,max_parms,gc,data,dens,
     :                  weight,the_sign,start,end)
*+
* Name:
*    ALL_GAUSS

* Invocation:
*    CALL ALL_GAUSS(M,N,XC,RC,AJC,AJTJC,MAX_PARMS,GC,DATA,DENS,
*                       WEIGHT,THE_SIGN,START,END)

* Purpose:
*   Fit a single gaussian to all lines specified
*   simultaneously.

* Description:
*   Fit a single gaussian to all lines specified
*   simultaneously. In this model the REDSHIFT and
*   WIDTH of each gaussian are identical, whereas the
*   BASE and HEIGHT are variable. In principal this should
*   give better answers for both REDSHIFT and WIDTH for
*   weak data. The method of calculation uses a standard LSQ
*   we form
*   fc = sum {[( gausian + base - data) * weight]**2}
*   were the sum is over the number of lines.
*   In other words.
*   The data is partioned into seperate sum of squares for
*   each line and these are then added in quadrature to form
*   the total sum of squares to be minimized.

*   This routine assumes that ALL lines have been reduced
*   to a common DATA axis  = REDSHIFT prior to entry.
*   In addition it is assumed that the SCALING of DENS
*   has been achieved using a SINGLE value of DENMAX and DENMIN
*   over all the included points.
*
* Arguments:-
*    M = INTEGER (Given)
*        Number of points in data
*    XC(5) = DOUBLE PRECISION ARRAY (Given)
*        Fit parameters
*    N = INTEGER (Given)
*
*    DATA(M) = DOUBLE PRECISION ARRAY (Given)
*        X data
*    DENS(M) = DOUBLE PRECISION ARRAY (Given)
*        Y data
*    WEIGHT(M) = DOUBLE PRECISION ARRAY (Given)
*        weights
*    MAX_PARMS = INTEGER (Given)
*        Maximum number of parameters
*    RC(M) = DOUBLE PRECISION ARRAY (Returned)
*        Residuals on fit
*    AJC(M,N) = DOUBLE PRECISION ARRAY (Returned)
*        Derivatives
*    AJTJC(MAX_PARMS,MAX_PARMS) (d) Hessian
*    GC(MAX_PARMS) = DOUBLE PRECISION ARRAY (Returned)
*        Multiple of gradient and sum of squares
*
* History:
*  TNW 18/3/91, LSQ_FW2 and RESID_FW2 combined
*  TNW Cambridge, 20-JUN-1991 Bug fix re weights
*  DJA MAN 28-Jun-1991 Created from Fs2_fun
*  TNW 12-JUL-1991 Made to compile
*  TIMJ 15/8/05 Force do loop to use integeres
*-
      implicit none
      integer max_parms
      double precision ajtjc(max_parms,max_parms),gc(max_parms)
      integer m
      double precision data(m)
      double precision dens(m)
      double precision weight(m)
      integer n
      integer the_sign
      double precision xc(n),rc(m),ajc(m,n)
      integer n_gauss
*      double precision start(n_gauss), end(n_gauss)
      double precision start(*), end(*)
      double precision sum

* local variables

      integer  i, k, i_base, i_height
      double precision current_gaussian
      double precision red_mult, g_exp
      double precision width_mult, z_score2, z_score

* Symbolic constants

      integer SHARED_OFFSET
      integer PARS_PER_G
      parameter (PARS_PER_G = 2)
      Parameter (SHARED_OFFSET = 2)

      integer REDSHIFT
      integer WIDTH
      Parameter (WIDTH = 2)
      Parameter (REDSHIFT = 1)

* Added so compiles

      double precision g_redshift,g_width
      integer j

* Symbolic constants defining the order of the model paramters
* in the array XC.
* ---------------------------------------------------------------------
* Each line is assumed to have the same REDSHIFT and WIDTH
* The HEIGHT is independent for each line and in addition
* each line is allowed a varaible BASE. Thus number of
* Gaussians is

      n_gauss = (n - SHARED_OFFSET ) / PARS_PER_G

* zero out the derivatives and residuals arrays so that
* all points not refering to the current gaussian are correctly
* set to 0. we then only have to fill in those
* entries requried for the current gaussian.

      do i = 1,m
        do k = 1,n
          ajc(i,k) = 0.0d0
        end do
        rc(i) = 0.0d0
      end do

*    loop over the gaussians
*    determiing the residuals and the derivatives.
      do k = 1 , n_gauss

* break the data up into its start and end locations
* we need to do this because we must accumualte seperate
* derviatives for each HEIGHT and BASE

        do i = INT(start(k)) , INT(end(k))

          z_score    = ( data(i) - xc(REDSHIFT) ) / xc(WIDTH)
          Z_score2   = - 0.5d0 * z_score * z_score
          g_exp      = exp ( z_score2 )

* work out which paramters corrspond to the HEIGHT and BASE
* of this particular line

          i_base    = (k-1) * PARS_PER_G + SHARED_OFFSET + 1
          i_height  = i_base + 1

* form total gaussian for this line

          current_gaussian   = abs(xc(i_height)) * g_exp
          current_gaussian   = the_sign *  current_gaussian

* the residuals

          rc(i)  = current_gaussian - dens(i) + xc( i_base )
          rc(i)  = rc(i) * weight (i)

* form the gradient vector appropriate for least squares
* first we do the BASE and HEIGHT as these are different
* for EACH gaussian in this model.

*   diff wrt b

          ajc(i, i_base) = weight(i)

*   diff wrt h1

          ajc(i,i_height) = ajc(i, i_base) * g_exp

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

          ajc(i, REDSHIFT) = gc(i_base) * g_REDSHIFT
          ajc(i, WIDTH)    =  gc(i_base) * g_WIDTH

        end do

      end do

*
* GC    : the j(transpose)*r which is the multiple
*         of the gradient and the sum of squares
* AJTJC : the hesian matrix nb since symmetric do l .le. i
*
      do i = 1,n
        sum=0.0d0
        do k = 1,m
          sum = sum+ajc(k,i)*rc(k)
        end do
        gc(i) = sum
        do j = 1,i
          sum = 0.0d0
          do k = 1,m
            sum = sum+ajc(k,i)*ajc(k,j)
          end do
          ajtjc(j,i) = sum
        end do
      end do
      end
