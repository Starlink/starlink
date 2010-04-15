      subroutine mgfun(iflag,n,xc,fc1,gc1,iw,liw,w,lw)
*+
* Name:
*    MGFUN

* Invocation:
*    CALL MGFUN(IFLAG,N,XC,FC1,GC1,IW,LIW,W,LW)

* Purpose:
*   Derivative/function evaluation for E04KDF Gaussian fitting

* Description:
*   For constrained multiple gaussian fit.
*   runs over the number of gaussians specified
*   for current fit. for each gaussian the extra terms
*   for each of the derivatives in the least squares
*   fit are storred in g
*   the correct derivative is then created by multiplying
*   by g.
*
* Arguments:
*   N = INTEGER (Given)
*        Number of free parameters
*   XC(N) = DOUBLE PRECISION ARRAY (Given)
*        Parameters
*   FC1 = DOUBLE PRECISION (Given)
*
*   GC1(N) = DOUBLE PRECISION ARRAY (Given)
*
*   IFLAG = INTEGER (Not used)
*   W(LW) = DOUBLE PRECISION ARRAY (LW not used otherwise)
*   IW(LIW) = DOUBLE PRECISION ARRAY (ILW not used otherwise)
*
*
      implicit none
      include 'opt_cmn'
      include 'CNF_PAR'          ! For CNF_PVAL function
*
*-

* number of fit parameters

      integer n
      integer lw,liw,iflag

* work space

      double precision w(lw)

* current point

      double precision xc(n)
      integer iw(liw)
      double precision fc1,gc1(n)
      double precision g(max_parms)
* ---------------------------------------------------------------------

* set gradient vector to zero

      call zero_dble(gc1,n)

* Perform the work, done in a subroutine to allow use of pointers
* for large arrays in common.

      call mgfun_sub(mpts,n,%VAL(CNF_PVAL(dataptr)),fc1,
     :               %VAL(CNF_PVAL(densptr)),xc,gc1,
     :               %VAL(CNF_PVAL(weightptr)),g)
      end
