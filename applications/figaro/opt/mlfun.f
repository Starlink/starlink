      subroutine mlfun(iflag,n,xc,fc1,gc1,iw,liw,w,lw)
*+
* Name:
*    MLFUN

* Invocation:
*    CALL MLFUN(IFLAG,N,XC,FC1,GC1,IW,LIW,W,LW)

* Purpose:
*   Derivatives/residuals for Lorentzian fitting

* Description:
*   For constrained multiple Lorentzian fit.
*   runs over the number of Lorentzians specified
*   for current fit. for each Lorentzian the extra terms
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
* Not used:
*   IFLAG
*   W(LW) (LW not used otherwise)
*   IW(LIW) (ILW not used otherwise)
*
* History:
*      TNW Cambridge, 1/7/92 GC not used
*
      implicit none
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'opt_cmn'
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

      call mlfun_sub(mpts,n,%VAL(CNF_PVAL(dataptr)),fc,
     :               %VAL(CNF_PVAL(densptr)),xc,gc1,
     :               %VAL(CNF_PVAL(weightptr)),g)

      fc1 = fc
      end
