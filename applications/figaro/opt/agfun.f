      subroutine Agfun(iflag,n,xc,fc1,gc1,iw,liw,w,lw)
*+
* Name:
*    AGFUN

* Invocation:
*    CALL AGFUN(IFLAG,N,XC,FC1,GC1,IW,LIW,W,LW)

* Purpose:
*   Fit a single gaussian to all lines specified
*   simultaneously.

* Description:
*   Fit a single gaussian to all lines specified
*   simultaneously. In this model the REDSHIFT and
*   WIDTH of each gaussian are identical, whereas the
*   BASE and HEIGHT are variable. In principal this should
*   give better answers for both REDSHIFT and WIDTH for
*   weak data. The method of calculation usesa none LSQ
*   optimization routine because we need to form
*   fc = sum {[( gausian + base - data) * weight]**2}
*    were the sum is over the number of lines.
*   In other words.
*   The data is partioned into seperate sum of squares for
*   each line and these are then added in quadrature to form
*   the total sum of squares to be minimized.
*   N.B. for the standard NAG LSQ routines one is expected
*   to return RC at each point, which it then converts to
*   the sum-of-squares. With our formalisum this is possible
*   but we have written it here for the general routine
*   in order to allow the use of constraints.
*   This routine assumes that ALL lines have been reduced
*   to a common DATA axis  = REDSHIFT prior to entry.
*   In addition it is assumed that the SCALING of DENS
*   has been achieved using a SINGLE value of DENMAX and DENMIN
*   over all the included points.
*
* Arguments:-
*   N = INTEGER (Given)
*        Number of free parameters
*   XC(N) = DOUBLE PRECISION ARRAY (Given)
*        Parameters
*   FC1 = DOUBLE PRECISION (Given)
*
*   GC1(N) = DOUBLE PRECISION ARRAY (Given)
*
* Not used:-
*   IFLAG
*   W(LW) (LW not used otherwise)
*   IW(LIW) (ILW not used otherwise)
*
*
      implicit none
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'opt_cmn'
*
*-

* number of fit parameters

      integer n
      integer lw,liw,iflag,ngauss

* work space

      double precision w(lw)

* Symbolic constants

      integer PLUS_ONE
      integer MINUS_ONE
      integer SHARED_OFFSET
      integer PARS_PER_G
      parameter (PARS_PER_G = 2)
      Parameter (SHARED_OFFSET = 2)
      parameter (PLUS_ONE  = 1)
      Parameter (MINUS_ONE = -1)

* current point

      double precision xc(n)
      integer iw(liw)
      integer the_sign
      double precision fc1,gc1(n)

* These are just added to make in compile, unlikely to be right!

      double precision start(5),end(5)

* ---------------------------------------------------------------------
* Each line is assumed to have the same REDSHIFT and WIDTH
* The HEIGHT is independent for each line and in addition
* each line is allowed a varaible BASE. Thus number of
* Gaussians is

      ngauss = (n - SHARED_OFFSET ) / PARS_PER_G

* set gradient vector to zero

      call zero_dble(gc,n)

* decide if absortion or emission lines

      if(ABSORPTION) then
        the_sign = MINUS_ONE
      else
        the_sign = PLUS_ONE
      end if

* Perform the work, done in a subroutine to allow use of pointers
* for large arrays in common.

      call agfun_sub(mpts,n,%VAL(CNF_PVAL(dataptr)),fc,
     :               %VAL(CNF_PVAL(densptr)),xc,gc,gc1,
     :               %VAL(CNF_PVAL(weightptr)),start,end,ngauss,
     :               the_sign)
      fc1 = fc
      end
