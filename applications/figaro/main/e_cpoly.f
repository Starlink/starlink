      integer function e_cpoly(x,y,aa,k1,npts,athree,max_kplus1,maxnpts)
*+
* Name:
*    E_CPOLY

* Invocation:
*   (INTEGER) = E_CPOLY(X,Y,AA,K1,NPTS,ATHREE,MAX_KPLUS1,MAXNPTS)

* Purpose:
*   Evaluate a chebyshev polynomial

* Description:
*   Evaluate a chebyshev polynomial for a Specified order
*   and Normalization. This version assumes that the normalization
*   range is determined by X(1) and X(NPTS).

* Arguments:
*   X(NPTS) = DOUBLE PRECISION ARRAY (Given)
*        X coordinates of points to be evaluated
*   Y(NPTS) = DOUBLE PRECISION ARRAY (Returned)
*        Y values generated at each X from CHeby Poly
*   AA(K1) = DOUBLE PRECISION (Given)
*        Chebyshev coefficnets of order k1-1
*   K1 = INTEGER (Given)
*        Number of Cheby Coeffs
*   NPTS = INTEGER (Given)
*        Number of points to be evaluated
*   ATHREE(3*MAXNPTS+3*MAX_KPLUS1) = DOUBLE PRECISION ARRAY
*     PDA_DPOLFT fit co-effs
*   MAX_KPLUS1 = INTEGER (Given)
*        Max order of polynomial fit
*   MAXNPTS = INTEGER (Given)
*        Max number of data points

* Returned value:
*   status IFAIL.

* History:
*  Altered to use fig_nagerr, TNW 7/10/88
*  More calculations moved from loop, TNW 29/6/92
*  Removed NAG call to e02aef and replaced with PDA call JWP Dec 96
*  PDA call fixed using current fit results array AJH Sep 97
*  Remove local unused variables and remove character strings continued
*    across continuation lines: ACD, 28/9/00.
*  Revised the list of arguments in the prologue comments to correspond to
*    the actual order of the arguments: ACD, 19/12/00.
*-
      implicit none
      include 'PRM_PAR'
      include 'SAE_PAR'
      integer npts
      integer maxnpts
      double precision x(npts)
      double precision y(npts)
      integer k1
      double precision aa(k1)
      integer max_kplus1
      double precision athree(3*maxnpts + 3*max_kplus1)

* local
* NAG error condition flag

      integer ifail

* Cheby Normalization range

      double precision xmin,xmax

* Normalized X and Y values

C      double precision xcap
      double precision ycap,dummy

* Do Loop

      integer i
      double precision xrange,normal,offset
      integer pstat
*
* --------------------------------------------------------------
*
*  Generate the output data
*
      xmin=x(1)
      xmax=x(npts)
      xrange = xmax-xmin
      if(abs(xrange).le.(10.0*VAL__SMLR)) then
        call par_wruser('Range of x is approximately zero',pstat)
        e_cpoly = 20
        return
      end if
      normal = 2.0d0/xrange
      offset = (xmin+xmax)/xrange
      do i =1,npts
*        xcap = x(i) * normal - offset
*        ifail = 1
*        call e02aef(k1,aa,xcap,ycap,ifail)

        ifail = 0
*     JWP put aa for athree
*        CALL PDA_DP1VLU( k1-1, 0, xcap, ycap, dummy, athree, ifail)

        CALL PDA_DP1VLU( k1-1, 0, x(i), ycap, dummy, athree, ifail)

        y(i) = ycap

        IF (ifail.NE.0) CALL ERR_REP( ' ',
     :    'Error from  PDA_DP1VLU in e_cpoly.f', SAI__ERROR)
      end do

      e_cpoly = ifail

      end








