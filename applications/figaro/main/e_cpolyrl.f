      integer function e_cpolyrl(xminr,xmaxr,x,aa,k1,npts,add,y)
*+
* Name:
*    E_CPOLYRL

* Invocation:
*   (INTEGER) = E_CPOLYRL(XMINR,XMAXR,X,AA,K1,NPTS,ADD,Y)

* Purpose:
*   Evaluate a chebyshev polynomial.

* Description:
*   Evaluate a chebyshev polynomial. This Version outputs results in
*   real*4 format, and accepts the Normalizaton limits separately
*   rather than assuming that they are given by the first an last values
*   of X.
*   The logical flag ADD controls whether the result of evaluating the
*   cheby poly is added into Y without re-initialization to zero first
*   The NAG error code is returned via the function name.

* Arguments:
*   NPTS = INTEGER (Given)
*        Number of points to be evaluated
*   X(NPTS) = REAL ARRAY (Given)
*        Locations where polynomial should be evaluated
*   XMINR = REAL (Given)
*        Lower limit of Chebyshev X normalization range
*   XMAXR = REAL (Given)
*        Upper limit of valid Cheby range in X
*   AA(K1) = DOUBLE PRECISION ARRAY (Given)
*        Cheby Coeffs for order K1-1
*   K1 = INTEGER (Given)
*        Number of Cheby Coeffs
*   ADD = LOGICAL (Given)
*        TRUE if the current results are to be added into the existing Y array.
*        FALSE if the array is to initialized to zero
*   Y(NPTS) = REAL ARRAY (Given and returned)
*        The corrsponding Y values. Depending on the flag ADD these will either
*        by just the answer obtianed by evalauting the Cheby Poly or this and
*        the  sum the previous contents of Y.

* History:
*  Altered to use fig_nagerr, TNW 7/10/88
*  More calculations moved from loop, TNW 29/6/92
*  Replaced NAG call to e02aef with PDA_DP1VLU JWP Feb 97
*  Remove character strings continued across continuation lines: ACD,
*    28/9/00
*-
      implicit none
      integer npts
      real x(npts)
      real y(npts)
      integer k1
      double precision aa(k1)
      real xminr,xmaxr
      logical add

      include 'SAE_PAR'

*

* NAG ERROR CODE

      integer ifail

* Cheby Nornalization Double Precision Copies

      double precision xmin,xmax

* Normalized Cheby points

      double precision xcap,ycap,xpt,offset,normal,dummy

* do loop

      integer i
*
*  Generate the output data
*
      xmin=dble(xminr)
      xmax=dble(xmaxr)
*
      offset = (xmin+xmax)/(xmax-xmin)
      normal = 2.0d0/(xmax-xmin)
      do i =1,npts
        xpt = dble(x(i))

* Old version        xcap=((xpt-xmin)-(xmax-xpt))/(xmax-xmin)

        xcap = xpt * normal - offset
*        ifail =1
*        call e02aef(k1,aa,xcap,ycap,ifail)
*        if(ifail.ne.0) call fig_nagerr(ifail,'e02aef')

        ifail = 0
        CALL PDA_DP1VLU( k1-1, 0, xcap, ycap, dummy, aa, ifail)

        y(i) = ycap

        IF (ifail.NE.0) CALL ERR_REP( ' ',
     :    'Error from  PDA_DP1VLU in e_cpolyrl.f', SAI__ERROR)

* add in the results or re-fill Y with the answers
        if(add) then
          y(i) = y(i) + real(ycap)
        else
          y(i) = real(ycap)
        end if
      end do

      e_cpolyrl = ifail
      end





