      subroutine error_e04kdf(iflag,n,xc,fc1,gc1,iw,liw,w,lw)
*+
* Name:
*    ERROR_E04KDF

* Invocation:
*    CALL ERROR_E04KDF(IFLAG,N,XC,FC1,GC1,IW,LIW,W,LW)

* Purpose:
*  Return error to abort fitting.

* Description:
*  Return error to abort fitting.

* Arguments:
*   N = INTEGER (Given)
*        Number of free parameters
*   XC(N) = DOUBLE PRECISION ARRAY (Given)
*        Parameters
*   FC1 = DOUBLE PRECISION (Given)
*
*   GC1(N) = DOUBLE PRECISION ARRAY (Given)
*
*   IFLAG = INTEGER (Given)
*        Error flag
*   W(LW) (LW not used otherwise)
*   IW(LIW) (ILW not used otherwise)
*
*
      implicit none
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
* ---------------------------------------------------------------------
      iflag = -1
      end
