      real function rchn2rx(array,n,elem)
*+
* Name:
*    RCHN2RX

* Invocation:
*   (REAL) = RCHN2RX(ARRAY,N,ELEM)

* Purpose:
*  To convert a real array element to a array value.

* Description:
*  To convert a real array element to a array value.
*
* Arguments:
*      ARRAY(N) = REAL ARRAY (Given)
*        Axis array
*      N = INTEGER (Given)
*        Dimension of axis array
*      ELEM = REAL (Given)
*        Element number (may be fractional)
*      RCHN2RX = REAL (Returned)
*        Array value
* Subroutines/functions referenced:
* Author:
*   T.N.Wilkins, Cambridge,  2-OCT-1990
*-
      implicit none
      integer n
      real array(n)
      real elem
      real frac
      integer ielem,ielem2

*  If out of range use last available dispersion value

      if(elem.lt.1.0) then
        ielem = 1
      else if(elem.gt.real(n)) then
        ielem = n - 1
      else
        ielem = int(elem)
      end if
      ielem2 = ielem + 1
      frac = (elem - real(ielem))/real(ielem2 - ielem)
      rchn2rx = array(ielem2)*frac + array(ielem)*(1.0-frac)
      end
