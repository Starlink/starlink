      subroutine bget(xu,xl,mean,skew,sigma1)
*+
* Name:
*    BGET

* Invocation:
*    CALL BGET(XU,XL,MEAN,SKEW,SIGMA1)
* Purpose:
*   Routine to estimate b and sigma1 for skewed gaussian.

* Description:
*   Routine to estimate b and sigma1 for skewed gaussian.
*
* Arguments:
*    XU = DOUBLE PRECISION (Given)
*        x-value for upper half max (I.E x2/2)
*    XL = DOUBLE PRECISION (Given)
*        x-value for lower half max
*    MEAN = DOUBLE PRECISION (Given)
*        mean
*    SKEW = DOUBLE PRECISION (Returned)
*        skew parameter
*    SIGMA1 = DOUBLE PRECISION (Returned)
*        sigma1
* History:
*   Optimised, TNW/Cambridge, 21/3/91
*   Removed NAG call. JWP March 97
*-
      implicit none
      double precision xu
      double precision xl
      double precision mean
      double precision skew
      double precision sigma1
      double precision dasinh
*
* local
*
      double precision a
*      double precision s11abf
      double precision b
      double precision c
      double precision a1
      integer ifail
* ----------------------------------------------------------------------
*
      a = (xu - mean)
      b = (xl - mean)
      c = (xu - xl)
*
      a1    = -0.5d0*(a*a - b*b) / (a * b)
      ifail = 0
*     skew  = s11abf(a1,ifail)
      skew = dasinh( a1 )
*
*  ** Check to see if SKEW is less than 10-3.
*  ** If so return and fit ordinary gaussian.
*
      if(abs(b).le.1.0d-3) then
        skew = 0.0d0
      else
*
*  ** And if line skewed estimate SIGMA1.
*
        sigma1 = c*skew / sinh(skew)
      end if
      end
