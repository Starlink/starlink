      real function FAST_GAU_CDF(z)
*+
* Name:
*    FAST_GAU_CDF

* Invocation:
*   (REAL) = FAST_GAU_CDF(Z)

* Purpose:
*   this function calculates the value of the standard gaussian cummlative
*   distibution function at Z.

* Description:
*   this function calculates the value of the standard gaussian cummlative
*   distibution function at Z.
*   the algorithm uses approximations given in Derenzo, S. E. in
*   Mathematics of cumputation V31 (1977), pp 214-225.
*
      implicit none
      real z
*-
      real p,pi,x

      x = abs(z)
      if (x.le.5.5) then
        p = exp( -((83.0*x + 351.0) * x + 56.2) * x /
     : (703.0 + 165.0 * x ))

      else
        pi = 4.0 * atan(1.0)
        p = sqrt(2.0/pi) * exp ( -(x * x/2.0 + 0.94/(x*x)))/x
      end if

* the approximations yield values of the half-normal tail area.
* translate that into the value of the gaussian cdf and allow for
* the sign of Z.

      FAST_GAU_CDF = p/2.0
      if (z .gt. 0.0 ) then
        FAST_GAU_CDF = 1.0 - FAST_GAU_CDF
      end if
      end
