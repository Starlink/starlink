      subroutine endpts( y , n)
*+
* Name:
*    ENDPTS

* Invocation:
*    CALL ENDPTS( Y , N)

* Description:
*   Estimate smoothed values for both end points of the sequence in y()
*   using the end point extrapolation rule.  All the values in Y()
*   except the end points have already been smoothed.

      implicit none
      integer n
      real y(n)
*-
      logical CHANGE
      real y0 , ymed

      CHANGE =  .FALSE.

* left end

      Y0 = 3.0 * y(2) - 2.0 * y(3)

      call medof3(y0, y(1) , y(2) , ymed , CHANGE)
      y(1) = ymed
* right end

      Y0 = 3.0 * y(n - 1 ) - 2.0 * y(n - 2)

      call medof3(y0, y(n) , y(n - 1) , ymed , CHANGE)
      y(N) = ymed

      end
