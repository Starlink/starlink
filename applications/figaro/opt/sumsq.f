      subroutine sumsq(rc,m,f)
*+
* Name:
*    SUMSQ

* Invocation:
*    CALL SUMSQ(RC,M,F)

* Purpose:
*    To produce the sum of squares of residuals

* Description:
*    The sum of the squares of RC is evaluated.

* Arguments:
*    RC(M) = DOUBLE PRECISION ARRAY (Given)
*        Residuals
*    M = INTEGER (Given)
*        Number of points
*    F = DOUBLE PRECISION (Returned)
*        Sum of squares

* Authors:
*   T.N.Wilkins, 14-Oct-1991
*-
      implicit none
      integer m
      double precision rc(m),f
      integer i
      f = 0.0d0
      do i = 1, m
        f = f + rc(i)*rc(i)
      enddo
      end
