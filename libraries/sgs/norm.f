      SUBROUTINE sgs_1NORM (X,Y, XN,YN)
*+
*   - - - - -
*    N O R M     (Internal routine)
*   - - - - -
*
*   Normalize a pair of numbers which specify the extent of a rectangle.
*
*   The routine returns a pair of numbers with the same quotient but
*   neither of which is greater than unity.
*
*   Given:
*        X,Y       r     number pair to be normalised
*
*   Returned:
*        XN,YN     r     normalised number pair
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL X,Y,XN,YN

      REAL XA,YA,D



      XA=ABS(X)
      YA=ABS(Y)
      D=MAX(XA,YA)
      XN=X/D
      YN=Y/D

      END
