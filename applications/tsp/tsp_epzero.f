      SUBROUTINE TSP_EPZERO(SIZE,A)
*+
*
*   T S P _ E P Z E R O
*
*   Fill an array with zeros
*
*   (>)   SIZE   (Integer)    Size of array
*   (!)   A      (Real array(SIZE))  The array to be filled with zeros
*
*   Jeremy Bailey    12/7/1990
*
*+

      IMPLICIT NONE

*  Parameters
      INTEGER SIZE
      REAL A(SIZE)

*  Local variable
      INTEGER I

      DO I=1,SIZE
          A(I) = 0.0
      ENDDO
      END


