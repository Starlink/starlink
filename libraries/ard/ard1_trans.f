      SUBROUTINE ARD1_TRANS( N, C, IN, OUT )
*+
*  Name:
*     ARD1_TRANS

*  Purpose:
*     Apply an n-D linear mapping to a set of co-orrdinates

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_TRANS( N, C, IN, OUT )

*  Description:
*     The supplied co-efficients are used to transform the co-ordinates
*     given by IN, and the results are returned in OUT. The mapping is:
*
*     OUT(1) = C(1)      +C(2)*IN(1)    +C(3)*IN(2)...     +C(N+1)*IN(N)
*     OUT(2) = C(N+2)  +C(N+3)*IN(1)  +C(N+4)*IN(2)...   +C(2*N+2)*IN(N)
*      ...
*     OUT(N) = C(N*N)+C(N*N+1)*IN(1)+C(N*N+2)*IN(2)... +C(N*(N+1))*IN(N)

*  Arguments:
*     N = INTEGER (Given)
*        The no. of dimensions
*     C( * ) = REAL (Given)
*        Co-efficients. There should be N*(N+1) of them. This is not
*        checked.
*     IN( N ) = REAL (Given)
*        The input co-ordinates.
*     OUT( N ) = REAL (Returned)
*        The output co-ordinates.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER N
      REAL C(*)
      REAL IN( N )

*  Arguments Returned:
      REAL OUT( N )

*  Local Variables:
      INTEGER IC, J, I
*.

*  Initialise the index of the next co-efficient.
      IC = 1

*  Loop round each output dimensions.
      DO J = 1, N

*  Initialise the output value.
         OUT( J ) = C( IC )
         IC = IC + 1

*  Loop round the input dimensions.
         DO I = 1, N

*  Increment the output value.
            OUT( J ) = OUT( J ) + IN( I )*C( IC )
            IC = IC + 1

         END DO

      END DO
      
      END
