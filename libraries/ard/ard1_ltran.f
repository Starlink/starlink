      SUBROUTINE ARD1_LTRAN( NDIM, C, NP, IN, OUT, STATUS )
*+
*  Name:
*     ARD1_LTRAN

*  Purpose:
*     Apply an n-D linear mapping to a set of co-ordinates

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_LTRAN( NDIM, C, NP, IN, OUT, STATUS )

*  Description:
*     The supplied co-efficients are used to transform the co-ordinates
*     given by IN, and the results are returned in OUT. The mapping is:
*
*     OUT(1) = C(1)      +C(2)*IN(1)    +C(3)*IN(2)...     +C(N+1)*IN(N)
*     OUT(2) = C(N+2)  +C(N+3)*IN(1)  +C(N+4)*IN(2)...   +C(2*N+2)*IN(N)
*      ...
*     OUT(N) = C(N*N)+C(N*N+1)*IN(1)+C(N*N+2)*IN(2)... +C(N*(N+1))*IN(N)

*  Arguments:
*     NDIM = INTEGER (Given)
*        The no. of dimensions
*     C( * ) = DOUBLE PRECISION (Given)
*        Co-efficients. There should be NDIM*(NDIM+1) of them. This is not
*        checked.
*     NP = INTEGER (Given)
*        The no. of points.
*     IN( NDIM, NP ) = DOUBLE PRECISION (Given)
*        The input co-ordinates.
*     OUT( NDIM, NP ) = DOUBLE PRECISION (Returned)
*        The output co-ordinates. This can be the same as IN.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1994 (DSB):
*        Original version.
*     26-JUN-2001 (DSB):
*        Modified for ARD version 2.0.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants

*  Arguments Given:
      INTEGER NDIM
      DOUBLE PRECISION C( * )
      INTEGER NP
      DOUBLE PRECISION IN( NDIM, NP )

*  Arguments Returned:
      DOUBLE PRECISION OUT( NDIM, NP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IC, J, I, K
      DOUBLE PRECISION RES( ARD__MXDIM )     ! Resulting position      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each position.
      DO K = 1, NP

*  Initialise the index of the next co-efficient.
         IC = 1

*  Loop round each output dimension.
         DO J = 1, NDIM

*  Initialise the output value.
            RES( J ) = C( IC )
            IC = IC + 1

*  Loop round the input dimensions.
            DO I = 1, NDIM

*  Increment the output value.
               RES( J ) = RES( J ) + IN( I, K )*C( IC )
               IC = IC + 1

            END DO

         END DO

*  Store the results in the output array (which may be the same as the
*  input array).
         DO J = 1, NDIM
            OUT( J, K ) = RES( J )
         END DO

      END DO
      
      END
