      SUBROUTINE KPG1_ASDSV( FRM, NP, NAX, POS, DIS, BAD, STATUS )
*+
*  Name:
*     KPG1_ASDSV

*  Purpose:
*     Find the distances between a set of points.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASDSV( FRM, NP, NAX, POS, DIS, BAD, STATUS )

*  Description:
*     This routine returns the distance to each point in a set of points,
*     from the first point, measured along the path joining the points. 
*     Geodesic distances within the supplied Frame are used.

*  Arguments:
*     FRM = INTEGER (Given)
*        An AST pointer to the Frame. 
*     NP = INTEGER (Given)
*        The size of the first dimension of the POS array.
*     NAX = INTEGER (Given)
*        The number of axes in the Frame.
*     POS( NP, NAX ) = DOUBLE PRECISION (Given)
*        An array holding the co-ordinates at NP positions within the
*        supplied Frame.
*     DIS( NP ) = DOUBLE PRECISION (Returned)
*        The distance along the path to each position, starting at the
*        first position.
*     BAD = LOGICAL (Returned)
*        Are there any AST__BAD values in the returned array?
*     STATUS = INTEGER (Given)
*        Global status value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants 
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER FRM
      INTEGER NP
      INTEGER NAX
      DOUBLE PRECISION POS( NP, NAX )

*  Arguments Returned:
      DOUBLE PRECISION DIS( NP )
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION INC       ! The distance between P1 and P2
      DOUBLE PRECISION P1( NDF__MXDIM )! The first position
      DOUBLE PRECISION P2( NDF__MXDIM )! The second position
      INTEGER I                  ! Position index
      INTEGER J                  ! Axis index
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the first position.
      DO J = 1, NAX
         P1( J ) = POS( 1, J )
      END DO

*  Store zero for the first position.
      DIS( 1 ) = 0.0D0

*  Loop round each of the other positions.
      DO I = 2, NP

*  If the distance to the previous position is known...
         IF( DIS( I - 1 ) .NE. AST__BAD ) THEN

*  Store the next position.
            DO J = 1, NAX
               P2( J ) = POS( I, J )
            END DO

*  Add on the distance from the previous position to this position (if
*  known).
            INC = AST_DISTANCE( FRM, P1, P2, STATUS ) 
            IF( INC .NE. AST__BAD ) THEN
               DIS( I ) = DIS( I - 1 ) + INC
            ELSE
               DIS( I ) = AST__BAD
            END IF

*  This position becomes the previous position.
            DO J = 1, NAX
               P1( J ) = P2( J )
            END DO

*  If the distance to the previous position is not known, neither is the
*  distance to this position.
         ELSE
            DIS( I ) = AST__BAD
         END IF

      END DO

*  If any bad values were found they will have been propagated to the last
*  element.
      BAD = ( DIS( NP ) .EQ. AST__BAD )

      END
