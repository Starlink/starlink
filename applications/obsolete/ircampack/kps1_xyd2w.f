      SUBROUTINE KPS1_XYD2W( SCALE, OFFSET, NVERT, XVERT, YVERT,
     :                       STATUS )
*+
*  Name:
*     KPS1_XYD2W

*  Purpose:
*     Convert data co-ordinates to world co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_XYD2W( SCALE, OFFSET, NVERT, XVERT, YVERT, STATUS )

*  Description:
*     The co-efficients of the linear transformation from world
*     co-ordinates to data co-ordinates are supplied in arguments SCALE
*     and OFFSET. The inverse of this transformation is used to
*     transform each supplied vertex from data to world co-ordinates.

*  Arguments:
*     SCALE( 2 ) = DOUBLE PRECISION (Given)
*        The scale factors in the linear relationships between axis
*        co-ordinates and pixel co-ordinates.
*     OFFSET( 2 ) = DOUBLE PRECISION (Given)
*        The offsets in the linear relationships between axis
*        co-ordinates and pixel co-ordinates.
*     NVERT = INTEGER (Given)
*        The number of vertices specified.
*     XVERT( NVERT ) = REAL (Given and Returned)
*        Array holding the X co-ordinate of each vertex.
*     YVERT( NVERT ) = REAL (Given and Returned)
*        Array holding the Y co-ordinate of each vertex.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The supplied values of SCALE and OFFSET are such that:
*
*        AXIS = SCALE( I )*PIXEL + OFFSET( I )
*
*        where PIXEL is a pixel co-ordinate for the I'th dimension, and
*        DATA is the corresponding axis co-ordinate.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      DOUBLE PRECISION SCALE( 2 )
      DOUBLE PRECISION OFFSET( 2 )
      INTEGER NVERT

*  Arguments Given and Returned:
      REAL XVERT( NVERT )
      REAL YVERT( NVERT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if either of the scale factors are zero.
      IF( ABS( SCALE( 1 ) ) .LE. VAL__SMLD .OR.
     :    ABS( SCALE( 2 ) ) .LE. VAL__SMLD ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_XYD2W_ERR1', 'Pixels have zero size '//
     :                 'in the data co-ordinate system', STATUS )

*  Otherwise, loop round each vertex.
      ELSE
         DO I = 1, NVERT

*  Apply the scaling to convert the supplied data co-ordinates into pixel
*  co-ordinates.
            XVERT( I ) = REAL( ( DBLE( XVERT( I ) ) - OFFSET( 1 ) )
     :                         /SCALE( 1 ) )
            YVERT( I ) = REAL( ( DBLE( YVERT( I ) ) - OFFSET( 2 ) )
     :                         /SCALE( 2 ) )

         END DO

      END IF

      END
