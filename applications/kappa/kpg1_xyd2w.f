      SUBROUTINE KPG1_XYD2W( SCALE, OFFSET, NPOINT, XP, YP, STATUS )
*+
*  Name:
*     KPG1_XYD2W

*  Purpose:
*     Converts linear data co-ordinates to world co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_XYD2W( SCALE, OFFSET, NPOINT, XP, YP, STATUS )

*  Description:
*     The co-efficients of the linear transformation from world
*     co-ordinates to data co-ordinates are supplied in arguments SCALE
*     and OFFSET.  The inverse of this transformation is used to
*     transform each supplied position from data to world co-ordinates.

*  Arguments:
*     SCALE( 2 ) = DOUBLE PRECISION (Given)
*        The scale factors in the linear relationships between axis
*        co-ordinates and pixel co-ordinates. 
*     OFFSET( 2 ) = DOUBLE PRECISION (Given)
*        The offsets in the linear relationships between axis
*        co-ordinates and pixel co-ordinates. 
*     NPOINT = INTEGER (Given)
*        The number of points specified.
*     XP( NPOINT ) = REAL (Given and Returned)
*        Array holding the x co-ordinate of each point.
*     YP( NPOINT ) = REAL (Given and Returned)
*        Array holding the y co-ordinate of each point.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The supplied values of SCALE and OFFSET are such that:
*
*        DATA = SCALE( I ) * PIXEL + OFFSET( I )
*
*     where PIXEL is a pixel co-ordinate for the I'th dimension, and
*     DATA is the corresponding axis co-ordinate.
  
*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1993 (DSB):
*        Original version.
*     1995 April 12 (MJC):
*        Renamed from KPS1_XYD2W and referred to positions and not
*        vertices.  Minor stylistic changes and typo's fixed.
*     {enter_further_changes_here}

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
      INTEGER NPOINT

*  Arguments Given and Returned:
      REAL XP( NPOINT )
      REAL YP( NPOINT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if either of the scale factors are zero.
      IF ( ABS( SCALE( 1 ) ) .LE. VAL__SMLD .OR. 
     :     ABS( SCALE( 2 ) ) .LE. VAL__SMLD ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_XYD2W_ERR1', 'Pixels have zero size '/
     :                 /'in the data co-ordinate system', STATUS )

*  Otherwise, loop round for each point.
      ELSE
         DO I = 1, NPOINT

*  Apply the scaling to convert the supplied data co-ordinates into
*  pixel co-ordinates.
            XP( I ) = REAL( ( DBLE( XP( I ) ) - OFFSET( 1 ) )
     :                         / SCALE( 1 ) )
            YP( I ) = REAL( ( DBLE( YP( I ) ) - OFFSET( 2 ) )
     :                         / SCALE( 2 ) )

         END DO

      END IF

      END
