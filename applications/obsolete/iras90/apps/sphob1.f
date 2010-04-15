      SUBROUTINE SPHOB1( PARAM, PICID, X1, X2, Y1, Y2, STATUS )
*+
*  Name:
*     SPHOB1

*  Purpose:
*     Open a graphics device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPHOB1( PARAM, PICID, X1, X2, Y1, Y2, STATUS )

*  Description:
*     SGS is activated on the device specified by the given paramater.
*     An error is reported if the device does not have a cursor. The
*     AGI identifier for the most recent DATA picture created within
*     the current picture is returned, together with the upper and
*     lower bounds of the picture( with an additional 5% margin on each
*     edge). The picture comment (and label if it exists) is reported to
*     the user at VERBOSE priority.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The device parameter.
*     PICID = INTEGER (Returned)
*        The AGI identifier for the DATA picture.
*     X1 = REAL (Returned)
*        The lower bound on the X axis for which cursor positions
*        should be accepted. In world coordinates.
*     X2 = REAL (Returned)
*        The upper bound on the X axis for which cursor positions
*        should be accepted. In world coordinates.
*     Y1 = REAL (Returned)
*        The lower bound on the Y axis for which cursor positions
*        should be accepted. In world coordinates.
*     Y2 = REAL (Returned)
*        The upper bound on the Y axis for which cursor positions
*        should be accepted. In world coordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'AGI_ERR'          ! AGI_ error constants

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      INTEGER PICID
      REAL X1
      REAL X2
      REAL Y1
      REAL Y2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER PICCOM*80         ! Picture comment.
      CHARACTER PICLAB*80         ! Picture label.


      INTEGER PICID1             ! Original AGI picture identifier
      INTEGER ZONE1              ! SGS zone identifier for original
                                 ! picture.
      INTEGER ZONE2              ! SGS zone identifier for DATA
                                 ! picture.

      LOGICAL CURAVL             ! True if a cursor is available.


      REAL MARGIN                ! Margin to place round the edge of
                                 ! the picture.
      REAL XM                    ! X size of picture zone.
      REAL YM                    ! Y size of picture zone.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up SGS on the graphics device specified using the given
*  parameter.
      CALL AGS_ASSOC( PARAM, 'UPDATE', ' ', PICID1, ZONE1, STATUS )

*  Attempt to find a DATA picture.
      CALL IRM_AGFND( 'DATA', PICID, STATUS )

*  If a DATA picture could not be found, add a context message.
      IF( STATUS .EQ. AGI__NONAM ) THEN
         CALL ERR_REP( 'SPHOB1_ERR1',
     : 'SPHOB1: Unable to find a DATA picture which is contained '//
     : 'entirely within the current picture.', STATUS )
      ENDIF

*  Create an SGS zone corresponding to the DATA picture.
      CALL AGS_NZONE( ZONE2, STATUS )

*  See if the device has a cursor.
      CALL SGS_ICUAV( CURAVL )

*  If not, report an error.
      IF( .NOT. CURAVL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPHOB1_ERR2',
     :    'SPHOB1: Specified graphics device does not support a cursor',
     :                 STATUS )
      END IF

*  Describe the picture to the user.
      CALL AGI_ICOM( PICCOM, STATUS )
      CALL AGI_ILAB( -1, PICLAB, STATUS )

      IF( PICLAB .NE. ' ' ) THEN
         CALL MSG_SETC( 'COM', PICCOM )
         CALL MSG_SETC( 'LAB', PICLAB )
         CALL MSG_OUTIF( MSG__NORM, 'SPHOB1_MSG1',
     :                   '  DATA picture ^LAB ("^COM") being used',
     :                      STATUS )
      ELSE
         CALL MSG_SETC( 'COM', PICCOM )
         CALL MSG_OUTIF( MSG__NORM, 'SPHOB1_MSG2',
     :                   '  DATA picture "^COM" being used', STATUS )
      END IF

*  Enquire the zone size of the DATA picture. Extend the returned
*  bounds to give a "safety margin" round the edge of the picture, so
*  that the application doesn't exit if the user puts the cursor there
*  by accident, when in fact he intended to put it just inside the
*  picture.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

      MARGIN = 0.05*( X2 - X1 )
      X1 = X1 - MARGIN
      X2 = X2 + MARGIN

      MARGIN = 0.05*( Y2 - Y1 )
      Y1 = Y1 - MARGIN
      Y2 = Y2 + MARGIN

*  See if GKS/SGS has reported an error.
      CALL GKS_GSTAT( STATUS )

      END
