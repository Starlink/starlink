      SUBROUTINE IRA_DRBND( IDA, LBND, UBND, STATUS )
*+
*  Name:
*     IRA_DRBND

*  Purpose:
*     Draw a boundary around the area representing valid sky positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DRBND( IDA, LBND, UBND, STATUS )

*  Description:
*     This routine draws a curve around the region of image space
*     containing valid sky positions. For small images this will
*     usually just result in a box being drawn coincident with the
*     edges of the image. However, some projections (for instance
*     Aitoff and orthographic projections) project the sky into a
*     finite area of image space, resulting in some image coordinates
*     not corresponding to any valid sky coordinates. Large images may
*     extend beyond the valid region of image space, and in this case
*     this routine draws a boundary around the used area of image
*     space.

*     The plotting is done within the section of the current SGS zone
*     specified by LBND and UBND, and it is assumed that the world
*     coordinate system corresponds to image (or pixel) coordinates.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     LBND( 2 ) = REAL (Given)
*        Lower world coordinate bounds for each axis defining the
*        section of the current SGS zone in which the curve is to be
*        drawn.
*     UBND( 2 ) = REAL (Given)
*        Upper world coordinate bounds for each axis defining the
*        section of the current SGS zone in which the curve is to be
*        drawn.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is effected by the TOLERANCE and PEN1 options set
*     up by routine IRA_DROPT.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1992 (DSB):
*        Original version.
*     9-NOV-1992 (DSB):
*        Introduced used of graphics options.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_ERR'          ! IRA error constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_DROPT( IRA__NOPT ) = DOUBLE PRECISION (Read)
*           Graphics options values.

*  Arguments Given:
      INTEGER IDA
      REAL LBND( 2 )
      REAL UBND( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  External Routines:
      EXTERNAL IRA1_INIT         ! Initialise graphics options in common

*  Local Variables:
      REAL ACC                   ! Accuracy of curve, in pixels.
      LOGICAL ALLBAD             ! True if no good points found in image
      INTEGER LTOL               ! TOL limited to [0,10].
      LOGICAL NOBAD              ! true if no bad points found in image.
      INTEGER NPEN               ! SGS pen on entry.
      REAL X                     ! X image coord. of a point on the
                                 ! good/bad boundary.
      REAL Y                     ! Y image coord. of a point on the
                                 ! good/bad boundary.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Check that the LBND and UBND arguments are OK.
      IF( ( LBND( 1 ) .GE. UBND( 1 ) .OR.
     :    LBND( 2 ) .GE. UBND( 2 ) ) ) THEN
         STATUS = IRA__ORDER
         CALL MSG_SETR( 'U1', UBND( 1 ) )
         CALL MSG_SETR( 'U2', UBND( 2 ) )
         CALL MSG_SETR( 'L1', LBND( 1 ) )
         CALL MSG_SETR( 'L2', LBND( 2 ) )
         CALL ERR_REP( 'IRA_DRBND_ERR1',
     :'IRA_DRBND: Viewport upper bounds (^U1,^U2) are inconsistent '//
     :'with lower bounds (^L1,^L2).', STATUS )
         GO TO 999
      END IF

*  Set up accuracy required for boundary in units of pixels.
      LTOL = MAX( 0, MIN( 10, NINT( ACM_DROPT( 3 ) ) ) )
      ACC = ( 8.8E-4*REAL( LTOL )+2.0E-4 )*MIN( UBND( 1 ) - LBND( 1 ),
     :                                    UBND( 2 ) - LBND( 2 ) )

*  Find a point on the boundary to ten times the required accuracy.
      CALL IRA1_FNDB( IDA, LBND( 1 ), LBND( 2 ), UBND( 1 ), UBND( 2 ),
     :                0.1*ACC, NOBAD, ALLBAD, X, Y, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  No boundary exists if no good points were found.
      IF( ALLBAD ) GO TO 999

*  Save the current SGS pen, and set up the pen given by option PEN1.
      CALL SGS_IPEN( NPEN )
      CALL SGS_SPEN( NINT( ACM_DROPT( 7 ) ) )

*  If no bad points were found, assume entire image is inside the
*  boundary. Draw a box round the image to mark the boundary.
      IF( NOBAD ) THEN
         CALL SGS_BOX( LBND( 1 ), UBND( 1 ), LBND( 2 ), UBND( 2 ) )

*  Otherwise, trace out the good/bad boundary to the required accuracy.
      ELSE
         CALL IRA1_TRAB( IDA, LBND( 1 ), LBND( 2 ), UBND( 1 ),
     :                   UBND( 2 ), ACC, X, Y, STATUS )

      END IF

*  Reinstate the original SGS pen number.
      CALL SGS_SPEN( NPEN )

*  If an error has occurred, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_DRBND_ERR2',
     :            'IRA_DRBND: Error drawing a sky coordinate boundary.',
     :                 STATUS )
      END IF

      END
