      SUBROUTINE IRA_DRMER( IDA, A, B, INCB, SCS, LBND, UBND, STATUS )
*+
*  Name:
*     IRA_DRMER

*  Purpose:
*     Draw a section of a meridian.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DRMER( IDA, A, B, INCB, SCS, LBND, UBND, STATUS )

*  Description:
*     This routine draws a curve representing a section of a meridian,
*     starting at a given position and extending over a given latitude
*     range. The curve is split into a number of sections and each
*     section drawn as a straight line. The number of sections used
*     depends on the curvature of the meridian and the value of
*     graphics option TOLERANCE (see routine IRA_DROPT); highly curved
*     meridians are split into more sections than nearly straight ones.
*     The plotting is done within the section of the current SGS zone
*     specified by LBND and UBND, and it is assumed that the world
*     coordinate system within the zone corresponds to image (or pixel)
*     coordinates.  Only the section of the meridian which lies within
*     the zone is displayed.
*
*     Various items of information about the plotted curve can be
*     obtained once the plot has been produced using routine IRA_DRBRK.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     A = DOUBLE PRECISION (Given)
*        The sky longitude at the start of the meridian, in radians.
*     B = DOUBLE PRECISION (Given)
*        The sky latitude at the start of the meridian, in radians.
*     INCB = DOUBLE PRECISION (Given)
*        The increment in sky latitude between the start and end of
*        the meridian, in radians. This can be positive or negative.
*        The value used is limited internally to an absolute value no
*        greater than 2.PI.
*     SCS = CHARACTER * ( * ) (Given)
*        The name of the sky coordinate system to use. Any unambiguous
*        abbreviation will do. This need not be the same as the sky
*        coordinate system stored in the astrometry structure
*        identified by IDA. See ID/2 section "Sky Coordinates" for more
*        information. A blank value will cause the system associated
*        with IDA to be used.
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
*     -  This routine is effected by the TOLERANCE, LINES and PEN2
*     options set up by routine IRA_DROPT.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1992 (DSB):
*        Original version.
*     9-NOV-1992 (DSB):
*        Graphics options introduced.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'IRA_ERR'          ! IRA_ error constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_DROPT( IRA__NOPT ) = DOUBLE PRECISION (Read)
*           The values of the graphics options.
*        ACM_M2A0 = DOUBLE PRECISION (Write)
*           The sky longitude at the start of the meridian.
*        ACM_M2B0 = DOUBLE PRECISION (Write)
*           The sky latitude at the start of the meridian.
*        ACM_M2ID = INTEGER (Write)
*           The IRA identifier for the astrometry information.
*        ACM_M2IN = DOUBLE PRECISION (Write)
*           Latitude increment along the curve.
*        ACM_M2SC = CHARACTER (Write)
*           The Sky Coordinate System to use.
*        ACM_NBRK = INTEGER (Write)
*           The no. of breaks in the plotted curve.
*        ACM_OUT = LOGICAL (Write)
*           True if the curve was completely outside the plotting area.
*        ACM_BRK( 2, IRA__MXBRK ) = REAL (Write)
*           The world coords of each break in the curve.
*        ACM_VBRK( 2, IRA__MXBRK ) = REAL (Write)
*           The unit direction vector in world coords at of each break
*           in the curve.
*        ACM_LENG = REAL (Write)
*           The length of the plotted curve in world coordinates.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Read)
*           The sky coordinate system used by the projection.

*  Arguments Given:
      INTEGER IDA
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION INCB
      CHARACTER SCS*(*)
      REAL LBND( 2 )
      REAL UBND( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IRA1_MAP2
      EXTERNAL IRA1_INIT

*  Local Variables:
      LOGICAL INK                ! True if lines are to be plotted.
      INTEGER NPEN               ! SGS pen on entry.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Check that the LBND and UBND arguments are OK.
      IF( LBND( 1 ) .GE. UBND( 1 ) .OR.
     :    LBND( 2 ) .GE. UBND( 2 ) ) THEN
         STATUS = IRA__ORDER
         CALL MSG_SETR( 'U1', UBND( 1 ) )
         CALL MSG_SETR( 'U2', UBND( 2 ) )
         CALL MSG_SETR( 'L1', LBND( 1 ) )
         CALL MSG_SETR( 'L2', LBND( 2 ) )
         CALL ERR_REP( 'IRA_DRMER_ERR1',
     :'IRA_DRMER: Viewport upper bounds (^U1,^U2) are inconsistent '//
     :'with lower bounds (^L1,^L2).', STATUS )
         GO TO 999
      END IF

*  Return without drawing anything if any of the inputs are bad.
      IF( A .EQ. VAL__BADD .OR. B .EQ. VAL__BADD .OR.
     :    INCB .EQ. VAL__BADD ) GO TO 999

*  Store other information needed by IRA1_MAP2 in common.
      ACM_M2A0 = A
      ACM_M2B0 = B
      ACM_M2IN = MAX( -IRA__TWOPI, MIN( IRA__TWOPI, INCB ) )
      ACM_M2ID = IDA

*  If a blank SCS was given, use the value associated with IDA.
      IF( SCS .EQ. ' ' ) THEN
         ACM_M2SC = ACM_SCS( IDA )
      ELSE
         ACM_M2SC = SCS
      END IF

*  Set up a flag indicating if lines are to be drawn.
      IF( ACM_DROPT( 4 ) .GT. 0.0D0 ) THEN
         INK = .TRUE.
      ELSE
         INK = .FALSE.
      END IF

*  Set up the SGS pen specified by option PEN2.
      CALL SGS_IPEN( NPEN )
      CALL SGS_SPEN( NINT( ACM_DROPT( 8 ) ) )

*  Call IRA1_CURVE to draw the meridian section, using IRA1_MAP2 to
*  do the transformation from offset from the start of the curve to
*  image coordinates.
      CALL IRA1_CURVE( IRA1_MAP2, LBND( 1 ), UBND( 1 ), LBND( 2 ),
     :                 UBND( 2 ), INK, NINT( ACM_DROPT( 3 ) ),
     :                 IRA__MXBRK, ACM_OUT, ACM_BRK, ACM_VBRK,
     :                 ACM_NBRK, ACM_LENG, STATUS )

*  Reinstate the original SGS pen.
      CALL SGS_SPEN( NPEN )

*  If an error occurred, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_DRMER_ERR2',
     :  'IRA_DRMER: Error plotting a line of constant longitude.',
     :                 STATUS )

      END IF

      END
