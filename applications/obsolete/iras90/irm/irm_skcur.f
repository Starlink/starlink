      SUBROUTINE IRM_SKCUR( IDA, SCS, LBND, UBND, LON, LAT, OUT,
     :                      STATUS )
*+
*  Name:
*     IRM_SKCUR

*  Purpose:
*     Get the sky coordinate of the cursor position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_SKCUR( IDA, SCS, LBND, UBND, LON, LAT, OUT, STATUS )

*  Description:
*     This subroutine is used to get the sky coordinate of the cursor
*     position over the current SGS zone. A cursor should be available
*     on the graphic device in use, otherwise an error is reported. If
*     the selected cursor position is outside a specified area of the
*     SGS zone, a flag will be set and the output sky coordinates will
*     be the starlink bad value (VAL__BADD).
*
*     SGS must previously have been activated.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system in which LON and LAT are required.
*     LBND( 2 ) = REAL (Given)
*        The lower bounds of the area in which cursor positions are
*        consider valid, in image coordinates.
*     UBND( 2 ) = REAL (Given)
*        The upper bounds of the area in which cursor positions are
*        consider valid, in image coordinates.
*     LON = DOUBLE PRECISION (Returned)
*        The longitude of the cursor position.
*     LAT = DOUBLE PRECISION (Returned)
*        The latitude of the cursor position.
*     OUT = LOGICAL (Returned)
*        Returned true if the selected cursor position is outside the
*        area specified by LBND and UBND.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-FEB-1993 (WG):
*        Original version.
*     10-FEB-1993 (DSB):
*        Modified for inclusion in IRM.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      INTEGER IDA
      CHARACTER*( * ) SCS
      REAL LBND( 2 )
      REAL UBND( 2 )

*  Arguments Returned:
      DOUBLE PRECISION LON
      DOUBLE PRECISION LAT
      LOGICAL OUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NKEY               ! Number of the key pressed

      LOGICAL CURSOR             ! Cursor availability flag

      REAL X                     ! X position of a cursor position
      REAL Y                     ! Y position of a cursor position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the cursor is not available, set status, report and exit.
      CALL SGS_ICUAV( CURSOR )
      IF ( .NOT.CURSOR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_SKCUR_ERR1',
     :      'IRM_SKCUR: No cursor is available on the graphics device.',
     :                 STATUS )
         GO TO 999
      END IF

*  Enable cursor.
      CALL SGS_CUVIS( .TRUE. )

*  Get the image coordinate of the cursor position.
      CALL SGS_REQCU( X, Y, NKEY )

*  Find the sky coordinate of the image
      IF ( X .GE. LBND( 1 ) .AND. X .LE. UBND( 1 ) .AND.
     :     Y .GE. LBND( 2 ) .AND. Y .LE. UBND( 2 ) ) THEN
         OUT = .FALSE.
         CALL IRA_TRANS( 1, DBLE( X ), DBLE( Y ), .TRUE., SCS, IDA,
     :                   LON, LAT, STATUS )

*  If the position is outside the image set the flag.
      ELSE
         OUT = .TRUE.
         LON = VAL__BADD
         LAT = VAL__BADD
      END IF

*  If an error occurred, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRM_SKCUR_ERR2',
     :     'IRM_SKCUR: Unable to get the sky coordinates of a cursor '//
     :     'position.', STATUS )
         GO TO 999
      END IF

      END
