      SUBROUTINE TRACC1( A, B, X, Y, SCS, STATUS )
*+
*  Name:
*     TRACC1

*  Purpose:
*     Plot a pair of sky coordinate values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACC1( A, B, X, Y, SCS, STATUS )

*  Description:
*     This routine uses IRA_DRVAL to plot sky longitude and latitude
*     values. Each value is preceeded with an abbreviation of the
*     coordinate name such as "RA = ". The current text attributes are
*     used.

*  Arguments:
*     A = DOUBLE PRECISION (Given)
*        The sky longitude value.
*     B = DOUBLE PRECISION (Given)
*        The sky latitude value.
*     X = REAL (Given)
*        The world X coordinate of the top left corner of the box in
*        which the value are to be written.
*     Y = REAL (Given)
*        The world Y coordinate of the top left corner of the box in
*        which the value are to be written.
*     SCS = CHARACTER * ( * ) (Returned)
*        The sky coordinate system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA constants

*  Arguments Given:
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      REAL X
      REAL Y
      CHARACTER SCS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ABBREV*(IRA__SZSCA)! Abbreviation of axis name.
      CHARACTER DESCR*(IRA__SZSCD)! Full axis name.
      CHARACTER TXJ*2            ! SGS text justification.

      INTEGER LA                 ! Length of ABBREV.
      INTEGER LD                 ! Length of DESCR.
      INTEGER NC                 ! No. of characters in current text string.
      INTEGER NF                 ! Font no.
      INTEGER NPR                ! SGS precision.

      REAL AR                    ! Aspect ratio
      REAL DX                    ! Change in X along text string.
      REAL DY                    ! Change in Y along text string.
      REAL HT                    ! Text height.
      REAL SP                    ! Text spacing.
      REAL X1                    ! Lower X limit of SGS zone.
      REAL X2                    ! Upper X limit of SGS zone.
      REAL XM                    ! X size of SGS zone in metres.
      REAL XU                    ! X component of up vector.
      REAL XX                    ! X coordinate of end of text string.
      REAL Y1                    ! Lower Y limit of SGS zone.
      REAL Y2                    ! Upper Y limit of SGS zone.
      REAL YM                    ! Y size of SGS zone in metres.
      REAL YU                    ! Y component of up vector.
      REAL YY                    ! Y coordinate of end of text string.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the current text height.
      CALL SGS_ITXA( NF, NPR, HT, AR, XU, YU, SP, TXJ )

*  Set up a corresponding height for the coordinate values text.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )
      CALL IRA_DROPT( 'COORD_SIZE',
     :                DBLE( HT/MAX( ABS( X2 - X1 ), ABS( Y2 - Y1 ) ) ),
     :                STATUS )

*  Write the longitude abbreviation.
      CALL IRA_SCNAM( SCS, 1, DESCR, LD, ABBREV, LA, STATUS )
      CALL SGS_BTEXT( X, Y )
      CALL SGS_ATEXT( ABBREV( : LA )//' : ' )

*  Get the end position of the longitude abbreviation.
      CALL SGS_ITXB( XX, YY, NC, DX, DY )
      XX = XX + DX
      YY = YY + DY

*  Write out the longitude value.
      CALL IRA_DRVAL( A, SCS, 1, XX, YY, 2, IRA__AS2R, 'KEEP', STATUS )

*  Write the latitude abbreviation.
      CALL IRA_SCNAM( SCS, 2, DESCR, LD, ABBREV, LA, STATUS )
      CALL SGS_BTEXT( X, Y - 2.2*HT )
      CALL SGS_ATEXT( ABBREV( : LA )//' : ' )

*  Get the end position of the latitude abbreviation.
      CALL SGS_ITXB( XX, YY, NC, DX, DY )
      XX = XX + DX
      YY = YY + DY

*  Write out the latitude value.
      CALL IRA_DRVAL( B, SCS, 2, XX, YY, 2, IRA__AS2R, 'KEEP', STATUS )

      END
