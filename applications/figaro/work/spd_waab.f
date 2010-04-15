      SUBROUTINE SPD_WAAB( LEFT, RIGHT, BOTTOM, TOP, STATUS )
*+
*  Name:
*     SPD_WAAB

*  Purpose:
*     Find AGI picture data range.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WAAB( LEFT, RIGHT, BOTTOM, TOP, STATUS )

*  Description:
*     This routine will investigate the current AGI picture. The picture
*     is supposed to have data coordinates that are linear on the plot
*     device and have axes parallel to the horizontal and vertical
*     direction on the plot device. If this is not the case, an error
*     report is made and a bad status returned. If this is the case,
*     then the four returned arguments are the left x data coordinate,
*     the right x data coordinate, the bottom y data coordinate, and the
*     top y data coordinate.
*
*     This routine thus checks whether an AGI picture is suitable as a
*     PGPLOT view port and returns what should be made the plot window.
*     This is of relevance when an overlay on a previous plot is to be
*     made.

*  Arguments:
*     LEFT = REAL (Returned)
*        If the returned status is SAI__OK, this is the first data
*        coordinate of the left edge of the picture.
*     RIGHT = REAL (Returned)
*        If the returned status is SAI__OK, this is the first data
*        coordinate of the right edge of the picture.
*     BOTTOM = REAL (Returned)
*        If the returned status is SAI__OK, this is the second data
*        coordinate of the bottom edge of the picture.
*     TOP = REAL (Returned)
*        If the returned status is SAI__OK, this is the second data
*        coordinate of the top edge of the picture.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Linearity and diagonality are checked by subjecting two test lines
*     to the AGI picture's transform. The picture must have world
*     coordinates (in AGI speak) that run linearly increasing to the
*     right or top. It also may have a transform to data coordinates (in
*     AGI speak), which need not be linear, diagonal, or bijective. We
*     require these data coordinates to be linear and diagonal (not
*     rotated). We do not require that they increase to the right or
*     top.
*
*     To check the validity of the transform, the four edges of the
*     picture are subjected to the transform. Each edge is sampled
*     linearly at 101 points in the range of the picture's world
*     coordinates. The transformed points must be linear in one
*     coordinate and constant in the other.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     21 Sep 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Returned:
      REAL LEFT, RIGHT, BOTTOM, TOP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER PICRES             ! No. of point to sample edge
      PARAMETER ( PICRES = 101 )
      REAL EPS                   ! Tolerance for coordinate linearity
      PARAMETER ( EPS = 1E-5 )

*  Local Variables:
      LOGICAL LINEAR             ! Result of linearity check
      INTEGER PICID              ! AGI identifier of the current picture
      REAL TEMP1, TEMP2          ! Temporary numbers
      REAL WX1, WX2, WY1, WY2    ! Picture world range
      REAL WX( PICRES )          ! World x
      REAL WY( PICRES )          ! World y
      REAL DX( PICRES )          ! Data x
      REAL DY( PICRES )          ! Data y

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out picture ID and world range.
      CALL AGI_ICURP( PICID, STATUS )
      CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  UAAF sets an array constant,
*  UAAJ makes an array linear,
*  UAAA finds array extrema,
*  UAAH checks array linearity.

*  Check left edge.
      CALL SPD_UAAFR( 1, PICRES, WX, WX1, STATUS )
      CALL SPD_UAAJR( WY1, WY2, PICRES, WY, STATUS )
      CALL AGI_TWTOD( PICID, PICRES, WX, WY, DX, DY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL SPD_UAAAR( .FALSE., PICRES, DX, TEMP1, TEMP2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( ABS(TEMP2-TEMP1) .GT. EPS*(ABS(TEMP2)+ABS(TEMP1)) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAB_E01',
     :      'SPD_WAAB: Error: Axis transforms are not independent.',
     :      STATUS )
         GO TO 500
      END IF
      LEFT = TEMP1
      CALL SPD_UAAHR( PICRES, DY, EPS, TEMP1, TEMP2, LINEAR, STATUS )
      IF ( .NOT. LINEAR .OR. TEMP1 .EQ. TEMP2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAB_E02',
     :      'SPD_WAAB: Error: At least one axis is not linear.',
     :      STATUS )
         GO TO 500
      END IF

*  Check right edge.
      CALL SPD_UAAFR( 1, PICRES, WX, WX2, STATUS )
*     CALL SPD_UAAJR( WY1, WY2, PICRES, WY, STATUS )
      CALL AGI_TWTOD( PICID, PICRES, WX, WY, DX, DY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL SPD_UAAAR( .FALSE., PICRES, DX, TEMP1, TEMP2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( ABS(TEMP2-TEMP1) .GT. EPS*(ABS(TEMP2)+ABS(TEMP1)) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAB_E01',
     :      'SPD_WAAB: Error: Axis transforms are not independent.',
     :      STATUS )
         GO TO 500
      END IF
      RIGHT = TEMP1
      CALL SPD_UAAHR( PICRES, DY, EPS, TEMP1, TEMP2, LINEAR, STATUS )
      IF ( .NOT. LINEAR .OR. TEMP1 .EQ. TEMP2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAB_E02',
     :      'SPD_WAAB: Error: At least one axis is not linear.',
     :      STATUS )
         GO TO 500
      END IF

*  Check bottom edge.
      CALL SPD_UAAJR( WX1, WX2, PICRES, WX, STATUS )
      CALL SPD_UAAFR( 1, PICRES, WY, WY1, STATUS )
      CALL AGI_TWTOD( PICID, PICRES, WX, WY, DX, DY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL SPD_UAAHR( PICRES, DX, EPS, TEMP1, TEMP2, LINEAR, STATUS )
      IF ( .NOT. LINEAR .OR. TEMP1 .EQ. TEMP2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAB_E02',
     :      'SPD_WAAB: Error: At least one axis is not linear.',
     :      STATUS )
         GO TO 500
      END IF
      CALL SPD_UAAAR( .FALSE., PICRES, DY, TEMP1, TEMP2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( ABS(TEMP2-TEMP1) .GT. EPS*(ABS(TEMP2)+ABS(TEMP1)) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAB_E01',
     :      'SPD_WAAB: Error: Axis transforms are not independent.',
     :      STATUS )
         GO TO 500
      END IF
      BOTTOM = TEMP1

*  Check top edge.
*     CALL SPD_UAAJR( WX1, WX2, PICRES, WX, STATUS )
      CALL SPD_UAAFR( 1, PICRES, WY, WY2, STATUS )
      CALL AGI_TWTOD( PICID, PICRES, WX, WY, DX, DY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL SPD_UAAHR( PICRES, DX, EPS, TEMP1, TEMP2, LINEAR, STATUS )
      IF ( .NOT. LINEAR .OR. TEMP1 .EQ. TEMP2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAB_E02',
     :      'SPD_WAAB: Error: At least one axis is not linear.',
     :      STATUS )
         GO TO 500
      END IF
      CALL SPD_UAAAR( .FALSE., PICRES, DY, TEMP1, TEMP2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( ABS(TEMP2-TEMP1) .GT. EPS*(ABS(TEMP2)+ABS(TEMP1)) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAB_E01',
     :      'SPD_WAAB: Error: Axis transforms are not independent.',
     :      STATUS )
         GO TO 500
      END IF
      TOP = TEMP1

*  Return.
 500  CONTINUE
      END
