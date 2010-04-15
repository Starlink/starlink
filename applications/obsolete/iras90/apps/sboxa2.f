      SUBROUTINE SBOXA2( IDA, SCS, XC, YC, STATUS )
*+
*  Name:
*     SBOXA2

*  Purpose:
*     Calculate and display the geometry of the box.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SBOXA2( IDA, SCS, XC, YC, STATUS )

*  Description:
*     The centre of the box is found as the mean of the two supplied
*     corner positions (in image coordinates). The sky coordinates of
*     all four corners and the centre are then found and the size of the
*     box in arc-minutes is found by calculating the arc-length between
*     the mid points of opposite edges of the box. The information is
*     then displayed.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system to use.
*     XC( 2 ) = REAL (Given)
*        The X image coordinates of both corners of the box.
*     YC( 2 ) = REAL (Given)
*        The Y image coordinates of both corners of the box.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1993 (DSB):
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
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants

*  Arguments Given:
      INTEGER IDA
      CHARACTER SCS*(*)
      REAL XC( 2 )
      REAL YC( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        ATEXT*(IRA__SZFSC),! Formatted longitude value
     :        BTEXT*(IRA__SZFSC),! Formatted latitude value
     :        CNAME( 6 )*20      ! Names of each point in box.

      DOUBLE PRECISION
     :        AA( 9 ),           ! Latitude values
     :        BB( 9 ),           ! Longitude values
     :        DISTX,             ! Arc-length between edge mid-points
     :        DISTY,             ! Arc-length between edge mid-points
     :        XX( 9 ),           ! X image coordinate values
     :        YY( 9 )            ! Y image coordinate values

      INTEGER
     :        I                  ! Loop count

*  Local Data:
      DATA CNAME / 'Bottom left corner :', 'Top left corner    :',
     :             'Top right corner   :', 'Bottom right corner:',
     :             'Centre             :', 'Extent             :' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if any of the corner positions are bad.
      IF( XC( 1 ) .EQ. VAL__BADR .OR. YC( 1 ) .EQ. VAL__BADR .OR.
     :    XC( 2 ) .EQ. VAL__BADR .OR. YC( 2 ) .EQ. VAL__BADR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SBOXA2_ERR1',
     :       'SBOXA2: At least one corner has no corresponding pixel '//
     :       'coordinates', STATUS )
         GO TO 999
      END IF

*  Store the double precision image coordinates at all four corners
*  (indices 1 to 4), the centre of the box (index 5), and each edge
*  (indices 6 to 9).
      XX( 1 ) = DBLE( MIN( XC( 1 ), XC( 2 ) ) )
      XX( 2 ) = XX( 1 )
      XX( 3 ) = DBLE( MAX( XC( 1 ), XC( 2 ) ) )
      XX( 4 ) = XX( 3 )
      XX( 5 ) = 0.5D0*( XX( 1 ) + XX( 3 ) )
      XX( 6 ) = XX( 1 )
      XX( 7 ) = XX( 5 )
      XX( 8 ) = XX( 3 )
      XX( 9 ) = XX( 5 )

      YY( 1 ) = DBLE( MIN( YC( 1 ), YC( 2 ) ) )
      YY( 2 ) = DBLE( MAX( YC( 1 ), YC( 2 ) ) )
      YY( 3 ) = YY( 2 )
      YY( 4 ) = YY( 1 )
      YY( 5 ) = 0.5D0*( YY( 1 ) + YY( 2 ) )
      YY( 6 ) = YY( 5 )
      YY( 7 ) = YY( 2 )
      YY( 8 ) = YY( 5 )
      YY( 9 ) = YY( 1 )

*  Convert to sky coordinates.
      CALL IRA_TRANS( 9, XX, YY, .TRUE., SCS, IDA, AA, BB, STATUS )

*  Describe the box in pixel coordinates.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      CALL MSG_OUTIF( MSG__NORM, 'SBOXA2_MSG1', '  Pixel coordinates:',
     :                STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

      CALL MSG_SETR( 'X', REAL( XX( 1 ) ) )
      CALL MSG_SETR( 'Y', REAL( YY( 1 ) ) )
      CALL MSG_SETC( 'N', CNAME( 1 ) )
      CALL MSG_OUTIF( MSG__NORM, 'SBOXA2_MSG2',
     :                '    ^N   ^X,  ^Y', STATUS )

      CALL MSG_SETR( 'X', REAL( XX( 3 ) ) )
      CALL MSG_SETR( 'Y', REAL( YY( 3 ) ) )
      CALL MSG_SETC( 'N', CNAME( 3 ) )
      CALL MSG_OUTIF( MSG__NORM, 'SBOXA2_MSG3',
     :                '    ^N   ^X,  ^Y', STATUS )

      CALL MSG_SETR( 'X', REAL( XX( 5 ) ) )
      CALL MSG_SETR( 'Y', REAL( YY( 5 ) ) )
      CALL MSG_SETC( 'N', CNAME( 5 ) )
      CALL MSG_OUTIF( MSG__NORM, 'SBOXA2_MSG4',
     :                '    ^N   ^X,  ^Y', STATUS )

      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      CALL MSG_SETR( 'X', REAL( XX( 8 ) - XX( 6 ) ) )
      CALL MSG_SETR( 'Y', REAL( YY( 7 ) - YY( 9 ) ) )
      CALL MSG_SETC( 'N', CNAME( 6 ) )
      CALL MSG_OUTIF( MSG__NORM, 'SBOXA2_MSG5',
     :                '    ^N   ^X x ^Y pixels', STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )


      CALL MSG_SETI( 'X1', NINT( XX( 1 ) + 0.5D0 ) )
      CALL MSG_SETI( 'Y1', NINT( YY( 1 ) + 0.5D0 ) )
      CALL MSG_SETI( 'X3', NINT( XX( 3 ) + 0.5D0 ) )
      CALL MSG_SETI( 'Y3', NINT( YY( 3 ) + 0.5D0 ) )
      CALL MSG_OUTIF( MSG__NORM, 'SBOXA2_MSG6',
     :               '    Image section      :   ( ^X1:^X3, ^Y1:^Y3 )',
     :                STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )



*  Display the sky coordinates of each corner, and the centre.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      CALL MSG_SETC( 'SCS', SCS )
      CALL MSG_OUTIF( MSG__NORM, 'SBOXA2_MSG7',
     :                '  Sky coordinates (^SCS):', STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

      DO I = 1, 5
         CALL IRA_DTOC( AA( I ), BB( I ), SCS, 0, ATEXT, BTEXT, STATUS )
         IF( ATEXT .NE. ' ' .AND. BTEXT .NE. ' ' ) THEN

            CALL MSG_SETC( 'A', ATEXT )
            CALL MSG_SETC( 'B', BTEXT )
            CALL MSG_SETC( 'N', CNAME( I ) )
            CALL MSG_OUTIF( MSG__NORM, 'SBOXA2_MSG8',
     :                      '    ^N   ^A,  ^B', STATUS )
         ELSE

            CALL MSG_SETC( 'N', CNAME( I ) )
            CALL MSG_OUTIF( MSG__NORM, 'SBOXA2_MSG9',
     :                      '    ^N   (undefined)', STATUS )

         END IF

      END DO

*  Find the arc-length between the left and right hand edges.
      CALL IRA_DIST( AA( 6 ), BB( 6 ), AA( 8 ), BB( 8 ), DISTX, STATUS )

*  Find the arc-length between the top and bottom edges.
      CALL IRA_DIST( AA( 7 ), BB( 7 ), AA( 9 ), BB( 9 ), DISTY, STATUS )

*  Display the size of each dimension in arc-minutes.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

      IF( DISTX .NE. VAL__BADD .AND. DISTY .NE. VAL__BADD ) THEN
         CALL MSG_SETR( 'X', REAL( ABS( DISTX ) *IRA__R2AM ) )
         CALL MSG_SETR( 'Y', REAL( ABS( DISTY ) *IRA__R2AM ) )
         CALL MSG_SETC( 'N', CNAME( 6 ) )
         CALL MSG_OUTIF( MSG__NORM, 'SBOXA2_MSG10',
     :                   '    ^N   ^X x ^Y arc-minutes', STATUS )
      ELSE
         CALL MSG_SETC( 'N', CNAME( 6 ) )
         CALL MSG_OUTIF( MSG__NORM, 'SBOXA2_MSG11',
     :                   '    ^N   (undefined)', STATUS )
      ENDIF

      CALL MSG_BLANKIF( MSG__NORM, STATUS )

 999  CONTINUE

      END
