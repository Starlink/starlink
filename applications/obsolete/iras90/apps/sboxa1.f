      SUBROUTINE SBOXA1( PLON1, PLAT1, PLON2, PLAT2, PXY1, PXY2, SKYCO,
     :                   CURSOR, OUTLIN, IDA, SCS, XC, YC, STATUS )
*+
*  Name:
*     SBOXA1

*  Purpose:
*     Find image coordinates for two opposite corners of a box
*     specified by its corners.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SBOXA1( PLON1, PLAT1, PLON2, PLAT2, PXY1, PXY2, SKYCO,
*                  CURSOR, OUTLIN, IDA, SCS, XC, YC, STATUS )

*  Description:
*     The image coordinates of two opposite corners of the box are
*     obtained using the cursor or parameter interface, depending on
*     the value of parameter CURSOR. 

*  Arguments:
*     PLON1 = CHARACTER * ( * ) (Given)
*        The parameter associated with the longitude at the first
*        corner.
*     PLAT1 = CHARACTER * ( * ) (Given)
*        The parameter associated with the latitude at the first
*        corner.
*     PLON2 = CHARACTER * ( * ) (Given)
*        The parameter associated with the longitude at the second
*        corner.
*     PLAT2 = CHARACTER * ( * ) (Given)
*        The parameter associated with the latitude at the second
*        corner.
*     PXY1 = CHARACTER * ( * ) (Given)
*        The parameter associated with the image coordinates at the
*        first box corner.
*     PXY2 = CHARACTER * ( * ) (Given)
*        The parameter associated with the image coordinates at the
*        second box corner.
*     SKYCO = LOGICAL (Given)
*        True if the values held in BOXSIZ are in arc-minutes.
*     CURSOR = LOGICAL (Given)
*        True if the cursor is to be used to get the coordinates of the
*        centre of the box. Otherwise the parameter system is used.
*     OUTLIN = LOGICAL (Given)
*        True if markers are to be drawn at the selected cursor
*        positions.
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     SCS = CHARACTER * ( * ) (Given)
*        The Sky Coordinate System to use.
*     XC( 2 ) = REAL (Returned)
*        The X image coordinates of the two opposite corners.
*     YC( 2 ) = REAL (Returned)
*        The Y image coordinates of the two opposite corners.
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
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      CHARACTER PLON1*(*)
      CHARACTER PLAT1*(*)
      CHARACTER PLON2*(*)
      CHARACTER PLAT2*(*)
      CHARACTER PXY1*(*)
      CHARACTER PXY2*(*)
      LOGICAL SKYCO
      LOGICAL CURSOR
      LOGICAL OUTLIN
      INTEGER IDA
      CHARACTER SCS*(*)

*  Arguments Returned:
      REAL XC( 2 )
      REAL YC( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :        AA( 2 ),           ! Longitude values
     :        BB( 2 ),           ! Latitude values
     :        XX( 2 ),           ! Image coordinate X values
     :        YY( 2 )            ! Image coordinate Y values

      REAL
     :        XYC1( 2 ),         ! Image coordinates of first corner
     :        XYC2( 2 )          ! Image coordinates of second corner
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the cursor is to be used to get the position of the box corners...
      IF( CURSOR ) THEN

*  Display instructions describing the use of the cursor.
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_OUTIF( MSG__NORM, 'SBOXA1_MSG1',
     :     '  Position the cursor at a corner of the box and press '//
     :     'any key ', STATUS )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get the position of the first corner from the cursor.
         XC( 1 ) = VAL__BADR
         CALL SBOXB0( OUTLIN, XC( 1 ), YC( 1 ), STATUS )

*  Display instructions describing the use of the cursor.
         CALL MSG_OUTIF( MSG__NORM, 'SBOXA1_MSG2',
     :     '  Position the cursor at the opposite corner of the box '//
     :     'and press any key ', STATUS )
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get the position of the second corner from the cursor.
         CALL SBOXB0( OUTLIN, XC( 2 ), YC( 2 ), STATUS )

*  If the parameter interface is to be used to get the position of the
*  corners...
      ELSE

*  If the user wants to give sky coordinates, use IRA.
         IF( SKYCO ) THEN
            CALL IRA_GETCO( PLON1, PLAT1, ' of a corner of the box',
     :                      SCS, .FALSE., AA( 1 ), BB( 1 ), STATUS )

            CALL IRA_GETCO( PLON2, PLAT2,
     :                      ' of the opposite corner of the box',
     :                      SCS, .FALSE., AA( 2 ), BB( 2 ), STATUS )

*  Convert the sky coordinates to image coordinates.
            CALL IRA_TRANS( 2, AA, BB, .FALSE., SCS, IDA, XX, YY,
     :                      STATUS )

*  Store the single precision image coordinates at the corners.
            IF( XX( 1 ) .EQ. VAL__BADD .OR.
     :          YY( 1 ) .EQ. VAL__BADD ) THEN
               XC( 1 ) = VAL__BADR
               YC( 1 ) = VAL__BADR
            ELSE
               XC( 1 ) = REAL( XX( 1 ) )
               YC( 1 ) = REAL( YY( 1 ) )
            END IF

            IF( XX( 2 ) .EQ. VAL__BADD .OR.
     :          YY( 2 ) .EQ. VAL__BADD ) THEN
               XC( 2 ) = VAL__BADR
               YC( 2 ) = VAL__BADR
            ELSE
               XC( 2 ) = REAL( XX( 2 ) )
               YC( 2 ) = REAL( YY( 2 ) )
            END IF

*  If the user wants to give image coordinates, use PAR.
         ELSE
            CALL PAR_EXACR( PXY1, 2, XYC1, STATUS )
            CALL PAR_EXACR( PXY2, 2, XYC2, STATUS )

*  Return the corner coordinates.
            XC( 1 ) = XYC1( 1 )
            YC( 1 ) = XYC1( 2 )
            XC( 2 ) = XYC2( 1 )
            YC( 2 ) = XYC2( 2 )

         END IF

      END IF

 999  CONTINUE

      END
