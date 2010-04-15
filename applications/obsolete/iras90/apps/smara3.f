      SUBROUTINE SMARA3( NPNT, X, Y, LON, LAT, SCS, FID, STATUS )
*+
*  Name:
*     SMARA3

*  Purpose:
*     Write sky coordinates to a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SMARA3( NPNT, X, Y, LON, LAT, SCS, FID, STATUS )

*  Description:
*     This subroutine is used by application SKYMARK to write sky
*     coordinate to a text in the IRAS90 position exchange format.

*  Arguments:
*     NPNT = INTEGER (Given)
*        Number of sky coordinates to be written to the text file.
*     X( NPNT ), Y( NPNT ) = DOUBLE PRECISION (Given)
*        The image coordinates of the positions specified by their sky
*        coordinates.
*     LON( NPNT ), LAT( NPNT ) = DOUBLE PRECISION (Given)
*        The sky coordinates to be written to the text file.
*     SCS = CHARACTER
*        The name of the sky coordinates system in which the sky
*        coordiantes are given.
*     FID = INTEGER (Given)
*        The ID of the text file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     22-JUN-1992 (WG):
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
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants

*  Arguments Given:
      INTEGER NPNT
      DOUBLE PRECISION X( NPNT )
      DOUBLE PRECISION Y( NPNT )
      DOUBLE PRECISION LON( NPNT )
      DOUBLE PRECISION LAT( NPNT )
      CHARACTER SCS*(*)
      INTEGER FID

*  Status:
      INTEGER STATUS             ! Global status

*  External References:

*  Local Constants:
      INTEGER SZFSC              ! Size of a style 2 formatted sky
      PARAMETER ( SZFSC = 15 )   ! coordinate

*  Local Variables:
      CHARACTER BUF*80           ! Output buffer
      CHARACTER LONSTR*(IRA__SZFSC)! Longitude string
      CHARACTER LATSTR*(IRA__SZFSC)! Latitude string

      INTEGER I                  ! Do loop index
      INTEGER BLEN               ! Used length of BUF.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the name of the sky coordinate system as the first record.
      CALL FIO_WRITE( FID, SCS, STATUS )
      CALL FIO_WRITE( FID, ' ', STATUS )

*  Write all positions one by one.
      DO I = 1, NPNT

*  Only do this point if all four coordinate values are good.
         IF( LON( I ) .NE. VAL__BADD .AND. LAT( I ) .NE. VAL__BADD .AND.
     :       X( I ) .NE. VAL__BADD .AND. Y( I ) .NE. VAL__BADD ) THEN

*  Convert sky coordinates into string form.
            CALL IRA_DTOC( LON( I ), LAT( I ), SCS, 0, LONSTR, LATSTR,
     :                     STATUS )

*  Assign the image coordinate values to message tokens.
            CALL MSG_FMTR( 'X', 'F7.1', REAL( X( I ) ) )
            CALL MSG_FMTR( 'Y', 'F7.1', REAL( Y( I ) ) )

*  Create a text string holding the coordinate values, and a comment
*  giving the corresponding image coordinates.
            BUF = ' '
            BUF( 3: ) = LONSTR
            BUF( 6 + SZFSC: ) = LATSTR
            CALL MSG_LOAD( ' ', '#  =  ^X, ^Y',
     :                     BUF( 9 + 2*SZFSC: ), BLEN, STATUS )

*  Write the message to the log file.
            CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN + 9 + 2*SZFSC) ),
     :                      STATUS )

*  If any of the coordinates are bad, write a warning message to the
*  log file.
         ELSE
            CALL FIO_WRITE( FID,'   (unusable coordinates ignored)',
     :                      STATUS )
         END IF

      END DO

      END
