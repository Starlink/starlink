      SUBROUTINE SWRIB0( PLON, PLAT, IRA, SCS, PTXT, MXNTXT, LON,
     :                   LAT, TXT, NTXT, STATUS )
*+
*  Name:
*     SWRIB0

*  Purpose:
*     Write texts at specified sky positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRIB0( PLON, PLAT, IRA, SCS, PTXT, MXNTXT, LON, LAT
*                  TXT, NTXT, STATUS )

*  Description:
*     This subroutine writes the text strings to the specified sky
*     positions in the current SGS zone. The user will be prompted for
*     the next position after each writting until a null is given.

*  Arguments:
*     PLON = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the longitude of the sky
*        positions from the user.
*     PLAT = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the latitude of the sky
*        positions from the user.
*     IRA = INTEGER (Given)
*        The ID of the IRA system of the displayed image NDF
*     SCS = CHARACTER*( * ) (Given)
*        The name of the sky coordinate system under which the sky
*        position is specified.
*     PTXT = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the text from the user.
*     MXNTXT = INTEGER (Given)
*        The max. number of text can be written to the image.
*     LON( MXNTXT ) = DOUBLE PRECISION (Given and Returned)
*        The longitude of the sky positions at which the texts are
*        written.
*     LAT( MXNTXT ) = REAL (Given and Returned)
*        The latitude of the sky positions at which the texts are
*        written.
*     TXT( MXNTXT ) = CHARACTER*( * ) (Given and Returned)
*        The text string written to the image.
*     NTXT = INTEGER (Given and Returned)
*        The number of texts having been written to the image
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     {original_version_entry}

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
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Arguments Given:
      CHARACTER*( * ) PLON, PLAT
      INTEGER IRA
      CHARACTER*( * ) SCS
      CHARACTER*( * ) PTXT
      INTEGER MXNTXT

*  Arguments Given and Returned:
      DOUBLE PRECISION LON( MXNTXT ), LAT( MXNTXT )
      CHARACTER*( * ) TXT( MXNTXT )
      INTEGER NTXT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER ATEXT*(IRA__SZFSC)! Longitude text
      CHARACTER BTEXT*(IRA__SZFSC)! Latitude text
      LOGICAL EXIT               ! Exit loop flag
      DOUBLE PRECISION X, Y      ! Image coordinate of the position
      INTEGER TXTLN              ! Used length of a text string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Enter a do loop until exit is required, or the number of the writting
*  exceeds the uplimit, or something goes wrong.
      EXIT = .FALSE.
      DO WHILE ( .NOT.EXIT .AND. NTXT .LT. MXNTXT .AND.
     :            STATUS .EQ. SAI__OK )

*  Get a sky position form the user.
         CALL IRA_GETCO( PLON, PLAT, ' of the position', SCS, .TRUE.,
     :                   LON( NTXT + 1 ), LAT( NTXT + 1 ), STATUS )

*  If null is obtained, set exit flag and annul the status.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            EXIT = .TRUE.
            CALL ERR_ANNUL( STATUS )

*  If a valid position is obtained, write the text to the position.
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Get its image coordinate.
            CALL IRA_TRANS( 1, LON( NTXT + 1 ), LAT( NTXT + 1 ),
     :                     .FALSE., SCS, IRA, X, Y, STATUS )

*  If the converted coordinates are bad, give a warning message.
            IF( X .EQ. VAL__BADD .OR. Y .EQ. VAL__BADD ) THEN
               CALL IRA_DTOC( LON( NTXT + 1 ), LAT( NTXT + 1 ), SCS, 1,
     :                        ATEXT, BTEXT, STATUS )
               CALL MSG_SETC( 'A', ATEXT )
               CALL MSG_SETC( 'B', BTEXT )
               CALL MSG_OUTIF( MSG__NORM, 'SWRIB0_MSG1',
     :   '  Position ^A, ^B cannot be transformed to image coordinates',
     :                         STATUS )

*  Get a text string from the user.
            ELSE
               CALL PAR_GET0C( PTXT, TXT( NTXT + 1 ), STATUS )

*  If null is supplied, no text will be written at this position but not
*  exit.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )

*  If a text string is obtained, write the text to the SGS zone.
               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  NTXT = NTXT + 1
                  TXTLN = CHR_LEN( TXT( NTXT ) )
                  CALL SGS_TX( REAL( X ), REAL( Y ),
     :                         TXT( NTXT )( : TXTLN ) )
                  CALL SGS_FLUSH

*  See if GKS/SGS has reported an error.
                  CALL GKS_GSTAT( STATUS )

               END IF

            END IF

*  Cancel the value of parameter for next use.
            CALL PAR_CANCL( PTXT, STATUS )
         END IF

*  Cancel the values of the parameters for next use
         CALL PAR_CANCL( PLON, STATUS )
         CALL PAR_CANCL( PLAT, STATUS )
      END DO

      END
