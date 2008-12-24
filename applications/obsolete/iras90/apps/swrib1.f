      SUBROUTINE SWRIB1( LBND, UBND, IRA, SCS, PTXT, MXNTXT, LON, LAT,
     :                   TXT, NTXT, STATUS )
*+
*  Name:
*     SWRIB1

*  Purpose:
*     Write texts at the cursor positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRIB1( LBND, UBND, IRA, SCS, PTXT, MXNTXT, LON, LAT,
*                  TXT, NTXT, STATUS )

*  Description:
*     This subroutine writes the text strings to the cursor position
*     in the current GSG zone. The user will be prompted for the next
*     position after each writting until the cursor is outside the zone.

*  Arguments:
*     LBND( 2 ), UBND( 2 ) = REAL (Given)
*        Extension of the current SGS zone.
*     IRA = INTEGER (Given)
*        The ID of the IRA system of the displayed image NDF.
*     SCS = CHARACTER*( * ) (Given)
*        The name of the sky coordinate system under which the sky
*        position is specified.
*     PTXT = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the text from the user.
*     MXNTXT = INTEGER (Given)
*        The max. number of text can be written to the image.
*     LON( MXNTXT ) = REAL (Given and Returned)
*        The X coordinates of the image positions at which the texts are
*        written.
*     LAT( MXNTXT ) = REAL (Given and Returned)
*        The Y coordinates of the image positions at which the texts are
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
*     26-AUG-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      REAL LBND( 2 ), UBND( 2 )
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
      LOGICAL OUT                ! Position outside image flag
      DOUBLE PRECISION X, Y      ! Image coordinate of the position
      INTEGER TXTLN              ! Used length of a text string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initially put the cursor at the centre of the image.
      CALL SGS_SETCU( 0.5 * ( LBND( 1 ) + UBND( 1 ) ) ,
     :                0.5 * ( LBND( 2 ) + UBND( 2 ) ) )

*  See if GKS/SGS has reported an error.
      CALL GKS_GSTAT( STATUS )

*  Write help messages.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      CALL MSG_OUTIF( MSG__NORM, 'SWRIB1_MSG1',
     :       '  Position the cursor at a text position and press any '//
     :   'button (position the cursor well outside the image to exit).',
     :                STATUS )

*  Enter a do loop until exit is required, or the number of the writting
*  exceeds the uplimit, or something goes wrong.
      EXIT = .FALSE.
      DO WHILE ( .NOT.EXIT .AND. NTXT .LT. MXNTXT .AND.
     :            STATUS .EQ. SAI__OK )

*  Get a position via cursor.
         CALL IRM_SKCUR( IRA, SCS, LBND, UBND, LON( NTXT + 1 ),
     :                   LAT( NTXT + 1 ), OUT, STATUS )

*  If the position is outside the image, set exit flag.
         IF ( OUT ) THEN
            EXIT = .TRUE.

*  Otherwide, get the image coordinate of the position.
         ELSE
            CALL IRA_TRANS( 1, LON( NTXT + 1 ), LAT( NTXT + 1 ),
     :                     .FALSE., SCS, IRA, X, Y, STATUS )

*  If the converted coordinates are bad, give a warning message.
            IF( X .EQ. VAL__BADD .OR. Y .EQ. VAL__BADD ) THEN
               CALL IRA_DTOC( LON( NTXT + 1 ), LAT( NTXT + 1 ), SCS, 1,
     :                        ATEXT, BTEXT, STATUS )
               CALL MSG_SETC( 'A', ATEXT )
               CALL MSG_SETC( 'B', BTEXT )
               CALL MSG_OUTIF( MSG__NORM, 'SWRIB1_MSG1',
     :   '  Position ^A, ^B cannot be transformed to image coordinates',
     :                         STATUS )

*  Get a text string from the user.
            ELSE 
               CALL PAR_GET0C( PTXT, TXT( NTXT + 1 ), STATUS )

*  If null is supplied, no text will be written at this position but not
*  exit.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
      
*  Or if a valid string is obtained, get its used length.
               ELSE IF ( STATUS .EQ. SAI__OK ) THEN
                  NTXT = NTXT + 1
                  TXTLN = CHR_LEN( TXT( NTXT ) )

*  Write the text to the SGS zone.
                  IF( TXTLN .GT. 0 ) THEN
                     CALL SGS_TX( REAL( X ), REAL( Y ),
     :                            TXT( NTXT )( : TXTLN ) )
                     CALL SGS_FLUSH

*  See if GKS/SGS has reported an error.
                     CALL GKS_GSTAT( STATUS )

                  END IF

               END IF

*  Cancel the value of parameter for next use.
               CALL PAR_CANCL( PTXT, STATUS )

            END IF

         END IF

      END DO

      END
