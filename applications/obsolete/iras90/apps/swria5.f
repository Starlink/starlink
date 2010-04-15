      SUBROUTINE SWRIA5( NTXT, IRA, SCS, XDIM, LON, LAT, TXT, DIRX,
     :                   DIRY, HEIGHT, RATIO, JSTFCT, SPACE, FONT, PEN,
     :                   STATUS )
*+
*  Name:
*     SWRIA5

*  Purpose:
*     Write texts stored in arrays onto current zone

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRIA5( NTXT, IRA, SCS, XDIM, LON, LAT, TXT, DIRX, DIRY,
*                  HEIGHT, RATIO, JSTFCT, SPACE, FONT, PEN, STATUS )

*  Description:
*     This subroutine is used by the application SKYWRITE to write the
*     texts specified in arrays onto the current SGS zone.

*  Arguments:
*     NTXT = INTEGER (Given)
*        The number of the texts to be written onto the current zone.
*     IRA = INTEGER (Given)
*        The IRA id of the image displayed.
*     SCS = CHARACTER (Given)
*        The name of the sky coordinate system in use.
*     XDIM = REAL (Given)
*        The X size of the displayed image.
*     LON( NTXT ), LAT( NTXT ) = REAL (Given)
*        The sky coordinates of the positions at which the texts are to
*        be written.
*     TXT( NTXT ) = CHARACTER (Given)
*        The text strings to be written.
*     DIRX( NTXT ), DIRY( NTXT ) = REAL (Given)
*        The up direction of the texts.
*     HEIGHT( NTXT ) = REAL (Given)
*        The height of the texts.
*     RATIO( NTXT ) = REAL (Given)
*        The aspect ratio of the texts.
*     JSTFCT( NTXT ) = CHARACTER*( * ) (Given)
*        The justification of the texts.
*     SPACE( NTXT ) = REAL (Given)
*        The space between characters in texts.
*     FONT( NTXT ) = INTEGER (Given)
*        The font of the texts.
*     PEN( NTXT ) = INTEGER (Given)
*        The pen number of the texts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     1-SEP-1992 (WG):
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
      INTEGER NTXT
      INTEGER IRA
      CHARACTER*( * ) SCS
      REAL XDIM
      DOUBLE PRECISION LON( NTXT ), LAT( NTXT )
      CHARACTER*( * ) TXT( NTXT )
      REAL DIRX( NTXT ), DIRY( NTXT )
      REAL HEIGHT( NTXT ), RATIO( NTXT )
      CHARACTER*( * ) JSTFCT( NTXT )
      REAL SPACE( NTXT )
      INTEGER FONT( NTXT ), PEN( NTXT )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER ATEXT*(IRA__SZFSC)! Longitude text
      CHARACTER BTEXT*(IRA__SZFSC)! Latitude text
      INTEGER I                  ! Do loop index
      INTEGER TXTLN              ! Used length of a text string
      DOUBLE PRECISION X, Y      ! Image coordinate of a position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the text onto the zone one by one.
      DO I = 1, NTXT

*  Set text attributes first.
         CALL SGS_SUPTX( DIRX( I ), DIRY( I ) )
         CALL SGS_SHTX( HEIGHT( I ) * XDIM )
         CALL SGS_SARTX( RATIO( I ) )
         CALL SGS_STXJ( JSTFCT( I )( : 2 ) )
         CALL SGS_SSPTX( SPACE( I ) )
         CALL SGS_SFONT( FONT( I ) )
         CALL SGS_SPEN( PEN( I ) )

*  Check to see if GKS/SGS has reported an error.
         CALL GKS_GSTAT( STATUS )

*  Find the image coordinate of the position.
         CALL IRA_TRANS( 1, LON( I ), LAT( I ), .FALSE., SCS, IRA,
     :                   X, Y, STATUS )

*  If the converted coordinates are bad, give a warning message.
         IF( X .EQ. VAL__BADD .OR. Y .EQ. VAL__BADD ) THEN
            CALL IRA_DTOC( LON( NTXT + 1 ), LAT( NTXT + 1 ), SCS, 1,
     :                     ATEXT, BTEXT, STATUS )
            CALL MSG_SETC( 'A', ATEXT )
            CALL MSG_SETC( 'B', BTEXT )
            CALL MSG_OUTIF( MSG__NORM, 'SWRIA5_MSG1',
     :   '  Position ^A, ^B cannot be transformed to image coordinates',
     :                         STATUS )

*  Write the text to the position.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            TXTLN = CHR_LEN( TXT( I ) )
            CALL SGS_TX( REAL( X ), REAL( Y ), TXT( I )( : TXTLN ) )

*  Check to see if GKS/SGS has reported an error.
            CALL GKS_GSTAT( STATUS )

         END IF

*  If anything goes wrong, exit
         IF ( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Flush out the buffers.
      CALL SGS_FLUSH

 999  CONTINUE

      END
