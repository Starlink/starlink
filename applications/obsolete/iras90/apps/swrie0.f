      SUBROUTINE SWRIE0( FID, SCS, NTXT, LON, LAT, TXT, DIRX, DIRY,
     :                   HEIGHT, RATIO, JSTFCT, SPACE, FONT, PEN,
     :                   STATUS )
*+
*  Name:
*     SWRIE0

*  Purpose:
*     Save the current text written on to the image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRIE0( FID, SCS, NTXT, X, Y, TXT, DIRX, DIRY, HEIGHT, RATIO,
*                  JSTFCT, SPACE, FONT, TXPEN, STATUS )

*  Description:
*     This subroutine saves the current text written on to the displayed
*     image into a text file together with the attributes of the texts.

*  Arguments:
*     FID = INTEGER (Given)
*        The ID of the output file.
*     SCS = CHARACTER (Given)
*        The name of the sky coordinate system used.
*     NTXT = INTEGER (Given)
*        The number of texts has been written on to the iamge.
*     X( NTXT ), Y( NTXT ) = REAL (Given)
*        The image coordinates of each text on the image.
*     TXT( NTXT ) = CHARACTER*( * ) (Given)
*        Text strings written to the positions.
*     DIRX( NTXT ), DIRY( NTXT ) = REAL (Given)
*        The up direction vectors.
*     HEIGHT( NTXT ), RATIO( NTXT ) = REAL (Given)
*        The height and aspect ratio of the texts.
*     JSTFCT( NTXT ) = CHARACTER*( * ) (Given)
*        The justification of the texts.
*     SPACE( NTXT ) = REAL (Given)
*        The space between characters in texts.
*     FONT( NTXT ), PEN( NTXT ) = INTEGER (Given)
*        The font and pen number of the texts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     28-AUG-1992 (WG):
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
      INCLUDE 'SKY_PAR'          ! SKY_ constants

*  Arguments Given:
      INTEGER FID
      CHARACTER*( * ) SCS
      INTEGER NTXT
      DOUBLE PRECISION LON( NTXT ), LAT( NTXT )
      CHARACTER*( * ) TXT( NTXT )
      REAL DIRX( NTXT ), DIRY( NTXT ), HEIGHT( NTXT ), RATIO( NTXT )
      CHARACTER*( * ) JSTFCT( NTXT )
      REAL SPACE( NTXT )
      INTEGER FONT( NTXT ), PEN( NTXT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      INTEGER BLEN               ! Number of characters in a string
      CHARACTER BUF*255          ! Text buffer
      CHARACTER*( IRA__SZFSC ) LONST, LATST
                                 ! String form of a sky coordinate
      REAL SDIRX                 ! Current up-vector X component.
      REAL SDIRY                 ! Current up-vector Y component.
      INTEGER SFONT              ! Current font
      REAL SHGT                  ! Current text height.
      CHARACTER SJUST*2          ! Current text justidfication
      INTEGER SPEN               ! Current pen
      REAL SRAT                  ! Current text aspect ratio.
      REAL SSPACE                ! Current space between characters.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save a keyword to set up default text attributes.
      CALL FIO_WRITE( FID, 'DEFAULT', STATUS )
      CALL FIO_WRITE( FID, ' ', STATUS )

*  Set the current text attributes to the default values.
      SDIRX  = SKY__DFDR1
      SDIRY  = SKY__DFDR2
      SHGT   = SKY__DFHT
      SRAT   = SKY__DFRTO
      SJUST  = SKY__DFJST
      SSPACE = SKY__DFSPC
      SFONT  = SKY__DFFNT
      SPEN   = SKY__DFPEN

*  Write the texts together with their attributes one by one.
      DO I = 1, NTXT

*  Write out any keywords needed to set up the text attributes for the
*  next string.
         CALL SWRIF1( FID, DIRX( I ), DIRY( I ), HEIGHT( I ),
     :                RATIO( I ), JSTFCT( I ), SPACE( I ), FONT( I ),
     :                PEN( I ), SDIRX, SDIRY, SHGT, SRAT, SJUST,
     :                SSPACE, SFONT, SPEN, STATUS )

*  Get the formatted form the the sky coordinate of the position.
         CALL IRA_DTOC( LON( I ), LAT( I ), SCS, 0, LONST, LATST,
     :                  STATUS )

*  Write the current position and text string to the file.
         CALL MSG_SETC( 'A', LONST )
         CALL MSG_SETC( 'B', LATST )
         CALL MSG_SETC( 'T', TXT( I ) )
         CALL MSG_LOAD( ' ', '^A,^B, "^T"', BUF, BLEN, STATUS )
         CALL FIO_WRITE( FID, BUF( : MIN( 80, BLEN ) ), STATUS )

      END DO

      END
