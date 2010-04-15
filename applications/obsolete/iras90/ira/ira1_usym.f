      SUBROUTINE IRA1_USYM( WKID, UNIT, REFX, REFY, TEXT, FEND, HT,
     :                      SUPSIZ, SUPOFF, UX, UY, STATUS )
*+
*  Name:
*     IRA1_USYM

*  Purpose:
*     Plot a superscript unit smbol

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_USYM( WKID, UNIT, REFX, REFY, TEXT, FEND, HT, SUPSIZ,
*                     SUPOFF, UX, UY, STATUS )

*  Description:
*     Draws a small superscript units symbol, at the requested position
*     relative to the sypplied text string.

*  Arguments:
*     WKID = INTEGER (Given)
*        The GKS workstation identifier.
*     UNIT = CHARACTER * ( * ) (Given)
*        The character to be used for the units symbol.
*     REFX = REAL (Given)
*        The X coordinate of the reference position at which the text
*        string given by TEXT was displayed.
*     REFY = REAL (Given)
*        The Y coordinate of the reference position at which the text
*        string given by TEXT was displayed.
*     TEXT = CHARACTER * ( * ) (Given)
*        The text string with which the units symbol is associated.
*     FEND = INTEGER (Given)
*        The number of characters in text prior to the position at which
*        the units symbol is to be placed.
*     HT = REAL (Given)
*        Current full sized character height.
*     SUPSIZ = REAL (Given)
*        The height of a superscript symbol as a fraction of HT.
*     SUPOFF = REAL (Given)
*        Offset as a fraction of HT from the top of a full size
*        character to the bottom of the superscript sumbol.
*     UX = REAL (Given)
*        The X component of the current unit up vector.
*     UY = REAL (Given)
*        The Y component of the current unit up vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      INTEGER WKID
      CHARACTER UNIT*1
      REAL REFX
      REAL REFY
      CHARACTER TEXT*(*)
      INTEGER FEND
      REAL HT
      REAL SUPSIZ
      REAL SUPOFF
      REAL UX
      REAL UY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL    CPX                ! X coordinate of the end of the
                                 ! plotted text string.
      REAL    CPY                ! Y coordinate of the end of the
                                 ! plotted text string.
      REAL    DX                 ! Shift in X from a point on the
                                 ! centre line through the text, to the
                                 ! corresponding point on the centre
                                 ! line through the superscript
                                 ! symbols.
      REAL    DY                 ! Shift in Y from a point on the
                                 ! centre line through the text, to the
                                 ! corresponding point on the centre
                                 ! line through the superscript
                                 ! symbols.
      INTEGER ERRIND             ! GKS error status.
      REAL    SUPCOR             ! Correction to supplied value of SUPOFF
      REAL    TXEXPX( 4 )        ! X coordinates of corners of box which
                                 ! encloses the printed text.
      REAL    TXEXPY( 4 )        ! Y coordinates of corners of box which
                                 ! encloses the printed text.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* Find the coordinates of the end of the printed field (excluding
* trailing blanks).
      CALL GQTXX( WKID, REFX, REFY, TEXT( : FEND ), ERRIND, CPX, CPY,
     :            TXEXPX, TXEXPY )

      IF( ERRIND .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ERRIND', ERRIND )
         CALL ERR_REP( 'IRA1_USYM_ERR1',
     : 'IRA1_USYM: GKS error no. ^ERRIND occurred while calling GQTXX.',
     :                       STATUS )

      ELSE

*  Reduce the size of the characters.
         CALL SGS_SHTX( HT*SUPSIZ )

*  If the symbolds are single or double quotes, lower the unit symbol
*  slightly because these characters are displayed unussually high.
         IF( UNIT .EQ. '''' .OR. UNIT .EQ. '"' ) THEN
            SUPCOR = 0.2
         ELSE
            SUPCOR = 0.0
         END IF

*  Store the increments in X and Y which put the reference position at
*  the up-down centre of the superscript symbols.
         DX = UX*HT*( 0.5*( 1.0 + SUPSIZ ) - SUPOFF - SUPCOR )
         DY = UY*HT*( 0.5*( 1.0 + SUPSIZ ) - SUPOFF - SUPCOR )

*  Print the required symbol.
         CALL SGS_TX( CPX + DX, CPY + DY, UNIT )

*  Restore the original character size.
         CALL SGS_SHTX( HT )

      END IF

      END
