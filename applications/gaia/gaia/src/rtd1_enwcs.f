      SUBROUTINE RTD1_ENWCS( IWCS, IPHEAD, NHEAD, AVAIL, STATUS )
*+
*  Name:
*     RTD1_ENWCS

*  Purpose:
*     Encodes an NDF WCS component as FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_ENWCS( IWCS, IPHEAD, NHEAD, AVAIL, STATUS )

*  Description:
*     This routine encodes an NDF WCS component as FITS headers. A
*     native encoding is used so that a lossless recovery can be made
*     and so the system has natural precendence over other systems.

*  Arguments:
*     IWCS = INTEGER (Given)
*        AST identifier to the FrameSet that is to be encoded.
*     IPHEAD = INTEGER (Given and Returned)
*        Pointer to a character array that contains some FITS
*        headers and that will contain any new headers added here.
*     NHEAD = INTEGER (Given and Returned)
*        On exit this is the number of cards used in HEADER. On entry
*        the current number already used.
*     AVAIL = INTEGER (Given and Returned)
*        The number of cards allocated by IPHEAD.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*     PDRAPER: Peter W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils

*  History:
*     15-NOV-1997 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'AST_PAR'         ! AST parameters

*  Arguments Given:
      INTEGER IWCS

*  Arguments Given and Returned:
      INTEGER IPHEAD
      INTEGER NHEAD
      INTEGER AVAIL

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL AST_FITSCHAN     ! Create a FITS channel
      EXTERNAL AST_ISAFRAME     ! Test that identifier is a Frame or FrameSet
      EXTERNAL AST_WRITE        ! Write AST object to channel

*  Local Variables:
      CHARACTER * ( 80 ) CARD   ! The FITS card
      INTEGER CHAN              ! FITS channel
      INTEGER RESULT            ! Number of objects written by channel

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make sure we have a valid identifier.
      IF ( AST_ISAFRAME( IWCS, STATUS ) ) THEN

*  Create a FITS channel to use when writing the object out.
         CHAN = AST_FITSCHAN( AST_NULL, AST_NULL, 'Encoding=Native',
     :                        STATUS )

*  And write out the FrameSet to the channel.
         RESULT = AST_WRITE( CHAN, IWCS, STATUS )

*  Now read the channel until we have all the cards.
         CALL AST_CLEAR( CHAN, 'CARD', STATUS )
 1       CONTINUE
         IF ( AST_FINDFITS( CHAN, '%f', CARD, .TRUE., STATUS ) ) THEN
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL RTD1_WRCRD( CARD, IPHEAD, NHEAD, AVAIL, STATUS)
               GO TO 1
            END IF
         END IF
         CALL AST_ANNUL( CHAN, STATUS )
      END IF
      END

