      SUBROUTINE RTD1_DEWCS( HEAD, NHEAD, IWCS, STATUS )
*+
*  Name:
*     RTD1_DEWCS

*  Purpose:
*     Decodes an WCS system from FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_DEWCS( HEAD, NHEAD, IWCS, STATUS )

*  Description:
*     This routine processes a FITS header character array, looking for
*     an encoded WCS which it returns as an AST FrameSet (suitable for
*     writing to an NDF component). The headers are scanned using the
*     normal sequence of encoding priorities (so Native, the most
*     accurate, are scanned for first, followed by FITS-WCS).

*  Arguments:
*     HEAD ( NHEAD ) = CHARACTER * ( * ) (Given)
*        Character array that contains some FITS headers that may
*        contain a WCS system.
*     NHEAD = INTEGER (Given)
*        The number of cards in the input header.
*     IWCS = INTEGER (Given)
*        AST identifier to the decoded FrameSet. This is set to
*        AST__NULL if none is found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils

*  History:
*     15-DEC-1997 (PDRAPER):
*        Original version.
*     24-JUN-1998 (PDRAPER):
*        Now tests WCS to see if it is a valid frameset.
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
      INTEGER NHEAD
      CHARACTER * ( * ) HEAD( NHEAD )

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL AST_FITSCHAN     ! Create a FITS channel
      EXTERNAL AST_READ         ! Read an AST object from a channel

*  Local Variables:
      INTEGER CHAN              ! FITS channel
      INTEGER I                 ! Loop variable
      INTEGER BASE              ! Pointer to BASE frame

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a FITS channel to use when reading the headers.
      CHAN = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Now write all the cards to the FITS channel. Clear any errors as
*  these are probably due to trivial formatting mistakes.
         DO 1 I = 1, NHEAD
            CALL AST_PUTFITS( CHAN, HEAD( I ), .FALSE., STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF
 1       CONTINUE

*  And read the channel for a WCS object.
         CALL AST_CLEAR( CHAN, 'CARD', STATUS )
         IWCS = AST_READ( CHAN, STATUS )

*  Validate the WCS, just in case there isn't one really.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. 
     :        ( IWCS .NE. AST__NULL ) .AND. 
     :        ( AST_GETC( IWCS, 'Class', STATUS ) .EQ. 'FrameSet' ) ) 
     :   THEN

*  NDF insists that the FrameSet have a base frame with domain Grid, so
*  just ensure this (we could check this, but then we'd probably set it
*  to grid anyway, so just do it).
            BASE = AST_GETFRAME( IWCS, AST__BASE, STATUS )
            CALL AST_SET( BASE, 'Domain=Grid', STATUS )
            CALL AST_ANNUL( BASE, STATUS )
         ELSE 

*  This object had better be AST__NULL.
            IF ( IWCS .NE. AST__NULL ) THEN 
               CALL AST_ANNUL( IWCS, STATUS )
               IWCS = AST__NULL
            END IF
         END IF

*  Release the channel as we are now finished with it.
         CALL AST_ANNUL( CHAN, STATUS )
      END IF
      END

