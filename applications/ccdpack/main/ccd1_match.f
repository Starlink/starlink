      LOGICAL FUNCTION CCD1_MATCH( STRING, LIST, NLIST, STATUS )
*+
*  Name:
*     CCD1_MATCH

*  Purpose:
*     Checks a given string against a list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CCD1_MATCH( STRING, LIST, NLIST, STATUS )

*  Description:
*     The routine checks the character string STRING against those
*     values in LIST looking for a match. If a match is found then
*     CCD1_MATCH is returned as true.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to compare against
*     LIST( NLIST ) = CHARACTER * ( * ) (Given)
*        List of character strings to match STRING against.
*     NLIST = INTEGER (Given)
*        Number of entries in LIST.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Returned Value:
*     CCD1_MATCH = LOGICAL
*        True if a match is found.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-FEB-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STRING
      INTEGER NLIST
      CHARACTER * ( * ) LIST( NLIST )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR
      EXTERNAL CHR_SIMLR         ! Checks if two strings are the same
                                 ! except for case.

*  Local Variables:
      INTEGER I
*.

*  Loop over all input list looking for case-insensitive match.
      DO 1 I = 1, NLIST
         CCD1_MATCH = CHR_SIMLR( STRING, LIST( I ) )
         IF ( CCD1_MATCH ) GO TO 99
 1    CONTINUE
 99   END

* $Id$
