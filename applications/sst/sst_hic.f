      SUBROUTINE SST_HIC( STR, IHIC )
*+
*  Name:
*     SST_HIC

*  Purpose:
*     Find the first unquoted exclamation mark (hic) in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_HIC( STR, IHIC )

*  Description:
*     The routine returns the character position of the first
*     exclamation mark in a string which does not lie within quotes.  A
*     value of zero is returned if no such character is found.  The
*     quote character is the Fortran 77 (single) quote character.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The string to be searched.
*     IHIC = INTEGER (Returned)
*        The character position (starting at 1).

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-AUG-1989 (RFWS):
*        Original version.
*     28-SEP-1990 (RFWS):
*        Adapted for release as part of the SST system.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR

*  Arguments Returned:
      INTEGER IHIC

*  Local Variables:
      INTEGER I                  ! Loop counter for characters
      LOGICAL QUOTE              ! Whether current character is quoted

*.

*  Initialise.
      IHIC = 0
      QUOTE = .FALSE.

*  Loop to inspect characters until an unquoted '!' is found.
      DO 1 I = 1, LEN( STR )

*  Note whether the current character is quoted.
         IF ( STR( I : I ) .EQ. '''' ) THEN
            QUOTE = .NOT. QUOTE

*  If an unquoted '!' is found, then return its position.
         ELSE IF ( ( STR( I : I ) .EQ. '!' ) .AND.
     :             ( .NOT. QUOTE ) ) THEN
            IHIC = I
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

      END
* @(#)sst_hic.f   1.1   94/12/05 11:31:27   96/07/05 10:27:29
