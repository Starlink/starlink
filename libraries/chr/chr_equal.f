      LOGICAL FUNCTION CHR_EQUAL( STR1, STR2 )
*+
*  Name:
*     CHR_EQUAL

*  Purpose:
*     Return whether two strings are equal.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_EQUAL( STR1, STR2 )

*  Description:
*     Determine whether the two given strings are the same, with case
*     distinction. Their lengths must be identical after removing 
*     trailing blanks.
 
*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The first string.
*     STR2 = CHARACTER * ( * ) (Given)
*        The second string.

*  Algorithm:
*     Use a Fortran relational expression.

*  Returned Value:
*     CHR_EQUAL = LOGICAL
*        Returned as .TRUE. if the two given strings are the same,
*        otherwise .FALSE.

*  Notes:
*     This routine is OBSOLETE.  It exists for historical reasons.
*     Its function is better performed by a Fortran relational expression.

*  Authors:
*     ASOC5: Dave Baines (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JUN-1984 (ASOC5):
*        Original version.
*     25-SEP-1988 (AJC):
*        Remove INCLUDE 'SAE_PAR'.
*        Improve documentation.
*     26-JAN-1990 (AJC):
*        Re-write as portable version.
*     30-MAR-1994 (ACC):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR1
      CHARACTER * ( * ) STR2

*.

*  Perform the string comparison.
      CHR_EQUAL = ( STR1 .EQ. STR2 )

      END
