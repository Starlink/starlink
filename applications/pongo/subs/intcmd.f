      INTEGER FUNCTION INTCMD( CLIST, STRING )
*+
*  Name:
*     INTCMD

*  Purpose:
*     Compares a string against a list allowing for abbreviation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = INTCMD( CLIST, STRING )

*  Description:
*     This routine compares the characters in 'STRING' against the list
*     in 'CLIST' and returns the position in the list which agrees with
*     STRING up to as many characters as are contained in STRING.  If
*     no match can be found then INTCMD is zero on exit.  If more than
*     one match is found then the following action is taken: if the
*     value in CLIST is also ambigous then the string is assumed as
*     matching the shortest of the matching CLIST entries; otherwise if
*     the value in CLIST is not ambiguous or if the ambiguous entries
*     are identical then INTCMD is set to the negative of the first
*     entry.

*  Arguments:
*     CLIST( * ) = CHARACTER * ( * ) (Given)
*        The array of characters strings which are compared to STRING.
*     STRING = CHARACTER * ( * ) (Given)
*        The string to compare against the strings in CLIST.

*  Returned Value:
*     INTCMD = INTEGER
*        The index of the string in the CLIST array which is selected.
*        This will be 0 if no match is obtained or the negative of one
*        of the possible matches if the comparison is ambiguous.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-MAY-1985 (?OLAF?):
*        Original version.
*     6-JUN-1994 (PDRAPER):
*        Added prologue and perform code tidying.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) CLIST( * )
      CHARACTER * ( * ) STRING

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string
      EXTERNAL COMSTR
      LOGICAL COMSTR             ! Compares strings for abbreviated
                                 ! match

*  Local Variables:
      INTEGER I                  ! Index into CLIST
      INTEGER L1                 ! Used length of current match
      INTEGER L2                 ! Used length of possible match

*.

*  Set intcmd.
      INTCMD = 0

*  Scan through CLIST comparing the entries with STRING until the end of
*  the list is reached.
      I = 1
 1000 CONTINUE                   ! Start of DO WHILE loop
      IF ( CLIST( I ) .NE. ' ' ) THEN

*  Compare the entry against STRING.
         IF ( COMSTR( CLIST( I ), STRING ) ) THEN

*  They agree, test if a previous entry also agreed.
            IF ( INTCMD .NE. 0 ) THEN

*  Yes, are but are the entries in CLIST ambiguous?  First check if they
*  are identical.
               L1 = CHR_LEN( CLIST( ABS( INTCMD ) ) )
               L2 = CHR_LEN( CLIST( I ) )
               IF ( CLIST( ABS( INTCMD ) ) .EQ. CLIST( I ) ) THEN

*  They are identical, note the possible ambiguity.
                  INTCMD = -ABS( INTCMD )
               ELSE IF ( L1 .LT. L2 ) THEN

*  The previous match is the shortest, are the entries ambiguous?
                  IF( COMSTR( CLIST( I ), CLIST( ABS( INTCMD ) ) ) )
     :            THEN

*  Yes, stay with the previous match.
                     INTCMD = ABS( INTCMD )
                  ELSE

*  No, the string is ambiguous: exit.
                     INTCMD = -ABS( INTCMD )
                     GO TO 99
                  END IF
               ELSE

*  The previous match is the longer string of the two, are the entries
*  in CLIST ambiguous?
                  IF ( COMSTR( CLIST( ABS( INTCMD ) ), CLIST( I ) ) )
     :            THEN

*  Yes, use the match to the latest entry.
                     INTCMD = I
                  ELSE

*  No, the string is ambiguous: exit.
                     INTCMD = -ABS( INTCMD )
                     GO TO 99
                  END IF
               END IF
            ELSE

*  There are no previous matches, set INTCMD to the list entry number.
               INTCMD = I
            END IF
         END IF

*  Get the next entry.
         I = I + 1
         GO TO 1000
      END IF

*  Exit in error label.
 99   CONTINUE
      END
* $Id$
