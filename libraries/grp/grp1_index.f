      INTEGER FUNCTION GRP1_INDEX( STRING, CC, ESC, OK )
*+
*  Name:
*     GRP1_INDEX

*  Purpose:
*     Find a non-escaped sub-string within a supplied string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     INDEX = GRP1_INDEX( STRING, CC, ESC, OK )

*  Description:
*     This subroutine should be used in place of the Fortran intrinsic
*     function INDEX to find a GRP control character within a string.
*     It ignores any matching characters if they are preceeded by the
*     specified escape character (if OK is .TRUE.)

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to search.
*     CC = CHARACTER * ( * ) (Given)
*        The sub-string to search for (e.g. a GRP control character).
*     ESC = CHARACTER * 1 (Given)
*        The escape character.
*     OK = LOGICAL (Given)
*        If .FALSE. any occurences of ESC within STRING are ignored.
*        In this case, this function behaves just like the Fortran INDEX.

*  Returned Value:
*     GRP1_INDEX = INTEGER
*        The index within STRING at which the first unescaped occurence
*        of CC was found, or zero if no unescaped occurences were found.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-AUG-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STRING*(*)
      CHARACTER CC*(*)
      CHARACTER ESC*1
      LOGICAL OK

*  External References:
      LOGICAL GRP1_CHKCC         ! Check for an unescaped control character

*  Local Variables:
      INTEGER SLEN               ! Declared length of supplied string
      INTEGER START              ! Absolute index of start of remaining text
      LOGICAL MORE               ! Continue looking
*.

*  Find the first occurrence of CC.
      GRP1_INDEX = INDEX( STRING, CC )

*  If escape characters are to be ignored, just return this index.
*  Otherwise, ignore any occurences of CC which occur after an escape
*  character. If CC is the first character in the string (or was not
*  found) it cannot be escaped.
      IF( OK .AND. GRP1_INDEX .GT. 1 ) THEN

*  Save the declared length of the STRING.
         SLEN = LEN( STRING )

*  The current index is referenced to character 1.
         START = 1

*  Loop until we have the result. 
         MORE = .TRUE.
         DO WHILE( MORE )

*  If this is a genuine control character, leave the loop.
            IF( GRP1_CHKCC( STRING, GRP1_INDEX, CC, ESC, OK ) ) THEN
               MORE = .FALSE.

*  Otherwise, we find the next occurrence of the control character.
*  First set the index within the string at which to start the search.
            ELSE
               START = GRP1_INDEX + 1

*  If we have reached the end of the string, there is no match.
               IF( START .GT. SLEN ) THEN
                  GRP1_INDEX = 0
                  MORE = .FALSE.

*  Otherwise, look for another occurenceof the control character.
               ELSE
                  GRP1_INDEX = INDEX( STRING( START : ), CC )

*  If not dound, leave the loop.
                  IF( GRP1_INDEX .EQ. 0 ) THEN
                     MORE = .FALSE.

*  Otherwise, convert from a relative to an absolute offset.
                  ELSE
                     GRP1_INDEX = GRP1_INDEX + START - 1
                  END IF

               END IF

            END IF

         END DO

      END IF

      END
