      SUBROUTINE SCULIB_TIDY_LINE (COMCHAR, LINE, LENGTH)
*+
*  Name:
*     SCULIB_TIDY_LINE

*  Purpose:
*     remove tabs and comments from a line

*  Description:
*     This routine tidies up a character string and returns its length
*     ignoring trailing blanks. If the returned length is 0 the returned
*     string will be ' ', otherwise it will be equal to the input string
*     truncated at the used length.
*
*     The tidying involves:-
*      - calling CHR_CLEAN to remove non-printable characters.
*      - replacing all HT (tab) characters by spaces.
*      - truncating the string at the character before a COMCHAR character, if
*        present (the characters after COMCHAR are assumed to be comments).
*

*  Invocation:
*     CALL SCULIB_TIDY_LINE (COMCHAR, LINE, LENGTH)

*  Arguments:
*     COMCHAR    = CHARACTER*1 (Given)
*           the character at the beginning of a comment
*     LINE       = CHARACTER*(*) (Given and returned)
*           the line to be tidied
*     LENGTH     = INTEGER (Returned)
*           the length of the string, ignoring trailing blanks


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     15-SEP-1994: Original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*1 COMCHAR

*  Arguments Given & Returned:
      CHARACTER*(*) LINE

*  Arguments Returned:
      INTEGER LENGTH

*  Status:

*  External references:
      INTEGER CHR_LEN                ! CHR string-length function

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER COMMENT
      INTEGER TAB

*  Internal References:

*  Local data:

*.

      IF (CHR_LEN(LINE) .EQ. 0) THEN
         LENGTH = 0
      ELSE

*  clean non-printables

         CALL CHR_CLEAN (LINE)

*  replace tabs by spaces, not sure if tabs count as non-printables

         TAB = INDEX (LINE, CHAR(09))
         DO WHILE (TAB .NE. 0)
            LINE(TAB:TAB) = ' '
            TAB = INDEX(LINE, CHAR(09))
         END DO

*  look for comments, truncate LINE if found, find used length of line

         COMMENT = INDEX (LINE, COMCHAR)
         IF (COMMENT .GT. 0) THEN
            LENGTH = COMMENT - 1
            IF (LENGTH .GT. 0) THEN
               LENGTH = CHR_LEN (LINE(:LENGTH))
            END IF
         ELSE
            LENGTH = CHR_LEN (LINE)
         END IF

      END IF

*  set LINE to its clean form

      IF (LENGTH .EQ. 0) THEN
         LINE = ' '
      ELSE
         LINE = LINE (:LENGTH)
      END IF

      END
