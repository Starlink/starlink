      SUBROUTINE sgs_ATXB (STRING, NSPACE)
*+
*  Name:
*     ATXB

*  Purpose:
*     Append a field onto the text buffer leaving a specified number
*     of spaces before the non-blank region of the field.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     STRING = CHAR (Given)
*         String to be appended
*     NSPACE = INTEGER (Given)
*         Number of spaces to leave

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     sgs_ATEXT

*-

      IMPLICIT NONE

      INTEGER NSPACE
      CHARACTER*(*) STRING

      INTEGER N,IS,LS


*   String length
      LS=LEN(STRING)

*   Append the required number of spaces
      IF (NSPACE.GE.1) THEN
         DO 10 N=1,NSPACE
            CALL sgs_ATEXT(' ')
   10    CONTINUE
      END IF

*   Skip leading spaces in field
      DO 20 IS = 1,LS
         IF (STRING(IS:IS).NE.' ') GO TO 30
   20 CONTINUE
   30 CONTINUE

*  Append rest of field
      CALL sgs_ATEXT(STRING(IS:))

      END
