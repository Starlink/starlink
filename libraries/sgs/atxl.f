      SUBROUTINE sgs_ATXL (STRING)
*+
*  Name:
*     ATXL

*  Purpose:
*     Append left justified string onto text buffer.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     STRING = CHAR (Given)
*         Character string

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

      CHARACTER*(*) STRING

      INTEGER I


*  Find length of left justified portion
      DO 10 I = LEN(STRING),1,-1
         IF (STRING(I:I).NE.' ') GO TO 20
   10 CONTINUE
      I = 0
   20 CONTINUE

*  Append
      IF (I.GE.1) CALL sgs_ATEXT(STRING(:I))

      END
