      SUBROUTINE sgs_OTEXT
*+
*  Name:
*     OTEXT

*  Language:
*     Starlink Fortran 77

*  Description:
*     Output completed text string.

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From Sgscom:
*     LTEXT      i       length of text buffer

*  Externals:
*     GTX

*  Read From Common:
*     NTEXT      i       length of current string
*     XTEXT      r       position of current string (x)
*     YTEXT      r          "     "     "       "   (y)
*     CTEXT      c       current text string

*  Written To Common:
*     NTEXT      i       length of current string

*-

      IMPLICIT NONE

      INCLUDE 'sgscom'

      INTEGER LS



*  Make sure there's a string to output
      IF (NTEXT.GT.0) THEN

*     String length
         LS=MIN(LTEXT,NTEXT)

*     Output the string
         CALL GTX(XTEXT,YTEXT,CTEXT(:LS))

*     Reset
         NTEXT=-1
      END IF

      END
