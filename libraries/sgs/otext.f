      SUBROUTINE sgs_OTEXT
*+
*   - - - - - -
*    O T E X T
*   - - - - - -
*
*
*   Output completed text string.
*
*   Read from COMMON:
*      NTEXT      i       length of current string
*      XTEXT      r       position of current string (x)
*      YTEXT      r          "     "     "       "   (y)
*      CTEXT      c       current text string
*
*   Written to COMMON:
*      NTEXT      i       length of current string
*
*   Constants from SGSCOM:
*      LTEXT      i       length of text buffer
*
*   Externals:
*      GTX
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
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
