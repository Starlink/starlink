      SUBROUTINE sgs_TX (X,Y, STRING)
*+
*  Name:
*     TX

*  Purpose:
*     Begin a new text string with a string.

*  Language:
*     Starlink Fortran 77

*  Description:
*     (In practice, this means simply "plot a string")

*  Arguments:
*     X = REAL (Given)
*         Position of string (x)
*     Y = REAL (Given)
*         "     "    "   (y)
*     STRING = CHAR (Given)
*         String which is to begin the new text string

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

*  Externals:
*     sgs_BTEXT, sgs_ATEXT

*-

      IMPLICIT NONE

      REAL X,Y
      CHARACTER*(*) STRING



*  Begin a new text string
      CALL sgs_BTEXT(X,Y)

*  Append the string to it
      CALL sgs_ATEXT(STRING)

      END
