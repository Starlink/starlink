      SUBROUTINE sgs_BTEXT (X,Y)
*+
*  Name:
*     BTEXT

*  Purpose:
*     Begin a new text string.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     X = REAL (Given)
*         Position of string (X)
*     Y = REAL (Given)
*         Position of string (Y)

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

*  External:
*     sgs_OTEXT

*  Read From Common:
*     NTEXT   i     length of current string

*  Written To Common:
*     XTEXT   r     position of current string (X)
*     YTEXT   r     position of current string (Y)
*     NTEXT   i     length of current string

*-

      IMPLICIT NONE

      REAL X,Y

      INCLUDE 'sgscom'




*  Flush any existing text string
      IF (NTEXT.GT.0) CALL sgs_OTEXT

*  Copy starting X,Y
      XTEXT=X
      YTEXT=Y

*  Initialise character count
      NTEXT=0

      END
