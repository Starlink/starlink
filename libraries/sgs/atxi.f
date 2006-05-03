      SUBROUTINE sgs_ATXI (I, NFI)
*+
*  Name:
*     ATXI

*  Purpose:
*     Format an integer number and append to the text buffer.

*  Language:
*     Starlink Fortran 77

*  Description:
*     The field width is limited to 20 characters by the size of the
*     local string variable STRING.

*  Arguments:
*     I = INTEGER (Given)
*         Integer to be formatted
*     NFI = INTEGER (Given)
*         Format indicator:-
*           either  number of leading spaces (NFI.GE.0)
*           or  minus the field width (NFI.LT.0)

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
*     sgs_ATXB, sgs_ATEXT

*-

      IMPLICIT NONE

      INTEGER I,NFI

      CHARACTER*20 STRING


*  Format the number
      WRITE (STRING,'(I20)') I

*  Append it in the appropriate manner
      IF (NFI.GE.0) THEN
         CALL sgs_ATXB(STRING,NFI)
      ELSE
         CALL sgs_ATEXT(STRING(MAX(21+NFI,1):))
      END IF

      END
