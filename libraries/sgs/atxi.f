      SUBROUTINE sgs_ATXI (I, NFI)
*+
*   - - - - -
*    A T X I
*   - - - - -
*
*   Format an integer number and append to the text buffer.
*
*   The field width is limited to 20 characters by the size of the
*   local string variable STRING.
*
*   Given:
*      I      i      Integer to be formatted
*      NFI    i      Format indicator:-
*                       either  number of leading spaces (NFI.GE.0)
*                           or  minus the field width (NFI.LT.0)
*  Externals:
*     sgs_ATXB, sgs_ATEXT
*
*  P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
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
