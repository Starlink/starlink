      SUBROUTINE sgs_TXR (X,Y, R, NFI, NDP)
*+
*   - - - -
*    T X R
*   - - - -
*
*   Begin a new text string with a formatted real number.
*
*   (In practice, this means simply "plot a formatted real number")
*
*   Given:
*      X         r      position of text string (x)
*      Y         r          "    "   "      "   (y)
*      R         r      real number to be formatted
*      NFI       i      format indicator:-
*                       either  number of leading spaces (NFI.GE.0)
*                           or  minus the field width (NFI.LT.0)
*      NDP       i      number of decimal places
*                          If NDP.LT.0, only the integer part appears
*                          If NDP.EQ.0, the decimal point appears
*                          If NDP.GT.0, NDP digits appear after the point
*
*   Remarks:
*   The field width is limited in size - see sgs_ATXR coding.
*
*   Externals:
*      sgs_BTEXT, sgs_ATXR
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL X,Y,R
      INTEGER NFI,NDP



*  Begin the new text string
      CALL sgs_BTEXT(X,Y)

*  Format the number onto it
      CALL sgs_ATXR(R,NFI,NDP)

      END
