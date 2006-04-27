      SUBROUTINE sgs_TXI (X,Y, I, NFI)
*+
*   - - - -
*    T X I
*   - - - -
*
*   Begin a new text string with a formatted integer.
*
*   (In practice, this means simply "plot a formatted integer")
*
*   Given:
*      X        r      position of text string (x)
*      Y        r         "     "    "     "   (y)
*      I        i      integer to be formatted
*      NFI      i      format indicator:-
*                       either  number of leading spaces (NFI.GE.0)
*                           or  minus the field width (NFI.LT.0)
*
*   Remarks:
*   The field width is limited to 20 characters.
*
*   Externals:
*      sgs_BTEXT, sgs_ATXI
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL X,Y
      INTEGER I,NFI



*  Begin the new text string
      CALL sgs_BTEXT(X,Y)

*  Format the number onto it
      CALL sgs_ATXI(I,NFI)

      END
