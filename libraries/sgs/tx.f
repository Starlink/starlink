      SUBROUTINE sgs_TX (X,Y, STRING)
*+
*   - - -
*    T X
*   - - -
*
*   Begin a new text string with a string.
*
*   (In practice, this means simply "plot a string")
*
*   Given:
*        X         r       position of string (x)
*        Y         r           "     "    "   (y)
*        STRING    c       string which is to begin the new text string
*
*   Externals:
*      sgs_BTEXT, sgs_ATEXT
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      REAL X,Y
      CHARACTER*(*) STRING



*  Begin a new text string
      CALL sgs_BTEXT(X,Y)

*  Append the string to it
      CALL sgs_ATEXT(STRING)

      END
