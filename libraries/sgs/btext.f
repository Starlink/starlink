      SUBROUTINE sgs_BTEXT (X,Y)
*+
*   - - - - - -
*    B T E X T
*   - - - - - -
*
*   Begin a new text string.
*
*   Given:
*      X       r     position of string (X)
*      Y       r     position of string (Y)
*
*   Read from COMMON:
*      NTEXT   i     length of current string
*
*   Written to COMMON:
*      XTEXT   r     position of current string (X)
*      YTEXT   r     position of current string (Y)
*      NTEXT   i     length of current string
*
*   External:
*      sgs_OTEXT
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
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
