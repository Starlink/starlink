      SUBROUTINE sgs_1UPCAS (IN, OUT)
*+
*   - - - - - -
*    U P C A S     (Internal routine)
*   - - - - - -
*
*   Convert character string to uppercase.
*
*   Given:
*      IN       c      string to be converted
*
*   Returned:
*      OUT      c      converted string
*
*   Externals:
*      chr_UCASE
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      CHARACTER*(*) IN,OUT



      OUT = IN
      CALL chr_UCASE(OUT)

      END
