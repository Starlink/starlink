      SUBROUTINE hlp_UPCASE (STRING)
*+
*  - - - - - - -
*   U P C A S E
*  - - - - - - -
*
*  Convert a string to uppercase.
*
*  !!!  ASCII-dependent: assumes fixed offset between  !!!
*  !!!  uppercase and lowercase via ICHAR function     !!!
*
*  Given and returned:
*     STRING     c*(*)     string to be converted
*
*  P.T.Wallace   Starlink   26 August 1991
*-

      IMPLICIT NONE

      CHARACTER*(*) STRING

      INTEGER NUC,I
      CHARACTER C



*  Collating sequence offset between lowercase and uppercase.
      NUC=ICHAR('A')-ICHAR('a')

*  Pick up successive characters from the string.
      DO I=1,LEN(STRING)

*     Next character.
         C=STRING(I:I)

*     Convert to uppercase
         IF (C.GE.'a'.AND.C.LE.'z') C=CHAR(ICHAR(C)+NUC)

*     Store the character.
         STRING(I:I)=C

*     Next character.
      END DO

      END
