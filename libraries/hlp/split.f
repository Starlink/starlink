      SUBROUTINE hlp_SPLIT (STRING, ISTART, IFROM, ITO)
*+
*  - - - - - -
*   S P L I T
*  - - - - - -
*
*  Split a space-separated substring from the contents of STRING.
*
*  Given:
*     STRING   c*(*)     string containing space-separated substrings
*     ISTART   i         where to start looking in STRING
*
*  Returned:
*     IFROM    i         location of the start of the substring
*     ITO      i         location of the end of the substring
*
*  If no substring is found - i.e. if STRING(ISTART:) is all spaces or
*  if ISTART is past the end of STRING - IFROM is set to -1 and ITO is
*  left alone.  If ISTART is before the beginning of STRING the search
*  nonetheless starts at the beginning of STRING.
*
*  P.T.Wallace   Starlink   24 February 1991
*-

      IMPLICIT NONE

      CHARACTER STRING*(*)
      INTEGER ISTART,IFROM,ITO

      INTEGER L,I,J



*  Length of the string.
      L=LEN(STRING)

*  Search for the start of the substring.
      DO I=MAX(ISTART,1),L
         IF (STRING(I:I).NE.' ') THEN

*        Found it.
            IFROM=I

*        Now find the end of the substring.
            J=I+1
            DO WHILE (J.LE.L.AND.STRING(J:J).NE.' ')
               J=J+1
            END DO

*        Found it.
            ITO=J-1

*        All done: break.
            GO TO 9000
         END IF
      END DO

*  There is no substring.
      IFROM=-1

*  Exit.
 9000 CONTINUE

       END
