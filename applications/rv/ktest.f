      INTEGER FUNCTION KTEST (INREC, IP)
*+
*
*  KTEST:  function in RV utility which examines a string
*          to check for unexpected trailing characters
*
*  Given:
*     INREC   char   string
*     IP      int    string pointer
*
*  Result:
*
*     -1  =  the indicated character lies within the string and
*            is not a space
*
*      0  =  either the indicated character does not lie within
*            string or it and any that follow are all spaces
*
*     +1  =  the indicated character is a space, but in those
*            that follow it there is at least one non-space
*
*  P T Wallace   Starlink   9 June 1992
*-

      IMPLICIT NONE

      CHARACTER INREC*(*)
      INTEGER IP


      IF (IP.LT.1.OR.IP.GT.(LEN(INREC))) THEN
         KTEST=0
      ELSE IF (INREC(IP:IP).NE.' ') THEN
         KTEST=-1
      ELSE IF (INREC(IP:).EQ.' ') THEN
         KTEST=0
      ELSE
         KTEST=1
      END IF

      END
