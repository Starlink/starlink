      SUBROUTINE gns_1TRIM (IN,OUT,L)
*++
*   gns_1TRIM   Trims leading and trailing blanks from a string.
*
*   Description:
*      Leading and trailing blanks are removed from the input string and
*      the length of the resulting string returned. If the input string
*      is all blanks the length is set to zero.
*
*   Input arguments:
*      IN      c(*)     Input string
*
*   Output arguments:
*      OUT     c*(*)    Output string
*      L       i        Length of output string
*
*   Implicit inputs:
*      none
*
*   Implicit outputs:
*      none
*
*   External references:
*      none
*
*   D L Terrett   3-JUN-1988 
*++
      IMPLICIT NONE

      CHARACTER*(*) IN, OUT
      INTEGER L, I

*  Remove leading blanks
      DO 10 I = 1,LEN(IN)
         IF (IN(I:I).NE.' ') GO TO 20
   10 CONTINUE

*  If we get here then the string is all blanks.
      L = 0
      GO TO 40
      
   20 CONTINUE
      OUT = IN(I:)

*  Find position of last non blank character
      DO 30 L = LEN(OUT),1,-1
         IF (OUT(L:L).NE.' ') GO TO 40
   30 CONTINUE

   40 CONTINUE
      END       
