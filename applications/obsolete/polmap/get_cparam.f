      SUBROUTINE GET_CPARAM(ST,CPARAM,OK,OUT_LU)
C+
C
C Subroutine: 
C
C      G E T _  C P A R A M
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C ST (<), CPARAM (>), OK (>), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C Prompts for and inputs a character string parameter.
C
C
C-

      IMPLICIT NONE
      INTEGER IN_LU
      PARAMETER(IN_LU=5)
      CHARACTER*(*) ST
      CHARACTER*(*) CPARAM
      LOGICAL OK
      INTEGER OUT_LU
10    FORMAT(1X,A,'? ',$)
      OK=.TRUE.
      WRITE(OUT_LU,10) ST
      READ(IN_LU,'(A)',ERR = 666) CPARAM
      GOTO 999
666   CALL WR_ERROR('Cannot read parameter',OUT_LU)
      OK = .FALSE.
999   CONTINUE
      END
