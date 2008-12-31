      SUBROUTINE GET_PARAMD(NAME,VL,OK,OUT_LU)
C+
C
C Subroutine: 
C
C    G E T _ P A R A M D
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NAME (<), VL (>), OK (>), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C Obtains the value of a double precision parameter from the user
C
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      CHARACTER*(*) NAME
      INTEGER IN_LU
      PARAMETER(IN_LU=5)
      DOUBLE PRECISION VL
      LOGICAL OK
      OK = .TRUE.
C
10    FORMAT(1X,A,'? ',$)
      WRITE(OUT_LU,10) NAME
      READ(IN_LU,*,ERR = 666) VL
      GOTO 999
666   CALL WR_ERROR('Cannot read parameter',OUT_LU)
      OK = .FALSE.
999   CONTINUE
      END
