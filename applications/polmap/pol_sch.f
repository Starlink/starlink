      SUBROUTINE POL_SCH(NPARAMS,PARAMS,OUT_LU)
C+
C
C Subroutine:
C
C   P O L _ S C H
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C
C Interface for pgplot pgscf command.
C
C-

      IMPLICIT NONE
      INTEGER NPARAMS,OUT_LU
      REAL PARAMS(*)
      LOGICAL OK
C
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Character height',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS=1
      ENDIF
C
      CALL PGSCH(PARAMS(1))
C
 666  CONTINUE
      END
