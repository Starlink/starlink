      SUBROUTINE SRANGE(NPARAMS,PARAMS,RMIN,RMAX,AUTOLIM,OUT_LU)
      IMPLICIT NONE
      INTEGER OUT_LU
      INTEGER NPARAMS
      REAL PARAMS(*)
      REAL RMIN,RMAX
      LOGICAL AUTOLIM,OK
C+
C
C Subroutine:
C
C   S R A N G E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), RMIN (>), RMAX (>), AUTOLIM (><), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C
C Sets a minimum/maximum range for various plotting parameters.
C
C
C
C-
C
      IF (NPARAMS.GT.2) THEN
       CALL WR_ERROR('Additional parameters ignored',OUT_LU)
      ENDIF
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Minimum',PARAMS(1),OK,OUT_LU)
       NPARAMS = 1
       IF (.NOT.OK) GOTO 666
      ENDIF
      IF (NPARAMS.EQ.1) THEN
       CALL GET_PARAM('Maximum',PARAMS(2),OK,OUT_LU)
       NPARAMS = 2
       IF (.NOT.OK) GOTO 666
      ENDIF
C
C When user sets a range switch off the autolimit
C
      AUTOLIM = .FALSE.
      RMIN = PARAMS(1)
      RMAX = PARAMS(2)
666   CONTINUE
      END
