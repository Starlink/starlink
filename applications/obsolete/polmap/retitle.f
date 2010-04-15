      SUBROUTINE RETITLE(NPARAMS,CPARAM,PARAMS,STK_TITLE,
     &STK_NPTS,OUT_LU,IN_LU)
C+
C
C Subroutine:
C
C   R E T I T L E
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), CPARAM (<), PARAMS (<), STK_TITLE (><),
C STK_NPTS (<), OUT_LU (<), IN_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Retitles a stack polarization spectrum.
C
C
C-

      IMPLICIT NONE
      INCLUDE 'array_size.inc'
      INTEGER STK_NPTS,OUT_LU,IN_LU
      INTEGER NPARAMS,ENTRY
      REAL PARAMS(*)
      CHARACTER*80 STK_TITLE(MAXSPEC)
      CHARACTER*(*) CPARAM
      LOGICAL OK
      IF (NPARAMS.GT.1) THEN
        CALL WR_ERROR('Superfluous parameters ignored',OUT_LU)
      ENDIF
      IF (NPARAMS.EQ.0) THEN
        CALL GET_PARAM('Stack entry',PARAMS(1),OK,OUT_LU)
        IF (.NOT.OK) GOTO 666
      ENDIF
      ENTRY=INT(PARAMS(1))
      IF ((ENTRY.LT.1).OR.(ENTRY.GT.STK_NPTS)) THEN
        CALL WR_ERROR('Stack entry out of range',OUT_LU)
        GOTO 666
      ENDIF
 10   FORMAT(1X,'New title: ',$)
      WRITE(OUT_LU,10)
      READ(IN_LU,'(A)',ERR=667) STK_TITLE(ENTRY)
 666  GOTO 999
 667  CONTINUE
      CALL WR_ERROR('Error reading title',OUT_LU)
 999  CONTINUE
      END
