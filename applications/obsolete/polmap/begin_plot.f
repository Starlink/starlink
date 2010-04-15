      SUBROUTINE BEGIN_PLOT(NPARAMS,PARAMS,CPARAM,OUT_LU)
C+
C
C Subroutine:
C
C    B E G I N _ P L O T
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), CPARAM (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C Interface with PGBEGIN subroutine
C
C
C
C-
      IMPLICIT NONE
      INTEGER NPARAMS
      REAL PARAMS(*),DUMMY
      CHARACTER*(*) CPARAM
      CHARACTER*80 DEVICE
      INTEGER I
      INTEGER OUT_LU
      INTEGER NX,NY
      LOGICAL OK
      CALL SSTRIP(CPARAM)
      IF (CPARAM.EQ.' ') THEN
         CALL GET_CPARAM('GKS device',CPARAM,OK,OUT_LU)
         CALL SSTRIP(CPARAM)
         IF (.NOT.OK) GOTO 666
      ENDIF
      I = INDEX(CPARAM,' ')
      DEVICE=CPARAM(1:(I-1))
      CPARAM=CPARAM((I+1):)
      CALL SSTRIP(CPARAM)
      IF (CPARAM.EQ.' ') THEN
         NX=1
         NY=1
      ELSE
         READ(CPARAM,*,ERR=667) NX
         I = INDEX(CPARAM,' ')
         CPARAM=CPARAM((I+1):)
         CALL SSTRIP(CPARAM)
         IF (CPARAM.EQ.' ') THEN
            CALL GET_PARAM('Y divisions',DUMMY,OK,OUT_LU)
            IF (.NOT.OK) GOTO 666
            NY=INT(DUMMY)
         ELSE
            READ(CPARAM,*,ERR=667) NY
         ENDIF
      ENDIF
      CALL PGBEGIN(0,DEVICE,NX,NY)
      CALL PGASK(.FALSE.)
 666  CONTINUE
      GOTO 999
 667  CONTINUE
      CALL WR_ERROR('Cannot read parameters',OUT_LU)
 999  CONTINUE
      END
