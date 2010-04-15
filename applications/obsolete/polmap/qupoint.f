      SUBROUTINE QUPOINT(NPARAMS,PARAMS,OUT_LU)
C+
C
C Subroutine:
C
C   Q U P O I N T
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
C Plots a userdefined point in the QU plane
C
C
C
C-

      IMPLICIT NONE
      INTEGER NPARAMS,OUT_LU,FILL
      REAL PI
      REAL PARAMS(*)
      LOGICAL OK
C
      PI=3.141592654
C
      IF (NPARAMS.GT.6) THEN
         CALL WR_ERROR('Superfluous parameters ignored',OUT_LU)
      ENDIF
C
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Q(%)',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 999
       NPARAMS=1
      ENDIF
      IF (NPARAMS.EQ.1) THEN
       CALL GET_PARAM('Q(%) err',PARAMS(2),OK,OUT_LU)
       IF (.NOT.OK) GOTO 999
       NPARAMS=2
      ENDIF
      IF (NPARAMS.EQ.2) THEN
       CALL GET_PARAM('U(%)',PARAMS(3),OK,OUT_LU)
       IF (.NOT.OK) GOTO 999
       NPARAMS=3
      ENDIF
      IF (NPARAMS.EQ.3) THEN
       CALL GET_PARAM('U(%) err',PARAMS(4),OK,OUT_LU)
       IF (.NOT.OK) GOTO 999
       NPARAMS=4
      ENDIF
      IF (NPARAMS.EQ.4) THEN
       CALL GET_PARAM('Number of vertices',PARAMS(5),OK,OUT_LU)
       IF (.NOT.OK) GOTO 999
       NPARAMS=5
      ENDIF
      IF (NPARAMS.EQ.5) THEN
       CALL GET_PARAM('Size',PARAMS(6),OK,OUT_LU)
       IF (.NOT.OK) GOTO 999
       NPARAMS=6
      ENDIF
      IF (NPARAMS.EQ.6) THEN
       CALL GET_PARAM('Solid or hollow (1 or 2)',PARAMS(7),OK,OUT_LU)
       IF (.NOT.OK) GOTO 999
       NPARAMS=7
      ENDIF
C
      FILL=INT(PARAMS(7))
      IF ( (FILL.NE.1).AND.(FILL.NE.2) ) THEN
        CALL WR_ERROR('Wrong fill sytle',OUT_LU)
        GOTO 999
      ENDIF
      CALL PGSFS(FILL)
      CALL TPOLY(PARAMS(1),PARAMS(3),INT(PARAMS(5)),PARAMS(6))
      IF (PARAMS(2).NE.0.) THEN
       CALL PGERRX(1,PARAMS(1)-PARAMS(2),PARAMS(1)+PARAMS(2),
     &             PARAMS(3),1.)
      ENDIF
      IF (PARAMS(4).NE.0) THEN
       CALL PGERRY(1,PARAMS(1),PARAMS(3)-PARAMS(4),
     &             PARAMS(3)+PARAMS(4),1.)
      ENDIF
C
 999  CONTINUE
      END

