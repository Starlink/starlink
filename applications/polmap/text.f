      SUBROUTINE TEXT(NPARAMS,PARAMS,CPARAM,FSTR,OUT_LU)
C+
C
C Subroutine:
C
C   T E X T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), CPARAM (<), FSTR (><), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C Writes text on a plot using pgplot routine pgptext
C
C
C
C-

      IMPLICIT NONE
      INTEGER OUT_LU
      INTEGER NPARAMS
      REAL PARAMS(*)
      REAL X,Y,ANG,HT
      LOGICAL OK
      CHARACTER*1 CH
      CHARACTER*80 CTEXT
      CHARACTER*80 CPARAM
      LOGICAL FSTR

      CALL SSTRIP(CPARAM)
      IF (NPARAMS.EQ.0) THEN
       WRITE(OUT_LU,*) 'Use cursor to find point and then hit a key'
       WRITE(OUT_LU,*) 'Or hit q to quit'
       CALL PGCURSE(PARAMS(1),PARAMS(2),CH)
       NPARAMS=2
       IF ((CH(1:1).EQ.'Q').OR.(CH(1:1).EQ.'q')) GOTO 666
       ELSE
      ENDIF
      IF (NPARAMS.EQ.1) THEN
       CALL GET_PARAM('Y-position',PARAMS(2),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS=2
      ENDIF
      IF (NPARAMS.EQ.2) THEN
       CALL GET_PARAM('Angle',PARAMS(3),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS=3
      ENDIF
      IF (NPARAMS.EQ.3) THEN
       CALL GET_PARAM('Text height',PARAMS(4),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS=4
      ENDIF
      X=PARAMS(1)
      Y=PARAMS(2)
      ANG=PARAMS(3)
      HT=PARAMS(4)
      IF (.NOT.FSTR) THEN
       CALL GET_CPARAM('Text',CTEXT,OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
      ELSE
       CTEXT=CPARAM
      ENDIF
      CALL PGSCH(HT)
      CALL PGPTEXT(X,Y,ANG,0.,CTEXT)
      CALL PGSCH(1.)
 666  CONTINUE
      END
