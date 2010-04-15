      SUBROUTINE GET_ERRORS(I,Q,QV,U,UV,P,PE,THETA,TE)
C+
C
C Subroutine:
C
C         G E T _ E R R O R S
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C I (<), Q (<), QV (<), U (<), UV (<), P (>), PE (>), THETA (>), TE (>)
C
C History:
C
C   May 1994 Created
C
C
C Given I,Q,QV,U and UV this routine calculates the errors on P% and
C the PA.
C
C
C
C
C
C
C
C-
      IMPLICIT NONE
      REAL I,Q,U,P,PE,THETA,TE,X
      REAL QQ,UU,QQE,UUE,QV,UV
      IF ( (Q.NE.0.).AND.(U.NE.0) ) THEN
      THETA  =  ATAN2(U,Q) * 90.0/3.1415926
       ELSE
       THETA = 0.0
      ENDIF
      IF (THETA .LT. 0.0) THETA  =  THETA + 180.0
      QQ  =  100.0*Q/I
      UU  =  100.0*U/I
      QQE  =  100.0*QV/I
      UUE  =  100.0*UV/I
      P  =  SQRT(QQ*QQ+UU*UU)
      IF (THETA .LT. 0.0) THETA  =  THETA + 180.0
      IF (P .GT. 0.0) THEN
             X  =  QQE*QQE*QQ*QQ + UUE*UUE*UU*UU
             PE  =  SQRT(X)/P
             X  =  QQ*QQ*UUE*UUE + UU*UU*QQE*QQE
             X  =  0.5*SQRT(X)
             X  =  X/(P*P)
             TE  =  ABS(X*57.2958)
         ELSE
             PE  =  0.0
             TE  =  0.0
       ENDIF
C
C PE is in % and PA error is in degrees.
C
      END





