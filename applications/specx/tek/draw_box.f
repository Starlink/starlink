C-----------------------------------------------------------------------------

      SUBROUTINE DRAW_BOX (FIRST, P, Q)

C   Routine to draw a box on an open plot with vertices described by
C   arrays P (X) and Q (Y)

      IMPLICIT  NONE

C   Formal parameters
      LOGICAL*4 FIRST
      REAL*4    P(2),Q(2)

C   Local variables
      REAL*4    X(5),Y(5)
      SAVE      X,Y

C   Do it - if not first time delete old box first
C   Use complement line type, so that things look nice...

      IF (.NOT.FIRST) THEN
        CALL SXGLTYPE   (7)
        CALL SXGCONNECT (X,Y,5)
      END IF

      X(1)=P(1)
      Y(1)=Q(1)

      X(2)=P(1)
      Y(2)=Q(2)

      X(3)=P(2)
      Y(3)=Q(2)

      X(4)=P(2)
      Y(4)=Q(1)

      X(5)=X(1)
      Y(5)=Y(1)

      CALL SXGLTYPE   (0)
      CALL SXGCONNECT (X,Y,5)

      RETURN
      END

*-----------------------------------------------------------------------
