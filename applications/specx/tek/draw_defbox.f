C-----------------------------------------------------------------------------

      SUBROUTINE DRAW_DEFBOX (P, Q, LTYPE)

C   Routine to draw a box on an open plot with vertices described by
C   arrays P (X) and Q (Y)

      IMPLICIT  NONE

C   Formal parameters
      REAL*4    P(2),Q(2)
      INTEGER*4 LTYPE

C   Local variables
      REAL*4    X(5),Y(5)

      CALL SXGLTYPE (LTYPE)

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

      CALL SXGCONNECT (X,Y,5)
      CALL SXGLTYPE   (0)

      RETURN
      END

*-----------------------------------------------------------------------
