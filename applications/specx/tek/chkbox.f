C-----------------------------------------------------------------------------

      LOGICAL FUNCTION CHKBOX(P,Q)

C   Routine to check that numbers in arrays P and Q define a 'positive-going'
C   box

      REAL*4   P(2),Q(2)

      CHKBOX=.TRUE.
      IF(P(2).LE.P(1)) CHKBOX=.FALSE.
      IF(Q(2).LE.Q(1)) CHKBOX=.FALSE.

      RETURN
      END


