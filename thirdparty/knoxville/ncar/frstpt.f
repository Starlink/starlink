      SUBROUTINE FRSTPT (PX,PY)
C
C Given the user coordinates PX and PY of a point, FRSTPT generates a
C pen-up move to that point.
C
      CALL PLOTIT (KUMX(PX),KUMY(PY),0)
C
C Done.
C
      RETURN
C
      END
