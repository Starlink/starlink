      SUBROUTINE POINT (PX,PY)
C
C Draws a point at (PX,PY), defined in the user coordinate system.
C
      CALL PLOTIT (KUMX(PX),KUMY(PY),0)
      CALL PLOTIT (KUMX(PX),KUMY(PY),1)
      RETURN
      END
