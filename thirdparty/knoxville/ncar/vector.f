      SUBROUTINE VECTOR (PX,PY)
C
C Draw a vector (line segment) from the current pen position to the new
C pen position (PX,PY), in the user coordinate system, and then make
C (PX,PY) the current pen position.
C
      CALL PLOTIT (KUMX(PX),KUMY(PY),1)
      RETURN
      END
