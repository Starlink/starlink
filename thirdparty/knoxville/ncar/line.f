      SUBROUTINE LINE (X1,Y1,X2,Y2)
C
C Draw a line connecting the point (X1,Y1) to the point (X2,Y2), in the
C user coordinate system.
C
      CALL PLOTIT (KUMX(X1),KUMY(Y1),0)
      CALL PLOTIT (KUMX(X2),KUMY(Y2),1)
      RETURN
      END
