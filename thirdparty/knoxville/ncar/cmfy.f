      FUNCTION CMFY (IY)
C
C Given a y coordinate IY in the metacode system, CMFY(IY) is a y
C coordinate in the fractional system.
C
      CMFY=FLOAT(IY)/32767.
      RETURN
      END
