      FUNCTION CMFX (IX)
C
C Given an x coordinate IX in the metacode system, CMFX(IX) is an x
C coordinate in the fractional system.
C
      CMFX=FLOAT(IX)/32767.
      RETURN
      END
