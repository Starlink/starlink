      FUNCTION KFMX (RX)
C
C Given an x coordinate RX in the fractional system, KFMX(RX) is an x
C coordinate in the metacode system.
C
      KFMX=IFIX(RX*32767.)
      RETURN
      END
