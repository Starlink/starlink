      FUNCTION KFPX (RX)
C
C Given an x coordinate RX in the fractional system, KFPX(RX) is an x
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      KFPX=1+IFIX(RX*(2.**MX-1.))
      RETURN
      END
