      FUNCTION KFPY (RY)
C
C Given a y coordinate RY in the fractional system, KFPY(RY) is a y
C coordinate in the plotter system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      KFPY=1+IFIX(RY*(2.**MX-1.))
      RETURN
      END
