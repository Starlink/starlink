      FUNCTION CPFY (IY)
C
C Given a y coordinate IY in the plotter system, CPFY(IY) is a y
C coordinate in the fractional system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      CPFY=FLOAT(IY-1)/(2.**MY-1.)
      RETURN
      END
