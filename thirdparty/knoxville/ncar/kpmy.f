      FUNCTION KPMY (IY)
C
C Given a y coordinate IY in the plotter system, KPMY(IY) is a y
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      KPMY=IFIX(32767.*FLOAT(IY-1)/(2.**MY-1.))
      RETURN
      END
