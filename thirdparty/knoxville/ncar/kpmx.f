      FUNCTION KPMX (IX)
C
C Given an x coordinate IX in the plotter system, KPMX(IX) is an x
C coordinate in the metacode system.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      KPMX=IFIX(32767.*FLOAT(IX-1)/(2.**MX-1.))
      RETURN
      END
