C-----------------------------------------------------------------------

      SUBROUTINE MAP_LOCATE (MAP, NMAP, X, Y, M, N, Z)

      INCLUDE  'FLAGCOMM'
      INCLUDE  'PLOT2D'

      REAL*4    MAP(NAXX,NAXY,1)

D     CALL SXGTIDLE

      IX = LINK(1)
      IY = LINK(2)
      DX = (CEND(IX)-CBEG(IX))/FLOAT(NAXX-1)
      DY = (CBEG(IY)-CEND(IY))/FLOAT(NAXY-1)

D     Type *,'IX,IY',IX,IY
D     Type *,'DX,XY',DX,DY

      M  = NINT((X-CBEG(IX))/DX+1.0)
      N  = NINT((Y-CEND(IY))/DY+1.0)

D     Type *,'Raw indices (M,N,NMAP):    ',M,N,NMAP

      M  = MAX (MIN (M,NAXX), 1)
      N  = MAX (MIN (N,NAXY), 1)

D     Type *,'Limited indices (M,N,NMAP):',M,N,NMAP

      Z = MAP(M, N, NMAP)

D     Type *,'Z-value: ',Z

D     CALL SXGTTGRAPH

      RETURN
      END

C-----------------------------------------------------------------------
