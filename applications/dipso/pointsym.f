      SUBROUTINE POINTSYM(X,Y,I)
*** Subroutine to plot symbol at a point
      REAL XARRAY(1),YARRAY(1)
      XARRAY(1)=X
      YARRAY(1)=Y
      CALL POINTS(XARRAY,YARRAY,1,-I,0)
      CALL SGS_FLUSH
      CALL PLOTIT( 0, 0, 2 )
      RETURN
      END
