      SUBROUTINE DBOUND(X,NPLT,XMIN,XMAX)
      INTEGER NPLT
      REAL X(NPLT)
       XMIN=1.0E35
       XMAX=-1.0E35
       DO I=1,NPLT
        IF(X(I).LT.XMIN)XMIN=X(I)
        IF(X(I).GT.XMAX)XMAX=X(I)
       ENDDO
      RETURN
      END
