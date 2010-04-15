      SUBROUTINE GK2DLN(N,X,Y)

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Outputs polyline to buffer
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*      05/03/85   SHS  Initialise LIIY,LLOY,LIIX and LLOX
*
*  ARGUMENTS
*  ---------
*     INP N   - number of points
*     INP X,Y - coordinates of points
*
      INTEGER N
      REAL X(N),Y(N)
      INTEGER I,IX, IY

*
*  COMMON BLOCK USAGE
*  ------------------
*
*
*
      IX=INT(X(1))
      IY=INT(Y(1))
      CALL GK2DDR(IX,IY,0)
      DO 10 I=2,N
         IX=INT(X(I))
         IY=INT(Y(I))
         CALL GK2DDR(IX,IY,1)
   10 CONTINUE
*
      RETURN
      END
