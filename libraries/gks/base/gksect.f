C# IL>=a, OL>=0
      SUBROUTINE GKSECT(AX,AY,BX,BY,ICLIP,XMIN,XMAX,YMIN,YMAX,
     :                     XINTER,YINTER)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the intersect of A-B with ICLIP.
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/83  NGB  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP AX,AY,BX,BY coords of points A & B
*     INP ICLIP   Edge to clip on
*     INP XMIN,XMAX,YMIN,YMAX  Clip Spec
*     INP INTERX,INTERY coords of intersection point
*
      INTEGER ICLIP
      REAL AX, AY, BX, BY, XMIN, XMAX, YMIN, YMAX, XINTER, YINTER
*
*  LOCALS
*  ------
*
      REAL DX, DY, X, Y
*
*---------------------------------------------------------------------



      DX = BX-AX
      DY = BY-AY
      GOTO (10,20,30,40), ICLIP
* top
   10 CONTINUE
      Y = YMAX-AY
      X = (Y*DX)/DY
      XINTER = AX+X
      YINTER = YMAX
      GOTO 50
* right
   20 CONTINUE
      X = XMAX-AX
      Y = (X*DY)/DX
      XINTER = XMAX
      YINTER = AY+Y
      GOTO 50
* bottom
   30 CONTINUE
      Y = YMIN-AY
      X = (Y*DX)/DY
      XINTER = AX+X
      YINTER = YMIN
      GOTO 50
* left
   40 CONTINUE
      X = XMIN-AX
      Y = (X*DY)/DX
      XINTER = XMIN
      YINTER = AY+Y

   50 CONTINUE
      END
