C# IL>=a, OL>=0
      SUBROUTINE GKXLIN(AX,AY,BX,BY,LINSUB,XMIN,XMAX,YMIN,YMAX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Solid Fill Line Output
*     -uses <LINSUB>, the device's Line Output routine, to draw a
*     horizontal line
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/83  NGB   Original version stabilized
*     29/03/83  NGB   Coordinate Truncation included
*     28/04/83  NGB   Reinsert disappeared instruction (II=2)
*     29/11/83  NGB   Redundant Arguments IX,IY,IA removed
*
*  ARGUMENTS
*  ---------
*     INP AX,AY  End A coords
*     INP BX,BY  End B coords ( BX>AX, BY=AY )
*     INP LINSUB W/S Polyline Output Routine
*     INP XMIN,XMAX,YMIN,YMAX  Clipping Rectangle
*
      REAL AX, AY, BX, BY, XMIN, XMAX, YMIN, YMAX
      EXTERNAL LINSUB
*
*  LOCALS
*  ------
*
      REAL RXA(2),RYA(2)
*
*  ALGORITHM
*  ---------
*     (Clipping is included -trivial for horizontal line)
*
*  COMMENTS
*  --------
*     For consistency, the policy for all Pixel operations is to
*     truncate coordinates to the next lower pixel.
*
*     -provides a hook for optimisation as only horizontal lines
*     are ever drawn.
*
*
*---------------------------------------------------------------------

* Clip Y first
      IF ( (AY.GE.YMIN) .AND. (AY.LT.YMAX) ) THEN
* now clip X
         IF (AX.LT.XMIN)  AX=XMIN
         IF (BX.GT.XMAX)  BX=XMAX

         IF (AX.LE.BX) THEN

* some unclipped span remains

* fix coords
            RXA(1)=AX
            RYA(1)=AY
            RXA(2)=BX
            RYA(2)=BY
* output
            CALL LINSUB(2,RXA,RYA)

         ENDIF
*        .. span not all clipped
      ENDIF
*     .. scanline not clipped

      END
