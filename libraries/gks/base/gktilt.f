C# IL>=a, OL>=0
      SUBROUTINE GKTILT(AX,AY,BX,BY,LINSUB,XMIN,XMAX,YMIN,YMAX)
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
*     Rotates Hatch line end points back to DC frame and outputs them
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/83  NGB   Original version stabilized
*     29/03/83  NGB   Coordinate Truncation included
*     29/11/83  NGB   Redundant Arguments IX,IY,IA removed
*     21/02/84  MGC   Ensure Hatch line is clipped
*     20/08/85  RMK   Changed 5th parameter in call to GKLCLP
*                     from integer to real (S124 - Salford)
*     22/07/88  KEVP  Replaced IFIX with machine independent INT
*
*  ARGUMENTS
*  ---------
*     INP AX,AY  End A Coords
*     INP BX,BY  End B Coords
*     INP LINSUB Line Output Routine
*     INP XMIN,XMAX,YMIN,YMAX  Clipping Frame
*
      REAL AX, AY, BX, BY, XMIN, XMAX, YMIN, YMAX
      EXTERNAL LINSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*
      REAL    RXA(2),RYA(2)
*
*  ALGORITHM
*  ---------
*     Uses rotation coefficients left in the Comms Area by GKROTV
*
*  COMMENTS
*  --------
*     For consistency, the policy for all Pixel operations is to
*     truncate coordinates to the next lower pixel.
*
*---------------------------------------------------------------------



* Rotate end points back again
      RXA(1)=FLOAT(INT(AX*QWR8 - AY*QWR7))
      RYA(1)=FLOAT(INT(AX*QWR7 + AY*QWR8))
      RXA(2)=FLOAT(INT(BX*QWR8 - BY*QWR7))
      RYA(2)=FLOAT(INT(BX*QWR7 + BY*QWR8))

      CALL GKLCLP(2,RXA,RYA, .FALSE. ,0.0,
     :               XMIN,YMIN,XMAX,YMAX,LINSUB)

      END
