C# IL>=a, OL>=0
      SUBROUTINE GKCASM(LNSUB)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation Utility
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To perform cell array minimal simulation (i.e. draw boundary)
*
*  MAINTENANCE LOG
*  ---------------
*     23/07/84  JRG   First created
*
*  ARGUMENTS
*  ---------
*     INP  LNSUB   Polyline drawing subroutine for device
*
      EXTERNAL LNSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKWCA/  QWR1 ... QWR6 for P,Q,R points
*     Read   /GKWKD/  Total clipping rectangle on workstation
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     XP,YP    Coordinates of points of boundary in World Coordinates
*    DXP,DYP   Coordinates of points of boundary in Device Coordinates
*
      REAL XP(5),YP(5), DXP(5),DYP(5)
*
*  ERRORS
*  ------
*     None
*
*-----------------------------------------------------------------------


*   Construct points to draw parallelogram that describes
*   boundary P, R, Q, 4th point, P.  The 4th point is obtained
*   by subtracting the vector P->R from Q
      XP(1)=QWR1
      YP(1)=QWR2
      XP(2)=QWR5
      YP(2)=QWR6
      XP(3)=QWR3
      YP(3)=QWR4
      XP(4)=QWR3 - (QWR5 - QWR1)
      YP(4)=QWR4 - (QWR6 - QWR2)
      XP(5)=QWR1
      YP(5)=QWR2

*   Transform, clip and draw
      CALL GKTWD(5,XP,YP,DXP,DYP)
      CALL GKLCLP(5,DXP,DYP, .FALSE., 1.0,
     :   QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),QWCLYT(KWKIX),
     :   LNSUB)

      END
