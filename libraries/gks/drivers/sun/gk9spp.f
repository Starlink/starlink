      SUBROUTINE GK9SPP(ITYPE,XP,YP,NR,RX,RY,BOXDEF,SQPKAP,
     :                  SQDIST,INSIDE)
*
*---------------------------------------------------------------------
*
*  Author:             RMK, based on GK0TPP by KEVP
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Primitive Pick scan routine
*
*  ARGUMENTS
*  ---------
*     INP  ITYPE   Type of primitive
*     INP  XP,YP   The Pick Point
*     INP  NR      Number of points
*     INP  RX,RY   The points (coord system determined by ITYPE)
*     INP  BOXDEF  True, if bounding box needs defining
*     INP  SQPKAP  Squared Pick aperture in Square DC units
*     I/O  SQDIST  Nearest squared distance any primitive so far
*                  in Square DC units
*     OUT  INSIDE  True, if pick point is inside primitive
*
      INTEGER ITYPE, NR
      REAL XP,YP, RX(NR),RY(NR), SQPKAP, SQDIST
      LOGICAL BOXDEF, INSIDE
*
*---------------------------------------------------------------------
      CALL GKPPPR(ITYPE,XP,YP,NR,RX,RY,BOXDEF,0.25,SQPKAP,
     :            SQDIST,INSIDE)
      END
