

*----------------------------------------------------------------------
      SUBROUTINE GK0BXY(IX,IY,IPEN,NLEFT)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of Workstation Driver
*  Author:             DRJF
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     BENSON Position at point relative
*
*  MAINTENANCE LOG
*  ---------------
*     01/11/85  DRJF  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IX    x-displacement of point
*     INP IY    y-displacement of point
*     INP IPEN  pen set up or down
*     OUT NLEFT number of bytes left in the current record
*
      INTEGER IX,IY,IPEN,NLEFT
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER    NEGDIS,  LSH2,  RSH6
      PARAMETER (NEGDIS=1,LSH2=4,RSH6=2**6)
      INTEGER IA(4),INT,ISIGN
*
*  ALGORITHM
*  ---------
*
*     A vector order has the following format:
*
*       header dx dy dx dy dx dy . .
*
*     where header contains an 'FF02' and each pair dx dy is
*     an increment, dx being the x coordinate portion, dy the
*     y coordinate.
*
*     The format of dx is:
*
*       bits 0-5  least significant 6 bits of displacement
*       bit  6    zero
*       bit  7    sign of x displacement: 0 is +ve, 1 is -ve
*       bits 8-15 most significant 8 bits of x displacement
*
*     The format of dy is similar, but different in one vital
*     respect, in that bit 6 defines whether the pen should draw
*     (pen down) or move (pen up).
*
*       bits 0-5  least significant 6 bits of displacement
*       bit  6    pen control: 0 gives pen up, 1 pen down
*       bit  7    sign of y displacement: 0 is +ve, 1is -ve
*       bits 8-15 most significant 8 bits of y displacement
*
*     The function of this routine is to construct a dx dy
*     increment and pass it to GKIOFO. This routine puts the
*     first 8 bits of dx in one storage location and the second
*     8 bits in another, likewise for dy. GKIOFO combines the
*     two groups of 8 bits representing dx and puts the result
*     in the current record, likewise for dy.
*
*----------------------------------------------------------------------


*
*     dx
*
      INT=ABS(IX)
      IF (IX.LT.0) THEN
*
*       X displacement of point, negative direction
*
        ISIGN=NEGDIS
      ELSE
*
*       X displacement of point, positive direction
*
        ISIGN=0
      END IF
*
*     Construct X coordinate part of increment. Put the first
*     8 bits in IA(1) and the second 8 bits in IA(2)
*
      IA(1)=MOD(INT,64)*LSH2+ISIGN
      IA(2)=INT/RSH6
*
*     dy
*
      INT=ABS(IY)
      IF (IY.LT.0) THEN
*
*       Y displacement of point, negative direction
*
        ISIGN=NEGDIS
      ELSE
*
*       Y displacement of point, positive direction
*
        ISIGN=0
      END IF
*
*     Construct Y coordinate part of increment. Put the first
*     8 bits in IA(3) and the second 8 bits in IA(4)
*
      IA(3)=MOD(INT,64)*LSH2+ISIGN+IPEN
      IA(4)=INT/RSH6
*
*     Pass the constructed dx dy elements to the buffer
*     routine for entry into the current record
*
      CALL GKIOFO(KIOPB,4,IA,NLEFT)
      RETURN
*
      END
