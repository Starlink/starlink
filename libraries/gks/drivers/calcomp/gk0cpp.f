*
*
*
*-------------------------------------------------------------
      SUBROUTINE GK0CPP(IX,IY)
*----------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CC81 Position at point
*
*  MAINTENANCE LOG
*  ---------------
*     07/03/84  MGC  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IX    x-coordinate of point
*     INP IY    y-coordinate of point
*
      INTEGER IX,IY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
*
*  LOCALS
*  ------
*
      INTEGER NLEFT
      INTEGER IXPEN, IYPEN
      PARAMETER (IXPEN=2, IYPEN=3)
*
*  ALGORITHM
*  ---------
*     .END                 : <SP>
*     .MOVE PEN TO (IX,IY) : <IX>/<IY>K
*
* --------------------------------------------------------------

      CALL GK0CPB(32)
      CALL GKIOBO(KIOQS,1,KDAT,NLEFT)
      IF(NLEFT.LT.12) CALL GK0CPA(KIOSN,1,KDAT)
      CALL GK0CPN(IX)
      CALL GK0CPB(47)
      CALL GK0CPN(IY)
      CALL GK0CPB(75)
      KWKDAT(IXPEN,KWKIX)=IX
      KWKDAT(IYPEN,KWKIX)=IY
      RETURN
      END
