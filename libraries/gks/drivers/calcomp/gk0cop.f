

*---------------------------------------------------------------
      SUBROUTINE GK0COP
*---------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CC81 Flush buffer
*
*  MAINTENANCE LOG
*  ---------------
*     30/03/84  MGC  Original version stabilized
*
*  ARGUMENTS
*  ---------
*      None
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
      INTEGER IXPEN, IYPEN
      PARAMETER (IXPEN=2, IYPEN=3)

*  ALGORITHM
*  ---------
*     .PEN UP  : H
*
*  COMMENTS
*  --------
*  Pen position set unknown
*
* --------------------------------------------------------------

      CALL GK0CPB(72)
      CALL GK0CPA(KIOSN,1,KDAT)
      KWKDAT(IXPEN,KWKIX)=KNIL
      KWKDAT(IYPEN,KWKIX)=KNIL
      RETURN
      END
