

* --------------------------------------------------------------
      SUBROUTINE GK0CXP(IFID,ICHH,ICHW,ICHA)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CC81 Set text properties
*
*  MAINTENANCE LOG
*  ---------------
*     07/03/84  MGC  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier [1..5]
*     INP ICHH     Character height
*     INP ICHW     Character width
*     INP ICHA     Angle of rotation
*
      INTEGER IFID,ICHH,ICHW,ICHA
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
      INTEGER NLEFT
*
*  ALGORITHM
*  ---------
*
*     .DEF CHAR FONT               : #<IFID-1>
*     .DEF CHAR HEIGHT,ANGLE,WIDTH : Z<ICHH>,<ICHA>,<ICHW>
*     .END                         : <SP>
*
* --------------------------------------------------------------------

      CALL GKIOBO(KIOQS,1,KDAT,NLEFT)
      IF(NLEFT.LT.23) CALL GK0CPA(KIOSN,1,KDAT)
      CALL GK0CPB(35)
      CALL GK0CPN(IFID-1)
      CALL GK0CPB(90)
      CALL GK0CPN(ICHH)
      CALL GK0CPB(44)
      CALL GK0CPN(ICHA)
      CALL GK0CPB(44)
      CALL GK0CPN(ICHW)
      CALL GK0CPB(32)
      RETURN
      END
