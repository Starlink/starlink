

*---------------------------------------------------------------
      SUBROUTINE GK0CIE
*---------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CC81 Finish with device
*
*  MAINTENANCE LOG
*  ---------------
*     00/99/84  MGC  Original version stabilized
*     09/07/84  RMK  Removed ETX at end, as now sent in handshaking.
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
      INTEGER ILIE
      PARAMETER (ILIE=5)
      INTEGER ICIE(ILIE)
*                H SP  F  0 SP
      DATA ICIE/72,32,70,48,32/
*
*  ALGORITHM
*  ---------
*     .PEN UP      : H
*     .END         : <SP>
*     .UNLOAD PEN  : F0
*     .END         : <SP>
*
* --------------------------------------------------------------

      CALL GK0CPA(KIOPB,ILIE,ICIE)
      CALL GK0CPA(KIOSN,1,KDAT)
      RETURN
      END
