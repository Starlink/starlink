* --------------------------------------------------------------



      SUBROUTINE GK0TSL(LNTYPE, SOFT, PATPD)

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (part of) Workstation Driver
*  Author:             RMK
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To set the hardware linestyle if the device is capable of this,
*     and set linetype variables to pass to utilities.
*
*  MAINTENANCE LOG
*  ---------------
*     29/04/86  RMK   Original version stabilized.
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     IN   LNTYP   Linestyle required
*     OUT  SOFT    .TRUE. if utilities must simulate linestyles
*     OUT  PATPD   The repeat length for linestyle simulation
*
      INTEGER LNTYPE
      LOGICAL SOFT
      REAL PATPD
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     ILTS     Offset in KWKDAT for whether number of hardware
*              linestyles if 1 (GLOWER) or 4 (GHIGHR)
*     INTA(2)  Array to send bytes to buffer, first element is ESC
*     NLEFT    Space left in buffer
*     LTTABL(4)  Table of codes to select linestyles
*
      INTEGER ILTS, INTA(2), NLEFT, LTTABL(4)
      PARAMETER (ILTS=3)
      DATA INTA(1)/27/
      DATA LTTABL/96, 100, 97, 98/
*                  '   d    a   b
*
*-----------------------------------------------------------------------

* For Pericom Monterey no action if solid line in background
* colour is requested. Two reasons here: a).  When in "Line
* Erase" Mode, the solid line is the default linestyle. b).
* The Escape sequence for selecting a solid line linetype is
* identical to the "Exit Erase Line Mode" sequence.
      IF(KWKTYP.EQ.820.AND.KWPLCI(KWKIX).EQ.0.AND.
     : LNTYPE.EQ.1) RETURN
* Same as above, only for the Pericom with RAL mods.
      IF(KWKTYP.EQ.821.AND.KWPLCI(KWKIX).EQ.0.AND.
     : LNTYPE.EQ.1) RETURN
* Decide whether hardware linestyles can be used, and if so
* send command to device
      IF (KWKDAT(ILTS,KWKIX).EQ.GHIGHR.AND.LNTYPE.GE.1.AND.
     : LNTYPE.LE.4) THEN
        SOFT = .FALSE.
        INTA(2) = LTTABL(LNTYPE)
        CALL GKIOBO(KIOPB, 2, INTA, NLEFT)
      ELSE
        SOFT = .TRUE.
      ENDIF
*
* Set correct repeat length for software linestyles
      IF (KDSRX(KWKIX).EQ.4096) THEN
        PATPD = 100.0
      ELSE
        PATPD = 25.0
      ENDIF

      END
