C# IL>=a, OL>=1
      SUBROUTINE GCLSG
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  CLOSE SEGMENT
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level close segment.
*
*  MAINTENANCE LOG
*  ---------------
*     18/11/83  JGW   Original version stabilised
*     18/11/83  AS    Include CHECK.INC, other minor changes
*     16/12/83  JRG   Set KOPS to be WsActive
*     03/03/84  JGW   Improved handling of de-activate CSS
*     03/03/84  JGW   Removed unnecessary 'set catchup'
*     18/10/85  RSK   (I218,I233?) reset all catchup flags
*     03/06/87  RMK   Changed to only set necessary catchup flags.
*                     Removed unused local variables.
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /SL/     KSTRWK, KSFAWK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkerr.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
*
*  LOCALS
*  ------
*
*  ERRORS
*  ------
*      4    Not GSGOP
*
*-----------------------------------------------------------------------
*
      CALL GKPRLG(ECLSG,GSGOP,GSGOP)
      IF (KERROR.EQ.0) THEN
         KSGRQ = 0
         CALL GKSACW(KCLSG,1,KDAT,1,QDAT,QDAT,1,CH)
*        -- deactivate CSS --
         IF (KACPT(KNACWK) .EQ. KCSWIX)  KNACWK = KNACWK - 1
*        -- reset GKS state --
         KOPS   = GWSAC
         KCVIS  = GVISI
         KCHLT  = GNORML
*        -- set catch up flags for transformation, text and fill area --
         CALL GKTOLD
      ENDIF

      IF (KERROR.NE.0)  CALL GKERR(KERROR)
      END
