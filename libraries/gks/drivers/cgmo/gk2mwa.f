      SUBROUTINE GK2MWA(ASPECT,ASF)
*--------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine changes ASFs from 1 to 0 & 0 to 1
*  and also converts GKS aspect numbers to CGM ones
*  and then writes them to the CGM. (the change is
*  because GKS & CGM have 0 & 1 meaning the
*  opposite things.)
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*        ASPECT : Index of ASF
*        ASF    : Aspect Source Flag (individual=0 | bundled=1)
*                      NOTE: this is opposite to GKS
*
      INTEGER ASPECT, ASF
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'

      INCLUDE '../../include/gkwca.cmn'
*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgm.cmn'

*
*  LOCALS
*  ------
*        INTASF : Internal Aspect Source Flag (as for ASF)
*
      INTEGER INTASF
*
*---------------------------------------------------------------

      IF(ASF.EQ.0)THEN
         INTASF=1
      ELSE
         INTASF=0
      ENDIF

*   Write Aspect to CGM & Aspect source to CGM (Converting from GKS to
*   CGM Aspects
      IF(ASPECT.LE.6.OR.ASPECT.EQ.13) THEN
         CALL GK2MIW(1,ASPECT-1)
         CALL GK2MIW(1,INTASF)
      ELSEIF(ASPECT.EQ.7)THEN
         CALL GK2MIW(1,ASPECT-1)
         CALL GK2MIW(1,INTASF)
         CALL GK2MIW(1,ASPECT)
         CALL GK2MIW(1,INTASF)
      ELSEIF(ASPECT.EQ.12) THEN
         CALL GK2MIW(1,ASPECT+1)
         CALL GK2MIW(1,INTASF)
         CALL GK2MIW(1,ASPECT+2)
         CALL GK2MIW(1,INTASF)
      ELSE
         CALL GK2MIW(1,ASPECT)
         CALL GK2MIW(1,INTASF)
      ENDIF
      RETURN
      END
