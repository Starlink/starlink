C# IL>=a, OL>=0
      SUBROUTINE GKPCSI
*
* (C) COPYRIGHT ICL & SERC  1985
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CSS : Skip item arguments (part of pick echoplay interface)
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilized
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkcss.cmn'
*
*  LOCALS
*  ------
*     NID    Number of integer arguments
*     NRD    Number of coordinate pairs
*     NCD    Length of string argument
*
      INTEGER NID,NRD,NCD
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      NID=KCSI
      NRD=KCSR
      NCD=KCSC

      CALL GKPCXI(NID)
      CALL GKPCXP(NRD)
      CALL GKPCXC(NCD)

      KCSENT=KNIL

      END
