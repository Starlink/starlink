C# IL>=a, OL>=0
      SUBROUTINE GSLN(LTYPE)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET LINETYPE
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set linetype
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83  FY    Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value of KSPLWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   LTYPE  linetype
*
      INTEGER LTYPE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCLNTY,KSPLWK
*     Read   /GKYSL/   KCPLAF
*     Read   /gkysl/   khange,kgksfn..(par)
*     Read   /aspct/  klntya..(par)
*     Read   /gks/     indivi..(par)
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gaspct.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     63     Linetype is equal to zero
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESLN,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (LTYPE .EQ. 0) THEN
          CALL GKERR(63)
        ELSE
          KCLNTY = LTYPE
          IF (KCPLAF(KLNTYA) .EQ. GINDIV) KSPLWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
