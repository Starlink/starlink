      SUBROUTINE GSPKM(IWKID,IDNR,IMODE,IESW)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET PICK MODE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send modes to workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     10/05/83  AS   Original version stabilized
*     28/09/83  AS   Change subroutine name
*     17/10/83  AS   Change entrypoint name
*
*  ARGUMENTS
*  ---------
*     INP IWKID  workstation identifier
*     INP IDNR   pick device number
*     INP IMODE  operating mode
*     INP IESW   echo switch
*
      INTEGER IWKID, IDNR, IMODE, IESW
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     143  EVENT and SAMPLE are not available at this level of GKS
*
*---------------------------------------------------------------------


      CALL GKPRLG (ESPKM,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN

* Check operating mode against level of GKS
        IF (IMODE.GT.GREQU) THEN
          CALL GKERR(143)
          GOTO 999
        ENDIF

        KWI1 = GPICK
        KWI2 = IDNR
        KWI3 = IMODE
        KWI4 = IESW
        CALL GKSONW(IWKID,KSIM,1,KDAT,1,QDAT,QDAT,1,CH)
        IF (KERROR.NE.0) CALL GKERR(KERROR)
      ELSE
        CALL GKERR(KERROR)
      ENDIF

  999 CONTINUE
      END
