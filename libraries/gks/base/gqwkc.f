C# IL>=a, OL>=0
      SUBROUTINE GQWKC (IWKID,IER,ICONID,IWKTYP)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE WORKSTATION CONNECTION AND TYPE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns connection identifier and workstation type.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWKID  - workstation identifier
*     OUT IER    - error indicator
*     OUT ICONID - connection identifier
*     OUT IWKTYP - workstation type
*
      INTEGER IWKID, IER, ICONID, IWKTYP
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        CALL GKSONW(IWKID,KQWKC,1,KDAT,1,QDAT,QDAT,1,CH)
        IF (KERROR.EQ.0) THEN
          ICONID = KWI1
          IWKTYP = KWI2
        ENDIF
      ENDIF
      IER = KERROR

      END
