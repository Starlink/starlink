C# IL>=a, OL>=0
      SUBROUTINE GQWKDU (IWKID,IER,IDFMOD,IRGMOD,IDSMT,NFRAME)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE WORKSTATION DEFERRAL AND UPDATE STATES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns deferral and update states.
*
*  MAINTENANCE LOG
*  ---------------
*     04/10/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWKID  - workstation identifier
*     OUT IER    - error indicator
*     OUT IDFMOD - deferral mode
*     OUT IRGMOD - implicit regeneration mode
*     OUT IDSMT  - display surface empty
*     OUT NFRAME - new frame action necessary at update
*
      INTEGER IWKID, IER, IDFMOD, IRGMOD, IDSMT, NFRAME
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
        CALL GKSONW(IWKID,KQWKDU,1,KDAT,1,QDAT,QDAT,1,CH)
        IF (KERROR.EQ.0) THEN
          IDFMOD = KWI1
          IRGMOD = KWI2
          IDSMT  = KWI3
          NFRAME = KWI4
        ENDIF
      ENDIF
      IER = KERROR

      END
