C# IL>=a, OL>=0
      SUBROUTINE GQPAF (IWTYPE,IER,NPPAI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE PATTERN FACILITIES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns pattern facilities
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     OUT IER    - error indicator
*     OUT NPPAI  - number of predefined pattern indices
*
      INTEGER IWTYPE, IER, NPPAI
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


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        CALL GKSONW(IWTYPE,KQPAF,1,KDAT,1,QDAT,QDAT,1,CH)
        IF (KERROR.EQ.0) THEN
          NPPAI = KWI1
        ENDIF
      ENDIF

      IER = KERROR

      END
