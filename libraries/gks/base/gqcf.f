C# IL>=a, OL>=0
      SUBROUTINE GQCF (IWTYPE,IER,NCOLI,ICOLA,NPCI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE COLOUR FACILITIES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns colour facilities
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     OUT IER    - error indicator
*     OUT NCOLI  - number of available colours or intensities
*     OUT ICOLA  - colour available
*     OUT NPCI  - number of predefined colour indices
*
      INTEGER IWTYPE, IER, NCOLI, ICOLA, NPCI
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKWKE/    KQCF
*     Modify /GKWCA/    KDAT,QDAT,KERROR,KWI1,KWI2,KWI3
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        CALL GKSONW(IWTYPE,KQCF,1,KDAT,1,QDAT,QDAT,1,CH)
        IF (KERROR.EQ.0) THEN
          NCOLI = KWI1
          ICOLA = KWI2
          NPCI = KWI3
        ENDIF
      ENDIF
      IER = KERROR

      END
