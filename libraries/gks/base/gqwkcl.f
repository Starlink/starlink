C# IL>=a, OL>=0
      SUBROUTINE GQWKCL (IWTYPE,IER,IVRTYP)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE WORKSTATION CLASSIFICATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns workstation classification
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     22/01/87  JCS   IS conversion. Check WS is OUTIN or OUTPUT.
*     03/04/87  RMK   Added INCLUDE for GKS.PAR.
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     OUT IER    - error indicator
*     OUT IVRTYP - vector,raster, or other type
*
      INTEGER IWTYPE, IER, IVRTYP
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkwke.par'
*
*  LOCALS
*  ------
*
      INTEGER I, IWKCAT
*
*---------------------------------------------------------------------


      CALL GKQINT(IWTYPE, KQWKCA, IER, IWKCAT, I, I, I, I, I, I)
      IF (IER .EQ. 0) THEN

        IF (IWKCAT .EQ. GOUTPT  .OR. IWKCAT .EQ. GOUTIN) THEN

          CALL GKQINT(IWTYPE, KQWKCL, IER, IVRTYP, I, I, I, I, I, I)

        ELSE

          IER = 39

        ENDIF

      ENDIF
      CALL GKQINT(IWTYPE,KQWKCL,IER,IVRTYP,I,I,I,I,I,I)

      END
