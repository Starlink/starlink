C# IL>=a, OL>=0
      SUBROUTINE GQCR (IWKID,ICO,ITYPE,IER,RED,GREEN,BLUE)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE COLOUR REPRESENTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns colour representation.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     22/12/83  AS    Change check on colour index to allow 0
*     22/01/87  JCS   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP IWKID  - workstation identifier
*     INP ICO    - colour index
*     INP ITYPE  - type of returned values
*     OUT IER    - error indicator
*     OUT RED    -
*     OUT GREEN  - colour intensities
*     OUT BLUE   -
*
      INTEGER IWKID, ICO, ITYPE, IER
      REAL RED,GREEN,BLUE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKS/    wsop,sgop
*     Read   /GKWKE/  KQCR
*     Modify /GKWCA/  KWI1,KWI2,QWR1,QWR2,QWR3,KDAT,QDAT,KERROR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     93  Colour index is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG(KNIL,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN

        IF (ICO.LT.0) THEN
          IER = 93
        ELSE
          KWI1 = ICO
          KWI2 = ITYPE
          CALL GKSONW(IWKID,KQCR,1,KDAT,1,QDAT,QDAT,1,CH)
          IER = KERROR
          IF (KERROR.EQ.0) THEN
            RED   = QWR1
            GREEN = QWR2
            BLUE  = QWR3
          ENDIF
        ENDIF

      ELSE

        IER = KERROR

      ENDIF

      END
