C# IL>=a, OL>=0
      SUBROUTINE GQPCR (IWTYPE,ICO,IER,RED,GREEN,BLUE)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE PREDEFINED COLOUR REPRESENTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns predefined colour representation.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     22/12/83  AS    Change check on colour index to allow 0
*     22/01/87  JCS   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     INP ICO    - colour index
*     OUT IER    - error indicator
*     OUT RED    -
*     OUT GREEN  - colour intensities
*     OUT BLUE   -
*
      INTEGER IWTYPE, ICO, IER
      REAL RED,GREEN,BLUE
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
*  ERRORS
*  ------
*     93  Colour index is invalid.
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN

        IF (ICO.LT.0) THEN
          IER = 93
        ELSE
          KWI1 = ICO
          CALL GKSONW(IWTYPE,KQPCR,1,KDAT,1,QDAT,QDAT,1,CH)
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
