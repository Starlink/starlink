C# IL>=a, OL>=0
      SUBROUTINE GQPFAR (IWTYPE,IFA,IER,ISTYLE,INDSTY,IC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE PREDEFINED FILL AREA REPRESENTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns predefined fill area representation.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     22/01/87  JCS   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     INP IFA    - fill area index
*     OUT IER    - error indicator
*     OUT ISTYLE - fill area interior style
*     OUT INDSTY - fill area style index
*     OUT IC     - fill area colour index
*
      INTEGER IWTYPE, IFA, IER, ISTYLE, INDSTY, IC
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     80  Fill area index is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN

        IF (IFA.LE.0) THEN
          IER = 80
        ELSE
          KWI1 = IFA
          CALL GKSONW(IWTYPE,KQPFAR,1,KDAT,1,QDAT,QDAT,1,CH)
          IER = KERROR
          IF (KERROR.EQ.0) THEN
            ISTYLE = KWI1
            INDSTY = KWI2
            IC = KWI3
          ENDIF

        ENDIF

      ELSE
        IER = KERROR
      ENDIF

      END
