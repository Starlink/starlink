C# IL>=a, OL>=1
      SUBROUTINE GQFAR (IWKID,IFA,ITYPE,IER,ISTYLE,INDSTY,IC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE FILL AREA REPRESENTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns fill area representation.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     22/01/87  JCS   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP IWKID  - workstation identifier
*     INP IFA    - fill area index
*     INP ITYPE  - type of returned values
*     OUT IER    - error indicator
*     OUT ISTYLE - fill area interior style
*     OUT INDSTY - fill area style index
*     OUT IC     - fill area colour index
*
      INTEGER IWKID, IFA, ITYPE, IER, ISTYLE, INDSTY, IC
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKS/      wsop,sgop
*     Read   /GKWKE/    KQFAR
*     Modify /GKWCA/    KWI1,KWI2,KWI3,KDAT,QDAT,KERROR,KQFAR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     80   Fill area index is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN

        IF (IFA.LE.0) THEN
          IER = 80
        ELSE
          KWI1 = IFA
          KWI2 = ITYPE
          CALL GKSONW(IWKID,KQFAR,1,KDAT,1,QDAT,QDAT,1,CH)
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
