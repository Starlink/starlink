C# IL>=a, OL>=0
      SUBROUTINE GSCR (IWKID, ICLI, RED, GREEN, BLUE)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Set Colour Representation
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set the Colour bundle (ICLI) on workstation (IWKID)
*
*  MAINTENANCE LOG
*  ---------------
*     25/05/83  CJW   Original version stabilized
*     08/06/83  AS    Change name from GSCLR to GSCR,fix checking on RGB
*     13/06/83  AS    Fix check on colour index
*     27/06/83  CJW   Implement revised error handling precedure
*     20/01/87  ARG   IS conversion. Error numbers changed. Report any
*                     workstation errors.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation Identifier
*     INP   ICLI   Colour Index
*     INP   RED    Red intensity
*     INP   GREEN  Green intensity
*     INP   BLUE   Blue intensity
*
      INTEGER IWKID, ICLI
      REAL    RED, GREEN, BLUE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYSL/    Last source flags
*     Modify /GKYERR/   Set error status
*     Modify /GKYWCA/   Send KWI1, QWR1-3 receive KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  ERRORS
*  ------
*       93  Colour index is invalid
*       96  Colour is outside the range [0,1]
*
*  COMMENTS
*  --------
*
*    GKS says that this function shall also result in error #
*
*        7  Raised by prologue
*       20  Raised by call layer
*       25  Raised by call layer
*       33  Raised by call layer
*       35  Raised by call layer
*       36  Raised by call layer
*       83  Raised by workstation
*
*---------------------------------------------------------------------


*     Check GKS state

      CALL GKPRLG (ESCR, GWSOP, GSGOP)

      IF (KERROR .EQ. 0) THEN

*     Check arguments

         IF (ICLI .LT. 0) THEN
            KERROR = 93
         ELSE IF ( (RED.LT.0.0)   .OR. (RED.GT.1.0)    .OR.
     :             (GREEN.LT.0.0) .OR. (GREEN.GT.1.0)  .OR.
     :             (BLUE.LT.0.0)  .OR. (BLUE.GT.1.0))    THEN
            KERROR = 96
         END IF

      END IF

      IF (KERROR .EQ. 0) THEN

*        Set the representation on the workstation

         KWI1 = ICLI
         QWR1 = RED
         QWR2 = GREEN
         QWR3 = BLUE
         CALL GKSONW(IWKID,KSCR,1,KDAT,1,QDAT,QDAT,1,CH)
         IF (KERROR.EQ.0 .AND. KRGN) THEN
            CALL GKRGN
         END IF
      END IF

      IF (KERROR .NE. 0) CALL GKERR(KERROR)

      END
