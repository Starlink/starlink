C# IL>=a, OL>=1
      SUBROUTINE GSPMR (IWKID, IPMI, IMKTY, RMKSF, IPMCI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Set Polymarker Representation
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set the Polymarker bundle (IPMI) on workstation (IWKID)
*
*  MAINTENANCE LOG
*  ---------------
*     25/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     21/08/85  RMK   Split IF statement which tested whether a variable was
*                     non-zero and also used it as an array index
*                     (S106, S145 - Salford).
*     20/01/87  ARG   IS conversion. New error detected and error
*                     numbers changed.
*     20/01/87  DCS   IS conversion. Remove error check after GKCPMG
*                     (cannot generate error) and metafile last source
*                     case.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation Identifier
*     INP   IPMI   Polymarker Index
*     INP   IMKTY  Markertype
*     INP   RMKSF  Marker scale factor
*     INP   IPMCI  Polymarker Colour Index
*
      INTEGER IWKID, IPMI, IMKTY, IPMCI
      REAL RMKSF
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYSL/    Last source flags
*     Modify /GKYERR/   Set error status
*     Modify /GKYWCA/   Send KWI1-3, QWR1, receive KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  ERRORS
*  ------
*     66   Polymarker index is invalid
*     69   Marker type is equal to zero
*     71   Marker size scale factor is less than zero
*     93   Colour index is invalid
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
*     70   Raised by workstation
*
*---------------------------------------------------------------------


*     Check GKS state

      CALL GKPRLG (ESPMR, GWSOP, GSGOP)

      IF (KERROR .EQ. 0) THEN

*        Check arguments

         IF (IPMI .LT. 1) THEN
            KERROR = 66
         ELSE IF (IMKTY .EQ. 0) THEN
            KERROR = 69
         ELSE IF (RMKSF .LT. 0.0) THEN
            KERROR = 71
         ELSE IF (IPMCI .LT. 0) THEN
            KERROR = 93
         END IF

      END IF

      IF (KERROR .EQ. 0) THEN

*        Set the representation on the workstation

         KWI1 = IPMI
         KWI2 = IMKTY
         QWR1 = RMKSF
         KWI3 = IPMCI
         CALL GKSONW(IWKID,KSPMR,1,KDAT,1,QDAT,QDAT,1,CH)
         IF ((KSPMWK .EQ. KGKSFN) .AND. (IPMI .EQ. KCPMI)) THEN
*           GKS
            CALL GKCPMG
            CALL GKSONW(IWKID,KSPMA,1,KDAT,1,QDAT,QDAT,1,CH)
         END IF
         IF (KRGN) THEN
            CALL GKRGN
         END IF
      END IF

      IF (KERROR .NE. 0) CALL GKERR(KERROR)
      END
