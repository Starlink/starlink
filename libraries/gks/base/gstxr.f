C# IL>=a, OL>=1
      SUBROUTINE GSTXR (IWKID, ITXI, IFONT, IPREC, RCHXP, RCHSP,
     :                     ITXCI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Set Text Representation
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set the Text bundle (ITXI) on workstation (IWKID)
*
*  MAINTENANCE LOG
*  ---------------
*     25/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     21/08/85  RMK   Change made by Salford, April 1985:
*                     split IF statement which tested whether a variable
*                     was non-zero and also used it as an array index
*                     (S106).
*     19/02/86  DCS   Error checks removed as GKCTXG does not generate
*                     error and change to GCLWK ensures GKCTXM will not
*                     generate error for unknown metafile index.
*     20/01/87  ARG   IS conversion. Error numbers changed. Additional
*                     error detected.
*     20/01/87  DCS   IS conversion. Remove metafile last source case.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation Identifier
*     INP   ITXI   Text Index
*     INP   IFONT  Font
*     INP   IPREC  Precision  ( STRING, CHAR, STROKE )
*     INP   RCHXP  Character Expansion Factor
*     INP   RCHSP  Character Spacing Factor
*     INP   ITXCI  Text Colour Index
*
      INTEGER IWKID, ITXI, IFONT, IPREC, ITXCI
      REAL    RCHXP, RCHSP
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYERR/   Access and set error status
*     Read   /GKYSL/    Last source flags
*     Modify /GKYWCA/   Send KWI1-4, QWR1-2
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
*
*  ERRORS
*  ------
*     72   Text index is invalid
*     75   Text font is equal to zero
*     77   Character expansion factor is less than or equal to zero
*     93   Colour index is invalid
*     2000 Enumeration type out of range
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
*     76   Raised by workstation
*
*---------------------------------------------------------------------


*     Check GKS state

      CALL GKPRLG (ESTXR, GWSOP, GSGOP)
      IF (KERROR .EQ. 0) THEN

*        Check arguments

         IF (ITXI .LT. 1) THEN
            KERROR = 72
         ELSE IF (IFONT .EQ. 0) THEN
            KERROR = 75
         ELSE IF (IPREC .LT. GSTRP .OR. IPREC .GT. GSTRKP) THEN
            KERROR = 2000
         ELSE IF (RCHXP .LE. 0.0) THEN
            KERROR = 77
         ELSE IF (ITXCI .LT. 0) THEN
            KERROR = 93
         END IF
      END IF

*        Set the representation on the workstation

      IF (KERROR .EQ. 0 ) THEN
         KWI1 = ITXI
         KWI2 = IFONT
         KWI3 = IPREC
         QWR1 = RCHXP
         QWR2 = RCHSP
         KWI4 = ITXCI
         CALL GKSONW(IWKID,KSTXR,1,KDAT,1,QDAT,QDAT,1,CH)
      END IF

      IF (KERROR .EQ. 0) THEN
         IF ((KSTXWK .EQ. KGKSFN) .AND. (ITXI .EQ. KCTXI)) THEN
*           GKS
            CALL GKCTXG
            CALL GKSONW(IWKID,KSTXA,1,KDAT,1,QDAT,QDAT,1,CH)
         END IF
         IF (KRGN) CALL GKRGN
      END IF

*     Error Handler

      IF (KERROR .NE. 0) CALL GKERR(KERROR)

      END
