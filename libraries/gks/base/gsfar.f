C# IL>=a, OL>=1
      SUBROUTINE GSFAR (IWKID, IFAI, IFAIS, IFASI, IFACI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Set Fill Area Representation
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set the Fill Area bundle (IFAI) on workstation (IWKID)
*
*  MAINTENANCE LOG
*  ---------------
*     25/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     16/03/84  CJW   S25 - check fill area style
*     16/04/84  PGLS  Only check style index if pattern or hatch.
*                     Don't check colour index if pattern.
*                     Change error from 79 to 78.
*     20/08/85  RMK   Split IF statement which tested whether a variable
*                     was non-zero and also used it as an array index
*                     (S106, S144).
*     16/01/86  DRJF  Bug fix S116. Change error from 78 to 79 for invalid
*                     pattern index. Add error 80 for invalid hatch style.
*     19/01/87  ARG   IS conversion. Error numbers changed.
*     19/01/87  RMK   IS conversion. Style index check changed.
*     20/01/87  DCS   IS conversion. Remove error check after GKCFAG
*                     (cannot generate error) and metafile last source
*                     case.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation Identifier
*     INP   IFAI   Fill Area Index
*     INP   IFAIS  Fill Area Interior Style
*     INP   IFASI  Fill Area Style Index
*     INP   IFACI  Fill Area Colour Index
*
      INTEGER IWKID, IFAI, IFAIS, IFASI, IFACI
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYERR/   Set error status
*     Read   /GKYSL/    Last source flags
*     Modify /GKYWCA/   Send KWI1-4, receive KERROR
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
*       80  Fill Area index is invalid
*       85  Specified pattern index is invalid
*       86  Style (pattern or hatch) is not supported on this wkstn
*       93  Colour index is invalid
*     2000  Enumerated type invalid
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
*       86  Raised by workstation
*
*---------------------------------------------------------------------


*     Check GKS state

      CALL GKPRLG (ESFAR, GWSOP, GSGOP)

      IF (KERROR .EQ. 0) THEN

*        Check arguments

         IF (IFAI .LT. 1) THEN
            KERROR = 80
         ELSE IF ((IFAIS.LT.GHOLLO) .OR. (IFAIS.GT.GHATCH)) THEN
            KERROR = 2000
         ELSE IF (IFAIS .EQ. GPATTR) THEN
            IF (IFASI .LE. 0) KERROR = 85
         ELSE IF (IFAIS .EQ. GHATCH) THEN
            IF (IFASI .EQ. 0) KERROR = 86
         ELSE IF ((IFAIS .NE. GPATTR).AND.(IFACI .LT. 0)) THEN
            KERROR = 93
         END IF

      END IF

      IF (KERROR .EQ. 0) THEN

*        Set the representation on the workstation

         KWI1 = IFAI
         KWI2 = IFAIS
         KWI3 = IFASI
         KWI4 = IFACI
         CALL GKSONW(IWKID,KSFAR,1,KDAT,1,QDAT,QDAT,1,CH)
         IF ((KSFAWK .EQ. KGKSFN) .AND. (IFAI .EQ. KCFAI)) THEN
*           GKS
            CALL GKCFAG
            CALL GKSONW(IWKID,KSFAA,1,KDAT,1,QDAT,QDAT,1,CH)
         END IF
         IF (KRGN) CALL GKRGN

      END IF

      IF (KERROR .NE. 0) CALL GKERR(KERROR)

      END
