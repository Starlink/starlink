C# IL>=a, OL>=1
      SUBROUTINE GSPAR (IWKID,IPAI,IDIMX,IDIMY,ISC,ISR,IDX,IDY,IPATN)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Set Pattern Representation
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set the Pattern bundle (IPAI) on workstation (IWKID)
*
*  MAINTENANCE LOG
*  ---------------
*     25/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     24/11/83  AS    Add extra argument IDIMX
*     20/01/87  RMK   IS conversion. Language binding changes - leads to
*                     change in interface to workstation. Altered error
*                     numbers.
*     26/06/87  RMK   Corrected error 91 test.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation Identifier
*     INP   IPAI   Pattern Index
*     INP   IDIMX  X dimension of IPATN
*     INP   IDIMY  Y dimension of IPATN
*     INP   ISC    Index to start column
*     INP   ISR    Index to start row
*     INP   IDX    Number of columns
*     INP   IDY    Number of rows
*     INP   IPATN  Pattern Colour Index
*
      INTEGER IWKID, IPAI, IDIMX, IDIMY, ISC, ISR, IDX, IDY
      INTEGER IPATN(IDIMX,IDIMY)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYERR/   Set error status
*     Read   /GKYSL/    Last source flags
*     Modify /GKYWCA/   Send KWI1-3, IPATN receive KERROR
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
*       85  Specified pattern index is invalid
*       91  Dimensions of colour array are invalid
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
*       93  Colour index is invalid
*
*---------------------------------------------------------------------


*     Check GKS state

      CALL GKPRLG (ESPAR, GWSOP, GSGOP)

      IF (KERROR .EQ. 0) THEN

*     Check arguments

         IF (IPAI .LT. 1) THEN
            KERROR = 85
         ELSE IF (IDIMX.LE.0 .OR. IDIMY.LE.0 .OR. ISC.LE.0 .OR.
     :            ISR.LE.0 .OR. IDX.LE.0 .OR. IDY.LE.0 .OR.
     :            ISC+IDX-1.GT.IDIMX .OR. ISR+IDY-1.GT.IDIMY) THEN
            KERROR = 91
         ELSE

*           Set the representation on the workstation

            KWI1 = IPAI
            KWI2 = IDIMX
            KWI3 = IDIMY
            KWI4 = ISC
            KWI5 = ISR
            KWI6 = IDX
            KWI7 = IDY
            CALL GKSONW(IWKID,KSPAR,IDIMX*IDIMY,IPATN,1,QDAT,QDAT,1,CH)
            IF (KRGN) THEN
               CALL GKRGN
            END IF
         END IF
      END IF

*     Error Handler

      IF (KERROR .NE. 0) CALL GKERR(KERROR)

      END
