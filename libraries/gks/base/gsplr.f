C# IL>=a, OL>=1
      SUBROUTINE GSPLR (IWKID, IPLI, ILNTY, RLNWD, IPLCI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Set Polyline Representation
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set the Polyline bundle (IPLI) on workstation (IWKID)
*
*  MAINTENANCE LOG
*  ---------------
*     25/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     21/08/85  RMK   Split IF statement which tested whether a variable
*                     was non-zero and also used it as an array index (S106).
*     20/01/87  DCS   IS conversion. Remove error check after GKCPLG
*                     (cannot generate error) and metafile last source
*                     case.
*     20/01/87  ARG   IS conversion. New error detected and error
*                     numbers changed.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation Identifier
*     INP   IPLI   Polyline Index
*     INP   ILNTY  Linetype
*     INP   RLNWD  Linewidth scale factor
*     INP   IPLCI  Polyline Colour Index
*
      INTEGER IWKID, IPLI, ILNTY, IPLCI
      REAL RLNWD
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYSL/    Last source flags
*     Modify /GKYWCA/   Send KWI1-3, QWR1, receive KERROR
*     Modify /GKYERR/   Set error status
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*       60  Polyline index is invalid
*       63  Linetype is equal to zero
*       65  Linewidth scale factor is less than zero
*       93  Colour index is invalid
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
*       64  Raised by workstation
*
*---------------------------------------------------------------------


*     Check GKS state

      CALL GKPRLG (ESPLR, GWSOP, GSGOP)

      IF (KERROR .EQ. 0) THEN

*        Check arguments

         IF (IPLI .LT. 1) THEN
            KERROR = 60
         ELSE IF (ILNTY .EQ. 0) THEN
            KERROR = 63
         ELSE IF (RLNWD .LT. 0) THEN
            KERROR = 65
         ELSE IF (IPLCI .LT. 0) THEN
            KERROR = 93
         END IF

      END IF

      IF (KERROR .EQ. 0) THEN

*        Set the representation on the workstation

         KWI1 = IPLI
         KWI2 = ILNTY
         QWR1 = RLNWD
         KWI3 = IPLCI
         CALL GKSONW(IWKID,KSPLR,1,KDAT,1,QDAT,QDAT,1,CH)
         IF ((KSPLWK .EQ. KGKSFN) .AND. (IPLI .EQ. KCPLI)) THEN
*           GKS
            CALL GKCPLG
            CALL GKSONW(IWKID,KSPLA,1,KDAT,1,QDAT,QDAT,1,CH)
         END IF
         IF (KRGN) CALL GKRGN
      END IF

      IF (KERROR .NE. 0) CALL GKERR(KERROR)

      END
