      SUBROUTINE GERHND (IER, KFUNC, IERFL )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Error Logging
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Error Handler. Prints an error message and GKS function
*  Identification via the Starlink error reporting system

*
*  MAINTENANCE LOG
*  ---------------
*     22/10/82  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*                     (No change required)
*     21/07/83  CJW   Pass copy of IER to GKGEM incase its KERROR
*     26/07/83  CJW   Integer routine name
*     07/03/84  CJW   Check for invalid routine name
*     xx/02/85  SHS   Added temp storage for shifting part of CEMESS.
*     22/01/87  JCS   IS conversion. Error changes.
*     07/02/91  DLT   Use Starlink error system
*
*  ARGUMENTS
*  ---------
*     INP   IER    Error Number
*     INP   KFUNC  Ident of GKS procedure
*     INP   IERFL  GKS Error File
*
      INTEGER IER, IERFL, KFUNC
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKOPS/  KOPS
*
      INCLUDE '..//include/gks.par'
      INCLUDE '../include/gkops.cmn'
*
*  LOCALS
*  ------
*     IEMX    Maximum number of characters in an error message (P)
*     CEMESS  Hold the error message
*     ILCLER  Copy of IER
*     BNAME   Copy of name of routine
*     STATUS  EMS error status
*
      INTEGER IEMX, STATUS, ILCLER, INULL
      PARAMETER (IEMX = 152)
      CHARACTER * 7 AINVAL
      PARAMETER (AINVAL = 'Invalid')
      CHARACTER * (IEMX) CEMESS
      CHARACTER * 9 BNAME
      INCLUDE 'GKS_ERR'
      INCLUDE '../include/gksnam.par'
*
*
*  ALGORITHM
*  ---------
*
*  COMMENTS
*  --------
*                       SYSTEM DEPENDENT
*
*---------------------------------------------------------------------

*     Get Message
*     -----------

*     Save IER incase IER is infact KERROR (prevents GKGEM having
*     same variable as an argument and in common = side effects!)

      ILCLER = IER
      CALL GKGEM(ILCLER,CEMESS)
      INULL = INDEX(CEMESS,CHAR(0)) - 1
      IF (INULL.LT.0) THEN
         INULL = LEN(CEMESS)
      ELSE IF (INULL.EQ.0) THEN
         INULL = 1
      ENDIF

*     Get Name
*     --------

      IF ((KFUNC.LT.0) .OR. (KFUNC.GT.108)) THEN
         BNAME = AINVAL
      ELSE
         BNAME = 'G'//ANAMES(KFUNC)
      ENDIF

*     Set suitable status value

      STATUS = GKS__ERROR

*     Output Message
*     --------------

      CALL EMS_SETC( 'BNAME', BNAME//'[')
      CALL EMS_SETI( 'IER', IER)

      CALL EMS_REP( 'GKS_ERROR', '^BNAME^IER] '//CEMESS(:INULL), STATUS)

      END
