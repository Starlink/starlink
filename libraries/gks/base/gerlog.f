C# IL>=a, OL>=0
      SUBROUTINE GERLOG (IER, KFUNC, IERFL )
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
*  Error Logger. Prints an error message and GKS function
*  Identification on the error file.

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
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkops.cmn'
*
*  LOCALS
*  ------
*     IEMX    Maximum number of characters in an error message (P)
*     IEMXLN  Maximum number of characters of error messageto output on
*             a line (P)
*     CSPACE  Character Space (P)
*     JPOS    Position of a blank in the message
*     JSTART  Start position of second half of message
*     CEMESS  Hold the error message
*     CETEMP  Temporary string used in shifting of CEMESS
*     ILCLER  Copy of IER
*     BNAME   Copy of name of routine
*
      INTEGER     IEMX,       IEMXLN
      PARAMETER ( IEMX = 255, IEMXLN = 58)
      CHARACTER * 7 AINVAL
      PARAMETER (AINVAL = 'Invalid')
      CHARACTER * 1 CSPACE
      PARAMETER ( CSPACE = ' ' )
      INTEGER JPOS, JSTART, ILCLER
      CHARACTER * (IEMX) CEMESS
      CHARACTER * (IEMX) CETEMP
      CHARACTER * 9 BNAME
      INCLUDE '../include/gksnam.par'
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     IERFL    user defnd   Error stream
*       *      System defnd Error stream if GKS is Closed
*
*  ALGORITHM
*  ---------
*  The message (CEMESS) is cut into chunks so that the full error
*  message will fit onto a 80 character line. The message is searched
*  backwards, starting at the required cut position, until a blank is
*  found. A cut is made at this point. Thus words are not split.
*
*  COMMENTS
*  --------
*  The default output stream ("*") is used if GKS is closed. This might
*  not be a good idea on some systems - hence this routine is slightly
*
*                       SYSTEM DEPENDENT
*
*---------------------------------------------------------------------


*     Get Message
*     -----------

*     Save IER incase IER is infact KERROR (prevents GKGEM having
*     same variable as an argument and in common = side effects!)

      ILCLER = IER
      CALL GKGEM(ILCLER,CEMESS)

*     Get Name
*     --------

      IF ((KFUNC.LT.0) .OR. (KFUNC.GT.108)) THEN
         BNAME = AINVAL
      ELSE
         BNAME = 'G'//ANAMES(KFUNC)
      ENDIF

*     Output Message
*     --------------

      DO 1 JPOS = IEMXLN, 1, -1
         IF (CEMESS(JPOS:JPOS) .EQ. CSPACE) GO TO 2
    1 CONTINUE

      JPOS = IEMXLN
    2 CONTINUE

      DO 3 JSTART = JPOS+1, IEMX
         IF (CEMESS(JSTART:JSTART) .NE. CSPACE) GO TO 4
    3 CONTINUE

      JSTART = IEMX+1
    4 CONTINUE

      IF (KOPS .EQ. GGKCL) THEN
         WRITE(*,1000) BNAME, IER, CEMESS(1:JPOS)
      ELSE
         WRITE(IERFL,1000) BNAME, IER, CEMESS(1:JPOS)
      ENDIF

      JPOS = MIN(IEMX, JSTART+IEMXLN-1)
      IF (JSTART .LE. JPOS) THEN
         CETEMP(JSTART:) = CEMESS(JSTART:)
         CEMESS = CETEMP(JSTART:)
         DO 5 JPOS = IEMXLN, 1, -1
            IF (CEMESS(JPOS:JPOS) .EQ. CSPACE) GO TO 6
    5    CONTINUE

         JPOS = IEMXLN
    6    CONTINUE

         DO 7 JSTART = JPOS+1, IEMX
            IF (CEMESS(JSTART:JSTART) .NE. CSPACE) GO TO 8
    7    CONTINUE

         JSTART = IEMX+1
    8    CONTINUE
         IF (KOPS .EQ. GGKCL) THEN
            WRITE(*,1001) CEMESS(1:JPOS)
            JPOS = MIN(IEMX, JSTART+IEMXLN-1)
            IF (JSTART .LE. JPOS) WRITE(*,1001) CEMESS(JSTART:JPOS)
         ELSE
            WRITE(IERFL,1001) CEMESS(1:JPOS)
            JPOS = MIN(IEMX, JSTART+IEMXLN-1)
            IF (JSTART .LE. JPOS) WRITE(IERFL,1001) CEMESS(JSTART:JPOS)
         ENDIF
      END IF

*  -------
*  FORMATS
*  -------

 1000 FORMAT(1X,A,T12,' [',I5,'] ',A)
 1001 FORMAT(     T12,'  ',5X,'  ',A)

      END
