C# IL>=a, OL>=0
      SUBROUTINE GKBUG (IER, CFUNC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONT END
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     BUG error handler. Should never be called if GKS is working
*     correctly.
*
*  MAINTENANCE LOG
*  ---------------
*     20/12/82  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     28/07/83  CJW   No longer uses error logger - which now uses integer
*                     names
*     22/04/86  RMK   Added temporary variable for use in shifting
*                     characters in CEMESS. Removed unused local variable.
*                     Removed temporary code at end which forced a crash to
*                     produce a system traceback.
*     22/01/87  JCS   IS conversion. Error number changes.
*     19/08/87  PJWR  Corrected error message format to match that used by
*                     GERLOG.
*
*  ARGUMENTS
*  ---------
*     INP   IER    Error Number
*     INP   CFUNC  Ident of GKS procedure where error detected.
*
      INTEGER IER
      CHARACTER * (*) CFUNC
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKERR/    KERRFL,CRTNM
*     Modify /GKYERR/   Set error status
*     Read   /GKOPS/    KOPS
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkops.cmn'
*
*  LOCALS
*  ------
*     IEMX    Maximum number of characters in an error message (P)
*     IEMXLN  Maximum number of characters to output on a line (P)
*     CSPACE  Character Space (P)
*     JPOS    Position of a blank in the message
*     JSTART  Start position of second half of message
*     CEMESS  Hold the error message
*     TSTR    Temporary string used when shifting characters
*
      INTEGER     IEMX,       IEMXLN
      PARAMETER (IEMX = 255, IEMXLN = 60)
      CHARACTER * 1 CSPACE
      PARAMETER ( CSPACE = ' ' )
      INTEGER JPOS, JSTART
      CHARACTER * (IEMX) CEMESS, TSTR
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     KERRFL    user defnd   Error stream
*       *      System defnd Error stream if GKS is Closed
*
*  ALGORITHM
*  ---------
*  The message (CEMESS) is cut into chunks so that the full error message will
*  fit onto a 80 character line. The message is searched backwards, starting
*  at the required cut position, until a blank is found. A cut is made at this
*  point. Thus words are not split.
*
*  ERRORS
*  ------
*    -1016 Internal error detected within GKS
*
*  COMMENTS
*  --------
*  The default output stream ("*") is used if GKS is closed. This might not be
*  a good idea on some systems - hence this routine is slightly
*
*                       SYSTEM DEPENDENT
*
*---------------------------------------------------------------------

*     Report Bug

      CALL GKGEM(IER,CEMESS)

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
         WRITE(*,1000) CFUNC, IER, CEMESS(1:JPOS)
      ELSE
         WRITE(KERRFL,1000) CFUNC, IER, CEMESS(1:JPOS)
      ENDIF

      JPOS = MIN(IEMX, JSTART+IEMXLN-1)
      IF (JSTART .LE. JPOS) THEN
         TSTR = CEMESS(JSTART:)
         CEMESS = TSTR
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
            WRITE(KERRFL,1001) CEMESS(1:JPOS)
            JPOS = MIN(IEMX, JSTART+IEMXLN-1)
            IF (JSTART .LE. JPOS) WRITE(KERRFL,1001) CEMESS(JSTART:JPOS)
         ENDIF
      END IF

      KERROR = -1016

*  -------
*  FORMATS
*  -------

 1000 FORMAT (1X,A,T12,' [',I5,'] ',A)
 1001 FORMAT (     T12,'  ',5X,'  ',A)

      END
