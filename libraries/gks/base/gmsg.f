C# IL>=a, OL>=0
      SUBROUTINE GMSG(IWKID,STRING)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  MESSAGE
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To send message to workstation
*
*  MAINTENANCE LOG
*  ---------------
*      2/11/83  JRG  Created
*     30/11/83  AS   Change local character variable starting with C
*     20/01/87  PKY   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation identifier
*     INP   STRING String to be displayed
*
      INTEGER IWKID
      CHARACTER*(*) STRING
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /STK/    Stack space for converted chars
*     Read   /WCA/    Use dummy values
*     Modify /ERR/    KERROR
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     NOCHS  Number of chars in string
*     ISET   Allocated stack offset
*
      INTEGER NOCHS,ISET
*
*  STACK USAGE
*  -----------
*     NOCHS    INTEGER  used to expand text to 1 char/INTEGER
*
*  ERRORS
*  ------
*     7,20,25,36
*     300 Unable to allocate stack for converted chars
*
*---------------------------------------------------------------------

*   GKS Prologue
      CALL GKPRLG(EMSG,GWSOP,GSGOP)
      IF( KERROR.NE.0 ) GOTO 990

*   Get length of string and allocate space on stack for conversion
*   to internal ascii representation
      NOCHS=LEN(STRING)
      CALL GKSTAL(KINTGS,NOCHS,ISET)
      IF (KERROR.NE.0) GOTO 990

*   If an error is detected while some stack is allocated, then
*   stack must be deallocated.
*   If room then convert. Then send message to workstation.
      CALL GKNTOA(NOCHS,STRING,KSTACK(ISET))
      IF (KERROR .NE. 0) GOTO 980
      CALL GKSONW(IWKID,KMSG, NOCHS,KSTACK(ISET), 1,QDAT,QDAT, 1,CH)
      IF( KERROR.NE.0 ) GOTO 980

*   Report any error.
*   Then deallocate stack space.
*   Possibility of two errors being reported.
  980 IF( KERROR.NE.0 ) CALL GKERR(KERROR)
      KERROR=0
      CALL GKSTDA(KINTGS,ISET)

  990 CONTINUE
      IF (KERROR .NE. 0) CALL GKERR (KERROR)

      END
