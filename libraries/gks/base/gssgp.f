C# IL>=a, OL>=1
      SUBROUTINE GSSGP(ISGNO,SGPRI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET SEGMENT PRIORITY
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level set segment priority.
*
*  MAINTENANCE LOG
*  ---------------
*     06/12/83  JGW   Original version stabilised
*     07/12/83  AS    Change KQSGS to KCSSG
*     16/12/83  JRG   Error 917->1017, check for segment open added
*     04/01/84  JRG   More error checking
*     20/03/84  JRG   Set segment priority! (i.e. set QWR1)
*     20/01/87  ARG   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP     ISGNO    segment name
*     INP     SGPRI    priority
*
      INTEGER ISGNO
      REAL SGPRI
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    RGN: regenerate
*     Modify /SL/     KSTRWK, KSFAWK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IARRAY Integer array for inquire segment directory
*     RARRAY Real array for inquire segment directory
*
      INTEGER IARRAY(KSGISZ)
      REAL    RARRAY(KSGRSZ)
*
*  ERRORS
*  ------
*       7    GKS state error
*     120    Invalid segment name
*     122    Segment does not exist
*     126    Segment priority outside [0-1]
*
*----------------------------------------------------------------------------

*     -- validate call & argument
      CALL GKPRLG(ESSGP,GWSOP,GSGOP)
      IF (KERROR.NE.0) GOTO 999

      IF (ISGNO.LT.1) THEN
         KERROR = 120
         GOTO 999
      ENDIF

      IF (KSGLST.NE.KNIL)
     :    CALL GKDRGE(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      IF (KERROR .EQ. -1017 .OR. KSGLST .EQ. KNIL) KERROR = 122
      IF( KERROR.NE.0 ) GOTO 999

      IF (SGPRI .LT. 0.0  .OR.  SGPRI .GT. 1.0) THEN
         KERROR = 126
         GOTO 999
      ENDIF

*     -- validation OK, update segment details then send w/s --

      RARRAY(KSGPRI) = SGPRI
      CALL GKDRPU(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      IF( KERROR.NE.0 ) GOTO 999
      KWI1 = ISGNO
      KWI2 = KSNOPN
      KWI3 = GNEMPT
      IF (KOPS.EQ.GSGOP .AND. ISGNO.EQ.KOPSG) THEN
         KWI2 = KSOPN
         CALL GKCSSG(KWI3)
      ENDIF
      KWI4 = IARRAY(KSGVIS)
      QWR1 = SGPRI
      CALL GKSOPS(KSSGP,1,KDAT,1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 999
      IF (KRGN) THEN
         CALL GKRGN
         IF (KERROR .NE. 0) GOTO 999
      ENDIF
      RETURN

  999 CONTINUE
      CALL GKERR(KERROR)
      END
