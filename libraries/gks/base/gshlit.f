C# IL>=a, OL>=1
      SUBROUTINE GSHLIT(ISGNO,ISGHLT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET HIGHLIGHTING
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level set segment highlighting.
*
*  MAINTENANCE LOG
*  ---------------
*     18/11/83  JGW   Original version stabilised
*     18/11/83  AS    Include CHECK.INC, other minor changes
*     16/12/83  JRG   Error 917->1017, check for segment open added
*     03/01/84  JRG   Set current highlighting in W.C.A. only if this
*                     segment is open;  more comprehensive error checking
*     20/01/87  ARG   IS conversion. Error numbers changed. Added check
*                     on highlighting value.
*
*  ARGUMENTS
*  ---------
*     INP     ISGNO    segment name
*     INP     ISGHLT   highlighting { highlighted,normal }
*
      INTEGER ISGNO, ISGHLT
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    RGN: regenerate
*     Modify /SL/     KSTRWK, KSFAWK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
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
*      7    GKS state error
*    120    Invalid segment name
*    122    Segment does not exist
*   2000    Enumeration type out of range
*
*----------------------------------------------------------------------------

*     -- validate call & argument
      CALL GKPRLG(ESHLIT,GWSOP,GSGOP)
      IF (KERROR.NE.0) GOTO 999

      IF (ISGNO.LT.1) THEN
         KERROR = 120
         GOTO 999
      ENDIF
      IF (ISGHLT .NE. GNORML .AND. ISGHLT .NE. GHILIT) THEN
         KERROR = 2000
         GOTO 999
      ENDIF

      IF (KSGLST.NE.KNIL)
     :    CALL GKDRGE(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)

      IF (KERROR .EQ. -1017 .OR. KSGLST .EQ. KNIL) KERROR = 122
      IF( KERROR.NE.0 ) GOTO 999

*     -- validation OK, update segment details then send w/s --
*     =========================================================
      IARRAY(KSGHLT) = ISGHLT
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
      KWI5 = ISGHLT
      CALL GKSOPS(KSHLIT,1,KDAT,1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 999

      IF( KOPS.EQ.GSGOP .AND. ISGNO.EQ.KOPSG ) KCHLT = ISGHLT
      IF (KRGN) THEN
         CALL GKRGN
         IF (KERROR .NE. 0) GOTO 999
      ENDIF
      RETURN

  999 CONTINUE
      CALL GKERR(KERROR)
      END
