C# IL>=a, OL>=1
      SUBROUTINE GSSGT (ISGNO,TRAMAT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET SEGMENT TRANSFORMATION
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level set segment transformation.
*
*  MAINTENANCE LOG
*  ---------------
*     18/11/83  JGW   Original version stabilised
*     18/11/83  AS    Include CHECK.INC, other minor changes
*     22/11/83  AS    Include GKSE.PAR
*     16/12/83  JRG   Error 917->1017, check for segment open added
*       4/1/84  JRG   More error checking
*     21/01/87  ARG   IS conversion. Error number changed. Language
*                     binding has transposed the matrix.
*
*  ARGUMENTS
*  ---------
*     INP     ISGNO     segment name
*     INP     TRAMAT    new transformation matrix
*
      INTEGER ISGNO
      REAL    TRAMAT (2,3)
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
*     XFORM  Transposed transformation matrix
*
      INTEGER IARRAY(KSGISZ), I
      REAL    RARRAY(KSGRSZ)
      REAL    XFORM (6)
*
*  ERRORS
*  ------
*      7    GKS state error
*    120    Invalid segment name
*    122    Segment does not exist
*
*  ALGORITHM
*  ---------
*     The w/s recieves two set segment transformation requests.
*          Request 1 : (KWI5=1)
*                       The F/E is unchanged so an incapable w/s can
*                       obtain a replay (undraw) in the old transform
*                       position.
*          Update F/E
*          Request 2 : (KWI5=2)
*                       The w/s must now present the segment in the
*                       new transformed position. Any replay request
*                       (draw) will obtain the segment in the new
*                       transformed position.
*
*---------------------------------------------------------------------------

*     -- validate call & argument
      CALL GKPRLG(ESSGT,GWSOP,GSGOP)
      IF (KERROR.NE.0) GOTO 999
      IF (ISGNO.LT.1) THEN
         KERROR = 120
         GOTO 999
      ENDIF
      IF (KSGLST.NE.KNIL)
     :    CALL GKDRGE(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      IF (KERROR .EQ. -1017 .OR. KSGLST .EQ. KNIL) KERROR = 122
      IF( KERROR.NE.0 ) GOTO 999

*     -- validation OK, send to W/S first notification --
*     ===================================================

*     first transpose matrix
      XFORM (1) = TRAMAT (1,1)
      XFORM (2) = TRAMAT (1,2)
      XFORM (3) = TRAMAT (1,3)
      XFORM (4) = TRAMAT (2,1)
      XFORM (5) = TRAMAT (2,2)
      XFORM (6) = TRAMAT (2,3)

      KWI1 = ISGNO
      KWI2 = KSNOPN
      KWI3 = GNEMPT
      IF (KOPS.EQ.GSGOP .AND. ISGNO.EQ.KOPSG) THEN
         KWI2 = KSOPN
         CALL GKCSSG(KWI3)
      ENDIF
      KWI4 = IARRAY(KSGVIS)
      KWI5 = 1
      QWR1 = XFORM(1)
      QWR2 = XFORM(2)
      QWR3 = XFORM(3)
      QWR4 = XFORM(4)
      QWR5 = XFORM(5)
      QWR6 = XFORM(6)
      CALL GKSOPS(KSSGT,1,KDAT,1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 999

*     -- update F/E then send second request
      DO 10 I=0,5
         RARRAY(KSGTRN+I) = XFORM(I+1)
   10 CONTINUE
      CALL GKDRPU(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      IF( KERROR.NE.0 ) GOTO 999
      KWI5 = 2
      CALL GKSOPS(KSSGT,1,KDAT,1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 999
      IF (KRGN) THEN
         CALL GKRGN
         IF (KERROR .NE. 0) GOTO 999
      ENDIF
      RETURN

  999 CONTINUE
      CALL GKERR(KERROR)
      END
