*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE IHRSTR
*
*      RECLAIM STACK DATA WRITTEN USING 'SAVE'
*
*
*   IMPORTS:
*      MAXSTK   (INTEGER)  MAXIMUM SIZE OF STACK ARRAYS
*
*   MODIFIES:
*      NONSTK   (INTEGER)  NUMBER OF STACK ENTRIES
*      STKSZE   (INTEGER)  NUMBER OF POINTS IN STACK
*      BSTSZE   (INTEGER)  NUMBER OF BREAKS IN STACK
*      XSTACK   (REAL)     ARRAY OF X VALUES IN STACK
*      YSTACK   (REAL)     ARRAY OF Y VALUES IN STACK
*      BSTACK   (INTEGER)  ARRAY OF XSTACK INDEXES CORRESPONDING
*                          TO BREAKS
*      BSTNPT   (INTEGER)  NUMBER OF BREAKS PER STACK ENTRY
*      POINTR   (INTEGER)  ARRAY OF XSTACK INDEXES CORRESPONDING
*                          TO THE START OF EACH STACK ENTRY
*      STKNPT   (INTEGER)  ARRAY OF NUMBERS OF POINTS PER ENTRY
*      BPOINT   (INTEGER)  ARRAY OF BSTACK INDEXES CORRESPONDING
*                          TO THE FIRST BREAK PER STACK ENTRY
*      STITLE   (CHARACTER)ARRAY OF STACK ENTRY TITLES
*      WORVST   (REAL)     ARRAY OF WORV VALUES FOR STACK ENTRIES
*
*      STKLST   (INTEGER)  INDEX OF LAST XSTACK ENTRY
*      BSTLST   (INTEGER)  INDEX OF LAST BSTACK ENTRY
*
*      SUBCHK   (LOGICAL)  .TRUE. ON SUCCESSFUL COMPLETION
*
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE IHRSTR(sys,vmsrec,
     :                   NONSTK,STKSZE,BSTSZE,MAXSTK,XSTACK,YSTACK,
     :                   BSTACK,BSTNPT,POINTR,STKNPT,BPOINT,STITLE,
     :                   WORVST,STKLST,BSTLST,SUBCHK)
*
*   Declare argument list
*
       IMPLICIT NONE
*
       CHARACTER*32      sys
       INTEGER      vmsrec,vmssiz
       INTEGER NONSTK, STKSZE, BSTSZE, MAXSTK
       INTEGER BSTACK(BSTSZE), BSTNPT(MAXSTK)
       INTEGER POINTR(MAXSTK), STKNPT(MAXSTK)
       INTEGER BPOINT(MAXSTK)
       INTEGER STKLST, BSTLST
*
       REAL XSTACK(STKSZE), YSTACK(STKSZE)
       REAL WORVST(MAXSTK)
*
       LOGICAL SUBCHK
*
       CHARACTER*80 STITLE(MAXSTK)
*
*   Local variables
*
       INTEGER I, J
       INTEGER I1, I2, ie
       INTEGER BLST1, NSTORE
*
*
*
       BLST1 = STKLST
       SUBCHK = .TRUE.
       IF (NONSTK.EQ.MAXSTK) THEN
          WRITE (*,'('' RESTORE:  stacks already full'')')
          SUBCHK = .FALSE.
          GOTO 300
       ENDIF
*
*
*
       i1 = 0
       ie = 0
        READ (37,IOSTAT=I1) NSTORE
       IF (I1.NE.0) THEN
          WRITE (*,'('' RESTORE:   error on read'')')
          SUBCHK = .FALSE.
          GOTO 300
       ENDIF
*
*
*
       DO 100 I = 1, NSTORE
          NONSTK = NONSTK + 1
          IF (NONSTK.GT.MAXSTK) THEN
             NONSTK = NONSTK - 1
             WRITE (*,
     :       '('' RESTORE:  stack filled, only'',I3,
     :       '' entries acquired'')') I - 1
             SUBCHK = .FALSE.
             GOTO 300
          ENDIF
*
           READ (37,ERR=200) STITLE(NONSTK)
           READ (37,ERR=200) WORVST(NONSTK)
           READ (37,ERR=200) STKNPT(NONSTK)
           READ (37,ERR=200) BSTNPT(NONSTK)
*
          I1 = STKLST + STKNPT(NONSTK)
          I2 = BSTLST + BSTNPT(NONSTK)
          IF (I1.GT.STKSZE .OR. I2.GT.BSTSZE) THEN
             WRITE (*,
     :       '('' RESTORE:  arrays would overflow;'',
     :       '' only'',I3,'' entries acquired'')') I - 1
             NONSTK = NONSTK - 1
             SUBCHK = .FALSE.
             GOTO 300
          ENDIF
*
          I1 = STKLST + 1
          I2 = I1 + STKNPT(NONSTK) - 1
          POINTR(NONSTK) = I1
          STKLST = I2
           READ (37,ERR=200) (XSTACK(J),J=I1,I2)
           READ (37,ERR=200) (YSTACK(J),J=I1,I2)
*
*
*
          I1 = BSTLST + 1
          I2 = I1 + BSTNPT(NONSTK) - 1
          BPOINT(NONSTK) = I1
          BSTLST = I2
           READ (37,ERR=200) (BSTACK(J),J=I1,I2)
  100  CONTINUE
*
*
*
       GOTO 300
*
*
*
  200  CONTINUE
       WRITE (*,
     : '('' RESTORE:  error during read; only'',I3,
     : '' entries acquired'')') I - 1
       CLOSE (37)
       SUBCHK = .FALSE.

  300  CONTINUE

       END
