*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE IHSVAL
*
*      SAVE STACK DATA FOR RE-ACQUISITION USING 'RESTORE'
*
*
*   IMPORTS:
*      NONSTK   (INTEGER)  NUMBER OF STACK ENTRIES
*      STKSZE   (INTEGER)  MAXIMUM SIZE OF STACK ARRAYS
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
*
*   MODIFIES:
*      SUBCHK   (LOGICAL)  .TRUE. ON SUCCESSFUL COMPLETION
*
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE IHSVAL(SUBCHK)
*
*   Declare argument list
*
       IMPLICIT NONE
*
       INCLUDE 'declare_stks.f'
*
       INTEGER ISV1, ISV2
       COMMON /MINISV/ ISV1, ISV2
*
       LOGICAL SUBCHK
*
*   Local variables
*
       INTEGER I, J, I1, I2
*
*
*
       SUBCHK = .TRUE.
*
*
*
       WRITE (37,ERR=200) (ISV2-ISV1+1)
*
       DO 100 I = ISV1, ISV2
          WRITE (37,ERR=200) STITLE(I)
          WRITE (37,ERR=200) WORVST(I)
          WRITE (37,ERR=200) STKNPT(I)
          WRITE (37,ERR=200) BSTNPT(I)
*
          I1 = POINTR(I)
          I2 = I1 + STKNPT(I) - 1
          WRITE (37,ERR=200) (XSTACK(J),J=I1,I2)
          WRITE (37,ERR=200) (YSTACK(J),J=I1,I2)
*
          I1 = BPOINT(I)
          I2 = I1 + BSTNPT(I) - 1
          WRITE (37,ERR=200) (BSTACK(J),J=I1,I2)
  100  CONTINUE
*
*
*
       GOTO 300
*
*
*
  200  CONTINUE
       WRITE (*,'('' SAVE:  error during write'')')
       SUBCHK = .FALSE.

  300  CONTINUE

       END
