*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE UPUSH
*
*   Pushes x,y datasets onto the DIPSO 'stack'
*
*   IMPORTS:
*     ASIZE         (INTEGER)    SIZE OF X,Y ARRAYS
*     NPTS          (INTEGER)    NO. OF POINTS IN X,Y ARRAYS
*     BSIZE         (INTEGER)    SIZE OF 'BREAK' ARRAY
*     NBRKS         (INTEGER)    NO. OF POINTS IN 'BREAK' ARRAY
*     XVALS(ASIZE)  (REAL)       ARRAY OF X VALUES
*     YVALS(ASIZE)  (REAL)       ARRAY OF Y VALUES
*     IBRKS(BSIZE)  (INTEGER)    ARRAY OF BREAK INDEXES
*     TITLE         (CHARACTER)  TITLE ASSOCIATED WITH DATASET
*     WORV          (REAL)       Wavelength (WORV=1) OR
*                   Velocity (WORV = Wav0/c)
*
*   EXPORTS:
*     OK            (LOGICAL)    TRUE ON SUCCESSFUL COMPLETION
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE UPUSH(ASIZE,XVALS,YVALS,NPTS,BSIZE,IBRKS,NBRKS,TITLE,
     :                  WORV,OK)
*
*   Declarations
*
       IMPLICIT NONE
*
*   Imports
*
       INTEGER ASIZE, BSIZE
       INTEGER NPTS, NBRKS

       INTEGER IBRKS(BSIZE)

       REAL XVALS(ASIZE), YVALS(ASIZE)

       CHARACTER*(*) TITLE
       REAL WORV

       LOGICAL OK
*
*   Common area
*
       INCLUDE 'DECLARE_STKS'

       LOGICAL PUSHW
       COMMON /PUSHW / PUSHW

*
*   Local variables
*
       INTEGER I, I1
*
*
*   Test for legitimate imports
*
*
       OK = .TRUE.
*
       IF (NBRKS.LT.1 .OR. NBRKS.GT.NPTS .OR. NBRKS.GT.BSIZE) THEN
          OK = .FALSE.
          WRITE (*,'(''   PUSH:  unacceptable NBRKS'')')
          WRITE (*,'(''          NBRKS, NPTS, BSIZE:'',3I7)') NBRKS,
     :           NPTS, BSIZE
          GOTO 300
       ENDIF

       IF (IBRKS(NBRKS).NE.NPTS) THEN
          OK = .FALSE.
          WRITE (*,'(''   PUSH:  breaks/data mismatch'')')
          WRITE (*,'(''          number of breaks:'',I4)') NBRKS
          WRITE (*,'(''          index of last break:'',I5)')
     :            IBRKS(NBRKS)
          WRITE (*,'(''          number of points:'',I5)') NPTS
          GOTO 300
       ENDIF

       IF (NPTS.GT.ASIZE) THEN
          OK = .FALSE.
          WRITE (*,'(''   PUSH:  arrays/data mismatch'')')
          WRITE (*,'(''          NPTS, ASIZE:'',2I7)') NPTS, ASIZE
          GOTO 300
       ENDIF

       IF (NPTS.LE.0) THEN
          OK = .FALSE.
          WRITE (*,'(''   PUSH:  no data to push!'')')
          GOTO 300
       ENDIF

*

       IF (NONSTK.EQ.MAXSTK) THEN
          OK = .FALSE.
          WRITE (*,'(''   PUSH:  stack arrays already full'')')
          GOTO 300
       ENDIF

       IF ((STKLST+NPTS).GT.STKSZE) THEN
          OK = .FALSE.
          WRITE (*,'(''   PUSH:  data arrays would overflow'')')
          GOTO 300
       ENDIF

       IF ((BSTLST+NBRKS).GT.BSTSZE) THEN
          OK = .FALSE.
          WRITE (*,'(''   PUSH:  break arrays would overflow'')')
          GOTO 300
       ENDIF
*
*
*   Load stack
*
*
       NONSTK = NONSTK + 1
       STKNPT(NONSTK) = NPTS
       BSTNPT(NONSTK) = NBRKS
       STITLE(NONSTK) = ' '
       STITLE(NONSTK) = TITLE(1:MIN(80,LEN(TITLE)))
       WORVST(NONSTK) = WORV
       IF (WORVST(NONSTK).EQ.0.0) WORVST(NONSTK) = 1.0

       I1 = STKLST
       POINTR(NONSTK) = I1 + 1
       DO 100 I = 1, NPTS
          XSTACK(I1+I) = XVALS(I)
          YSTACK(I1+I) = YVALS(I)
  100  CONTINUE
       STKLST = STKLST + NPTS

       I1 = BSTLST
       BPOINT(NONSTK) = I1 + 1
       DO 200 I = 1, NBRKS
          BSTACK(I1+I) = IBRKS(I)
  200  CONTINUE
       BSTLST = BSTLST + NBRKS

       IF (PUSHW) WRITE (*,'(''   PUSH:  filling entry'',I3)') NONSTK

  300  CONTINUE

       END
