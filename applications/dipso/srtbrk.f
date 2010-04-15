*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE SRTBRK
*
*   Reorders datasets and determines appropriate break points.
*   'Null' data are flagged with the value TSTVAL
*
*   IMPORTS:
*       ASIZE     (INTEGER) SIZE OF X WAVE & FLUX Y ARRAYS
*       XVAL     (REAL)    ARRAY OF X VALUES
*       YVAL     (REAL)    ARRAY OF Y VALUES
*       NPOINT    (INTEGER) NO. OF X, Y PAIRS
*       MAXBRK    (INTEGER) SIZE OF BREAK ARRAY
*       BREAK    (INTEGER) ARRAY OF INPUT BREAK POINTS
*       NBREAK    (INTEGER) NO. OF BREAK POINTS
*       TSTVAL    (REAL)    Value of 'null data' Flag
*       OK        (LOGICAL) TRUE on success
*
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE SRTBRK(ASIZE1,XVAL,YVAL,NPOINT,MAXBRK,BREAK,NBREAK,
     :                   TSTVAL,OK)
*
*
*
       IMPLICIT NONE
*
*
*
       INTEGER ASIZE1, NPOINT, MAXBRK, NBREAK
       INTEGER BREAK(MAXBRK)

       REAL XVAL(ASIZE1), YVAL(ASIZE1)
       REAL TSTVAL

       LOGICAL OK
*
*
*
       INTEGER I, J, K, L, M, N
       INTEGER ITAG, NPT
       OK = .TRUE.

*   Mark pre-existing breaks with X=TSTVAL

       DO 200 I = 1, NBREAK - 1
          IF (NPOINT.EQ.ASIZE1) THEN
             OK = .FALSE.
             GOTO 400
          ENDIF

          NPOINT = NPOINT + 1
          DO 50 J = NPOINT, BREAK(I) + 2, -1
             XVAL(J) = XVAL(J-1)
             YVAL(J) = YVAL(J-1)
   50     CONTINUE
          YVAL(BREAK(I)+1) = TSTVAL

          DO 100 J = 1, NBREAK - 1
             BREAK(J) = BREAK(J) + 1
  100     CONTINUE
  200  CONTINUE

       IF (NPOINT.EQ.ASIZE1) THEN
          OK = .FALSE.
          GOTO 400
       ELSE
          NPOINT = NPOINT + 1
          YVAL(NPOINT) = TSTVAL
       ENDIF

*   CALCULATE NEW BREAK ARRAY

       ITAG = 0
       NPT = 0
       NBREAK = 0

       DO 300 I = 1, NPOINT
          IF (YVAL(I).NE.TSTVAL) THEN
             NPT = NPT + 1
             XVAL(NPT) = XVAL(I)
             YVAL(NPT) = YVAL(I)
             ITAG = 1
          ELSEIF (ITAG.NE.0) THEN
             IF (NBREAK.EQ.MAXBRK) THEN
                OK = .FALSE.
                GOTO 400
             ENDIF
             NBREAK = NBREAK + 1
             BREAK(NBREAK) = NPT
             ITAG = 0
          ENDIF
  300  CONTINUE

       NPOINT = NPT
       IF (NPOINT.EQ.0) THEN
          NBREAK = 1
          BREAK(NBREAK) = 0
       ENDIF

  400  CONTINUE

       END
