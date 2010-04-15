*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE YXN(ASIZE1,X,Y,NPOINT,MAXBRK,BREAKS,NBREAK,POWER)
*
*
       IMPLICIT NONE
*
*
       INTEGER ASIZE1, NPOINT, MAXBRK, NBREAK
       INTEGER IX
       LOGICAL OK
       INTEGER BREAKS(MAXBRK)
       INTEGER I, NTEST

       REAL X(ASIZE1), Y(ASIZE1), POWER, TSTVAL

       CHARACTER*10 TEXT
       INTEGER SLEN

       TSTVAL = -16.1054
       NTEST = 0
       DO 100 I = 1, NPOINT
          IF (Y(I).GT.0.0) THEN
             Y(I) = LOG10(Y(I)) + POWER*LOG10(X(I))
             IF (Y(I).GT.35.0) THEN
                Y(I) = TSTVAL
                NTEST = NTEST + 1
             ELSE
                Y(I) = 10.0**Y(I)
             ENDIF
          ELSE
             Y(I) = TSTVAL
             NTEST = NTEST + 1
          ENDIF
  100  CONTINUE

       IF (NTEST.GT.0) THEN
          WRITE (TEXT,'(I5)',IOSTAT=IX) NTEST
          CALL SSTRIP(TEXT)
          WRITE (*,'(''   YXN:  '',A,'' points lost'')')
     :    TEXT(1:SLEN(TEXT))
          CALL SRTBRK(ASIZE1,X,Y,NPOINT,MAXBRK,BREAKS,NBREAK,TSTVAL,OK)
          IF (.NOT.OK) THEN
             WRITE (*,'(''   YXN:  error resorting data'')')
             NPOINT = 0
          ENDIF
       ENDIF

       END
