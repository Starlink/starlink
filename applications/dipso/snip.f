*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE SNIP(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAKS,NBREAK,
     :                 PARAMS,SUBCHK)
*
*
*
       IMPLICIT NONE
*
*
*
       INTEGER ASIZE1, NPOINT, MAXBRK, NBREAK
       INTEGER BREAKS(NBREAK)

       REAL WAVE(ASIZE1), FLUX(ASIZE1)

       CHARACTER*(*) PARAMS

       LOGICAL SUBCHK
       LOGICAL CSNIP
       LOGICAL OK
       LOGICAL FIRSTCURSOR
*
*
       INTEGER I, J, K, L, M, N, NSNIP

       REAL X1, X2, Y1, Y2
       REAL XVS(2)
       REAL TSTVAL
*
*
*
       SUBCHK = .TRUE.
       OK = .TRUE.
       NSNIP = 0
       FIRSTCURSOR = .TRUE.
       TSTVAL = 161.054

       CALL SSTRIP(PARAMS)
       CSNIP = .TRUE.
       IF (PARAMS.NE.' ') CSNIP = .FALSE.

  100  CONTINUE
       IF (CSNIP) THEN
          CALL SGSCURSE(N,X1,Y1,FIRSTCURSOR)
          FIRSTCURSOR = .FALSE.
          CALL SGSCURSE(N,X2,Y2,FIRSTCURSOR)
       ELSE
          X1 = 10.
          X2 = 5.
          CALL SSTRIP(PARAMS)
          IF (PARAMS.NE.' ') THEN
             CALL DECODE('SNIP',PARAMS,2,2,XVS,'X1 X2 ',OK)
             IF (.NOT.OK) THEN
                WRITE (*,'(''   SNIP:  error decoding parameters'')')
                X1 = 10.
                X2 = 5.
             ELSE
                X1 = XVS(1)
                X2 = XVS(2)
             ENDIF
          ENDIF
       ENDIF

       IF (X2.GT.X1) THEN
          WRITE (*,'(''   SNIP:  X1, X2 ='',1P2E12.4)') X1, X2
          M = 0
          DO 150 I = 1, NPOINT
             IF (WAVE(I).GE.X1) THEN
                DO 110 J = I, NPOINT
                   IF (WAVE(J).GT.X2) GOTO 100
                   FLUX(J) = TSTVAL
                   NSNIP = NSNIP + 1
                   M = M + 1
  110           CONTINUE
                GOTO 100
             ENDIF
  150     CONTINUE
       ELSEIF (NSNIP.NE.0) THEN
          CALL SRTBRK
     :    (ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAKS,NBREAK,TSTVAL,SUBCHK)
          IF (.NOT.SUBCHK) THEN
             WRITE (*,'(''   SNIP:  error resorting arrays'')')
             GOTO 200
          ENDIF
       ENDIF

       IF (.NOT.OK) SUBCHK = .FALSE.

  200  CONTINUE

       END
