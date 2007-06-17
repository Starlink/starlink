       SUBROUTINE REMAP(XIN,YIN,NPTIN,XOUT,YOUT,NPTOUT,MODE)
*
*   Subroutine to remap data
*   Requires XIN    REAL array giving x-values for input data
*   YIN    REAL array of input y-values
*   NPTIN  INTEGER number of points in XIN,YIN
*   (NOT the size of the arrays)
*   XOUT   REAL array of x-values onto which the input is to be mapped
*   NPTOUT INTEGER number of points in XOUT
*   (NOT the size of the array)
*   MODE   INTEGER indicating how the remapping is to take place
*   outside the range of XIN.  If MODE is 0 then YOUT takes the
*   value 0 for XOUT values out of the range of XIN.  If MODE
*   is 1 it takes the value of YOUT(1) for XOUT values below
*   the range of XIN, YOUT(NPTIN) for values above the range
*   of XIN.
*   Returns  YOUT   REAL array of remapped y-values
*
*   For XOUT(I) < XIN(1) gives YOUT(I)=YIN(1)*MODE
*   For XIN(1) < XOUT(I) < XIN(NPTIN) interpolates between the nearest two
*   (xin,yin) pairs
*   For XOUT(I) > XIN(NPTIN) gives YOUT(I)=YIN(NPTIN)*MODE
*   Note that if XOUT is very much coarser than XIN then some (XIN,YIN) pairs
*   will not be used, thus effectively throwing data away.
*
       REAL XIN(1), YIN(1), XOUT(1), YOUT(1)
       INTEGER NPTIN, NPTOUT, MODE
       INTEGER IIN, IOUT
       REAL FACTOR
       FACTOR = MODE
       IOUT = 1
*
*   fixup for arrays changing in different senses
*
       IF (XOUT(1).LT.XOUT(NPTOUT)) THEN
          IF (XIN(1).GT.XIN(NPTIN)) THEN
             NDO = (NPTIN+1)/2
             J = 0
             DO 20 I = 1, NDO
                TX = XIN(I)
                TY = YIN(I)
                XIN(I) = XIN(NPTIN-J)
                YIN(I) = YIN(NPTIN-J)
                XIN(NPTIN-J) = TX
                YIN(NPTIN-J) = TY
                J = J + 1
   20        CONTINUE
          ENDIF
       ENDIF
*
*   fixup to deal with non-increasing values
*
       IF (XOUT(1).LE.XOUT(NPTOUT)) THEN
*
*   Deal with XOUT values below XIN(1)
*
   50     CONTINUE
          IF (XOUT(IOUT).LT.XIN(1) .AND. IOUT.LE.NPTOUT) THEN
             YOUT(IOUT) = YIN(1)*FACTOR
             IOUT = IOUT + 1
             GOTO 50
          ENDIF
          IIN = 2
*
*   Deal with points requiring interpolation
*
  100     CONTINUE
          IF (IOUT.LE.NPTOUT .AND. XOUT(IOUT).LE.XIN(NPTIN)) THEN
             IF (XOUT(IOUT).EQ.XIN(IIN)) THEN
                YOUT(IOUT) = YIN(IIN)
             ELSE
  110           CONTINUE
                IF (XIN(IIN).LT.XOUT(IOUT)) THEN
                   IIN = IIN + 1
                   GOTO 110
                ENDIF
                YOUT(IOUT) =
     :          (YIN(IIN)*(XOUT(IOUT)-XIN(IIN-1))+YIN(IIN-1)
     :          *(XIN(IIN)-XOUT(IOUT)))/(XIN(IIN)-XIN(IIN-1))
             ENDIF
             IOUT = IOUT + 1
             GOTO 100
          ENDIF
*
*   Deal with points with XOUT(I) greater than XIN(NPTIN)
*
  150     CONTINUE
          IF (IOUT.LE.NPTOUT) THEN
             YOUT(IOUT) = YIN(NPTIN)*FACTOR
             IOUT = IOUT + 1
             GOTO 150
          ENDIF
          GOTO 500
       ENDIF
*
*   duplicate code for decreasing X array
*
  200  CONTINUE
       IF (XOUT(IOUT).GT.XIN(1)) THEN
          IF (IOUT.LE.NPTOUT) THEN
             YOUT(IOUT) = YIN(1)*FACTOR
             IOUT = IOUT + 1
             GOTO 200
          ENDIF
       ENDIF
       IIN = 2
*
  300  CONTINUE
       IF (IOUT.LE.NPTOUT) THEN
          IF (XOUT(IOUT).GT.XIN(NPTIN)) THEN
             IF (XOUT(IOUT).EQ.XIN(IIN)) THEN
                YOUT(IOUT) = YIN(IIN)
             ELSE
  310           CONTINUE
                IF (XIN(IIN).GT.XOUT(IOUT)) THEN
                   IIN = IIN + 1
                   GOTO 310
                ENDIF
                YOUT(IOUT) = (YIN(IIN)*(XOUT(IOUT)-XIN(IIN-1))+YIN(IIN-1
     :                       )*(XIN(IIN)-XOUT(IOUT)))
     :                       /(XIN(IIN)-XIN(IIN-1))
             ENDIF
             IOUT = IOUT + 1
             GOTO 300
          ENDIF
       ENDIF
*
  400  CONTINUE
       IF (IOUT.LE.NPTOUT) THEN
          YOUT(IOUT) = YIN(NPTIN)*FACTOR
          IOUT = IOUT + 1
          GOTO 400
       ENDIF

  500  CONTINUE

       END
