      SUBROUTINE HEAPSORT(NDATA, XDATA, KEY)
*
* Numerical recipes sorting routine
*
* Input:
*       NDATA = Number of data values
*       XDATA = Data values
* Output:
*       KEY   = Key to sorted data values (i.e. XDATA(KEY(I)) ascends)
*
      REAL  XDATA(NDATA)
      INTEGER KEY(NDATA)
*
* Initialise KEY
*
      DO I = 1, NDATA
        KEY(I) = I
      END DO
      IF(NDATA.EQ.1) RETURN
*
      L = NDATA/2+1
      IR = NDATA
10    CONTINUE
        IF(L.GT.1) THEN
          L = L - 1
          IK = KEY(L)
          X = XDATA(IK)
        ELSE
          IK = KEY(IR)
          X = XDATA(IK)
          KEY(IR) = KEY(1)
          IR = IR - 1
          IF(IR.EQ.1) THEN
            KEY(1) = IK
            RETURN
          END IF
        END IF
        I = L
        J = L + L
20      IF(J.LE.IR) THEN
          IF(J.LT.IR) THEN
            IF(XDATA(KEY(J)).LT.XDATA(KEY(J+1))) J = J + 1
          END IF
          IF(X.LT.XDATA(KEY(J))) THEN
            KEY(I) = KEY(J)
            I = J
            J = J + J
          ELSE
            J = IR + 1
          END IF
          GOTO 20
        END IF
        KEY(I) = IK
      GOTO 10
      END

      SUBROUTINE IHEAPSORT(NDATA, XDATA, KEY)
*
* Numerical recipes sorting routine
*
* Input:
*       NDATA = Number of data values
*       XDATA = Data values
* Output:
*       KEY   = Key to sorted data values (i.e. XDATA(KEY(I)) ascends)
*
      INTEGER XDATA(NDATA)
      INTEGER KEY(NDATA)
*
* Initialise KEY
*
      DO I = 1, NDATA
        KEY(I) = I
      END DO
      IF(NDATA.EQ.1) RETURN
*
      L = NDATA/2+1
      IR = NDATA
10    CONTINUE
        IF(L.GT.1) THEN
          L = L - 1
          IK = KEY(L)
          X = XDATA(IK)
        ELSE
          IK = KEY(IR)
          X = XDATA(IK)
          KEY(IR) = KEY(1)
          IR = IR - 1
          IF(IR.EQ.1) THEN
            KEY(1) = IK
            RETURN
          END IF
        END IF
        I = L
        J = L + L
20      IF(J.LE.IR) THEN
          IF(J.LT.IR) THEN
            IF(XDATA(KEY(J)).LT.XDATA(KEY(J+1))) J = J + 1
          END IF
          IF(X.LT.XDATA(KEY(J))) THEN
            KEY(I) = KEY(J)
            I = J
            J = J + J
          ELSE
            J = IR + 1
          END IF
          GOTO 20
        END IF
        KEY(I) = IK
      GOTO 10
      END

      SUBROUTINE DHEAPSORT(NDATA, XDATA, KEY)
*
* Numerical recipes sorting routine
*
* Input:
*      NDATA = Number of data values
*      XDATA = Data values
* Output:
*      KEY   = Key to sorted data values (i.e. XDATA(KEY(I)) ascends)
*
      IMPLICIT NONE
      INTEGER I, NDATA, L, IR, IK, J
      DOUBLE PRECISION XDATA(NDATA), X
      INTEGER KEY(NDATA)
*
* Initialise KEY
*
      DO I = 1, NDATA
        KEY(I) = I
      END DO
      IF(NDATA.EQ.1) RETURN
*
      L = NDATA/2+1
      IR = NDATA
10    CONTINUE
      IF(L.GT.1) THEN
        L = L - 1
        IK = KEY(L)
        X = XDATA(IK)
      ELSE
        IK = KEY(IR)
        X = XDATA(IK)
        KEY(IR) = KEY(1)
        IR = IR - 1
        IF(IR.EQ.1) THEN
          KEY(1) = IK
          RETURN
        END IF
      END IF
      I = L
      J = L + L
20    IF(J.LE.IR) THEN
        IF(J.LT.IR) THEN
          IF(XDATA(KEY(J)).LT.XDATA(KEY(J+1))) J = J + 1
        END IF
        IF(X.LT.XDATA(KEY(J))) THEN
          KEY(I) = KEY(J)
          I = J
          J = J + J
        ELSE
          J = IR + 1
        END IF
        GOTO 20
      END IF
      KEY(I) = IK
      GOTO 10
      END
