*  History:
*     19 Nov 1993 (hme):
*        TABs removed.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE BASLIN (BUF, DATA, NBPTS, IBLRPT, BADVAL, IERR)

*  Routine to fit a linear baseline to a spectrum, using two baseline
*  regions to establish the gradient and intercept.

      IMPLICIT  NONE

*     Formal parameters

      REAL      BUF(*)
      REAL      DATA(*)
      INTEGER   NBPTS
      INTEGER   IBLRPT(4)
      REAL      BADVAL
      INTEGER   IERR

*     Local variables

      INTEGER   I
      INTEGER   L1, L2
      INTEGER   L3, L4
      INTEGER   NI

      REAL      DX,   DY
      REAL      GRAD
      REAL      YI
      REAL      XLOW, XHIGH
      REAL      YLOW, YHIGH

*  Ok, go....

      L1=IBLRPT(1)
      L2=IBLRPT(2)
      L3=IBLRPT(3)
      L4=IBLRPT(4)

      NI   = 0
      YLOW = 0.0
      DO I = L1, L2
        IF (BUF(I).NE.BADVAL) YLOW = YLOW + BUF(I)
        NI = NI + 1
      END DO

      IF (NI.NE.0) THEN
        YLOW = YLOW/FLOAT(NI)
        XLOW = FLOAT(L1) + 0.5*FLOAT(L2-L1)
      ELSE
        PRINT *, '-- basfit --   No good points in first interval'
        IERR = 9
        RETURN
      END IF

      NI    = 0
      YHIGH = 0.0
      DO I = L3, L4
        IF (BUF(I).NE.BADVAL) YHIGH = YHIGH + BUF(I)
        NI = NI + 1
      END DO

      IF (NI.NE.0) THEN
        YHIGH = YHIGH/FLOAT(NI)
        XHIGH = FLOAT(L3) + 0.5*FLOAT(L4-L3)
      ELSE
        PRINT *, '-- basfit --   No good points in second interval'
        IERR = 9
        RETURN
      END IF

      DX   = XHIGH - XLOW
      DY   = YHIGH - YLOW
      IF (DX.NE.0) THEN
        GRAD = DY/DX
      ELSE
        PRINT*,'Range zero - can''t do it'
        IERR = 9
        RETURN
      END IF

      DO I=1,NBPTS
        YI=GRAD*(I-XLOW)+YLOW
        IF (BUF(I).NE.BADVAL) THEN
          DATA(I) = BUF(I) - YI
        ELSE
          DATA(I) = BADVAL
        END IF
      END DO

      RETURN
      END


