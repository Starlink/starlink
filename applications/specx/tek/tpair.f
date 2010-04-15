* History:
*    21 Sep 2000 (ajc):
*       Unused XLIMS
C--------------------------------------------------------------------------

      INTEGER FUNCTION TPAIR (XSCALE, DATA, NCH, OFFER, P, Q)

      INCLUDE   'FLAGCOMM'
      INCLUDE   'PLOTPAR1'

      LOGICAL   REPEAT
      LOGICAL   OFFER
      LOGICAL   FIRST_BOX
      DATA      FIRST_BOX /.TRUE./
      REAL*4    XSCALE(*), DATA(*)
      REAL*4    P(2), PNEW(2), Q(2), QNEW(2)
      REAL*4    YLIMS(2)
      CHARACTER VALOPT*28

      VALOPT='^M L R T B N A E Q C D H ? S'

      REPEAT = .TRUE.
      QNEW(1) = YST1
      QNEW(2) = YEND1

      DO WHILE (REPEAT)

*     Generate some Y-values for the boxes that show where the
*     baseline regions are:

        IF (QNEW(1)*QNEW(2) .LE. 0.0) THEN
          YLIMS(1) = + 0.1*(QNEW(1)-QNEW(2))
          YLIMS(2) = - 0.1*(QNEW(1)-QNEW(2))
        ELSE
          YLIMS(1) = QNEW(1) + 0.2*(QNEW(2)-QNEW(1))
          YLIMS(2) = QNEW(1) + 0.8*(QNEW(2)-QNEW(1))
        END IF

*    Plot the default box, in dashed lines, and fetch new values

        IF (OFFER) THEN
          CALL DRAW_DEFBOX (P, YLIMS, 2)
        END IF

        ISTAT = IVTOPT (IDEV, VALOPT, PNEW, QNEW, REPEAT)

        IF (REPEAT) THEN
          CALL PLOTXY (XSCALE, DATA, NCH, IFAIL)
        END IF

      END DO

*     Returns with same value as ISTAT in GEN_GETtna routines
*     Note that P and Q are not changed by IVTOPT - copy PNEW and QNEW
*     if a change is required.

      IF (ISTAT.EQ.1)  THEN                    ! =1, accept default
        TPAIR = 1
        CALL DRAW_DEFBOX (P, YLIMS, 0)

      ELSE IF (ISTAT.EQ.3)  THEN               ! =0, new values supplied
        TPAIR = 0
        DO J = 1,2
          P(J) = PNEW(J)
          Q(J) = QNEW(J)
        END DO

      ELSE IF (ISTAT.EQ.2) THEN                ! =2, ^Z or \ (end of list)
        TPAIR = 2

      ELSE
        TPAIR = -1                             ! -ve, error return
      END IF

      RETURN
      END

