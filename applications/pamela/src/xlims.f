      SUBROUTINE XLIMS(TRACE,YREF,LEFT,RIGHT,YLO,YHI,XLO,XHI,STATUS)
C
C     Evaluates minimum X range needed during extraction (allowing a
C     little bit more than needed for safety). It works in the
C     case of NPOLY>0 by computing the smallest and largest shifts
C     and adding these to the left and right limits.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      LOGICAL TRACE
      INTEGER YLO, YHI, XLO, XHI, IY, STATUS
      REAL LEFT, RIGHT, XSHIFT, XMIN, XMAX
      DOUBLE PRECISION XREF, YREF, XD, YD
C
      IF(STATUS.NE.SAI__OK) RETURN
      IF(TRACE) THEN
         CALL GET_TRACK(YREF, XREF, STATUS)
         IF(STATUS.NE.SAI__OK) RETURN
         XMIN =  1.E20
         XMAX = -1.E20
         DO IY = YLO, YHI
            CALL GET_TRACK(DBLE(IY), XD, STATUS)
            XSHIFT = REAL(XD-XREF)
            XMIN   = MIN(XMIN, XSHIFT)
            XMAX   = MAX(XMAX, XSHIFT)
         END DO
         XLO = NINT(LEFT  - 2. + XMIN)
         XHI = NINT(RIGHT + 2. + XMAX)
      ELSE
         XLO = NINT(LEFT)
         XHI = NINT(RIGHT)
      END IF
      RETURN
      END
