*  History:
*     17 Nov 1993 (hme):
*        Change common name from GOODPT to GOOD_PT. Same in a number of
*        other source files in this directory. The same change will be
*        necessary in ../export.export_map.f and ../export/specx_wrfitsmap.f.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
C--------------------------------------------------------------

      LOGICAL FUNCTION GOODPT (INDEX,IX,IY,TEST)

C   Routine to check a map plane to see if the points have
C   been derived using data which is incomplete on the original,
C   possibly sparse, data.

      INTEGER*4 INDEX(*)
      INTEGER*4 TEST            ! Value to test against
      INTEGER*4 MRNGE(4),IG(2)

      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPHD'
      INCLUDE 'PLOT2D'

      LOGICAL*4 INVERT_AXIS
      COMMON /GOOD_PT/ INVERT_AXIS(3)

      GOODPT = .TRUE.
      IG(1)  = IX
      IG(2)  = IY

C   First get range of each index

      DO IAX = 1,3
        IREAL = LINK(IAX)
        IF (IREAL.EQ.3)   GO TO 100
        IM = 2*IREAL
        IOFFS = IOFF(IREAL)
        IF (IAX.EQ.3)   THEN
          MRNGE(IM-1) = IOFFS+1
          MRNGE(IM)   = IOFFS+NAX(IREAL)
        ELSE
          IF (INVERT_AXIS(IREAL)) IG(IAX) = NAX(IREAL)+1-IG(IAX)
          MM = IOFFS+IG(IAX)
          MRNGE(IM-1) = MM
          MRNGE(IM)   = MM
        END IF
  100   CONTINUE
      END DO

C  Then check INDEX over range of R.A. and Dec. indices

      DO J = MRNGE(1),MRNGE(2)
        DO K = MRNGE(3),MRNGE(4)
          IF (INDEX(J+(K-1)*MSTEP).LE.TEST)   GOODPT=.FALSE.
        END DO
      END DO

CD    PRINT *, ' -- goodpt --; ix, iy, goodpt = ', ix, iy, goodpt
      RETURN
      END


