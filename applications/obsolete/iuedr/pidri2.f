      SUBROUTINE PIDRI2(NXA, NYA, ZA, QA, BLANKA, XP, YP, IXP, IYP,
     :                  ZLIM, AZERO, ASCALE, LUTS, LUTMIN, LUTMAX,
     :                  LUTMIS, LUTUSR, LUTSEV, QUAL, IMAGE, STATUS)

*+
*
*   Name:
*      SUBROUTINE PIDRI2
*
*   Description:
*      Subset and scale an INTEGER*2 image.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      04-NOV-81
*         AT4 version.
*      Paul Rees          05-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees          09-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*      The pixels in the supplied 16-bit integer array are scaled
*      and subsetted from ZA to IMAGE.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      LOGICAL FALSE              ! .FALSE.
      LOGICAL TRUE               ! .TRUE.
      PARAMETER (FALSE=.FALSE., TRUE=.TRUE.)

      INTEGER IUEPIXRES          ! reseau affected pixel
      INTEGER IUEPIXSAT          ! saturated pixel
      INTEGER IUEPIXTRN          ! truncated ITF
      INTEGER IUEVALBRT          ! point affected by bright point
      INTEGER IUEVALMIS          ! point has no value
      INTEGER IUEVALRES          ! point affected by reseau mark
      INTEGER IUEVALSAT          ! point affected by saturation
      INTEGER IUEVALTRN          ! point affected by ITF truncation
      INTEGER MXPIX              ! maximum likely pixels
      PARAMETER (IUEPIXRES=8, IUEPIXSAT=7, IUEPIXTRN=6,
     :           IUEVALBRT=4, IUEVALMIS=8, IUEVALRES=5, IUEVALSAT=7,
     :           IUEVALTRN=6, MXPIX=768)

*   Import:
      INTEGER NXA                ! number of x-pixels in ZA
      INTEGER NYA                ! number of y-pixels in ZA

      INTEGER*2 ZA(NXA, NYA)     ! A array

      BYTE QA(NXA, NYA)          ! A data quality

      INTEGER BLANKA             ! blank pixel value in ZA
      INTEGER XP(2)              ! x-pixel subset in ZA
      INTEGER YP(2)              ! y-pixel subset in ZA
      INTEGER IXP                ! image X-axis size
      INTEGER IYP                ! image Y-axis size

      REAL*8 ZLIM(2)               ! z-limits in ZA
      REAL*8 AZERO                 ! offset for A
      REAL*8 ASCALE                ! scale for A

      INTEGER LUTS(2)            ! LUT limits (ZB limits for scaled values)
      INTEGER LUTMIN             ! LUT level for pixels below LUTS(1)
      INTEGER LUTMAX             ! LUT level for pixels above LUTS(2)
      INTEGER LUTMIS             ! missing value (bit 1 or blank) LUT level
      INTEGER LUTUSR             ! user-marked (bit 2) pixel LUT level
      INTEGER LUTSEV(8)          ! data quality severity LUT levels

      LOGICAL QUAL               ! whether data quality is displayed

*   Export:
      INTEGER IMAGE(IXP, IYP)    ! image return
      INTEGER STATUS             ! status return

*   Local variables:
      REAL*8 ZBMAX                 ! REAL*8 version of LUTS(2)
      REAL*8 ZBMIN                 ! REAL*8 version of LUTS(1)
      REAL*8 ZBR                   ! REAL*8 version of ZBP
      REAL*8 ZOFF                  ! offset from za to zb
      REAL*8 ZSCALE                ! sclae from za to zb

      INTEGER ZAP                ! work pixel from input
      INTEGER ZBI                ! work pixel for output image

      INTEGER IXA                ! A loop index
      INTEGER IXB                ! B loop index
      INTEGER IXD                ! A loop increment
      INTEGER IYA                ! A loop index
      INTEGER IYB                ! B loop index
      INTEGER IYD                ! A loop increment
      INTEGER Q                  ! temporary data quality
      INTEGER QBITS(8)           ! expanded data quality bits

*   Pixel intensity scaling
      ZSCALE = DBLE(LUTS(2) - LUTS(1)) / (ZLIM(2) - ZLIM(1))
      ZOFF = DBLE(LUTS(1)) - ZLIM(1) * ZSCALE
      ZBMIN = DBLE(LUTS(1)) - 0.5
      ZBMAX = DBLE(LUTS(2)) + 0.5

*   X-pixel order
      IF (XP(1).GT.XP(2)) THEN
         IXD = -1
      ELSE
         IXD = 1
      END IF

*   Y-pixel order
      IF (YP(1).GT.YP(2)) THEN
         IYD = -1
      ELSE
         IYD = 1
      END IF

*   Load IMAGE array
      IYB = 0

      DO IYA = YP(1), YP(2), IYD
         IYB = IYB + 1
         IXB = 0

         DO IXA = XP(1), XP(2), IXD
            IXB = IXB + 1
            ZAP = ZA(IXA, IYA)

            IF (ZAP.EQ.BLANKA) THEN
               ZBI = LUTMIS
            ELSE IF (QA(IXA, IYA).NE.0 .AND. QUAL) THEN
               CALL dq_UTOI(QA(IXA, IYA), Q)
               CALL dq_UNPK(Q, 8, QBITS)

               IF (QBITS(1).NE.0) THEN
                  ZBI = LUTMIS
               ELSE IF (QBITS(IUEPIXRES).NE.0) THEN
                  ZBI = LUTSEV(IUEVALRES)
               ELSE IF (QBITS(IUEPIXSAT).NE.0) THEN
                  ZBI = LUTSEV(IUEVALSAT)
               ELSE IF (QBITS(IUEPIXTRN).NE.0) THEN
                  ZBI = LUTSEV(IUEVALTRN)
               ELSE IF (QBITS(2).NE.0) THEN
                  ZBI = LUTUSR
               ELSE
                  ZBR = (DBLE(ZAP) * ASCALE + AZERO) * ZSCALE + ZOFF

                  IF (ZBR.LT.ZBMIN) THEN
                     ZBI = LUTMIN
                  ELSE IF (ZBR.GT.ZBMAX) THEN
                     ZBI = LUTMAX
                  ELSE
                     ZBI = NINT(REAL(ZBR))
                  END IF
               END IF

            ELSE
               ZBR = (DBLE(ZAP) * ASCALE + AZERO) * ZSCALE + ZOFF

               IF (ZBR.LT.ZBMIN) THEN
                  ZBI = LUTMIN
               ELSE IF (ZBR.GT.ZBMAX) THEN
                  ZBI = LUTMAX
               ELSE
                  ZBI = NINT(REAL(ZBR))
               END IF
            END IF

            IMAGE(IXB, IYB) = ZBI
         END DO
      END DO

      END
