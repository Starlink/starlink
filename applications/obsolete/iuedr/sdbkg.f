      SUBROUTINE SDBKG( IBKG, NSUB, DSUB, QSUB, RSUB, WSUB )

*+
*
*   Name:
*      SUBROUTINE SDBKG
*
*   Description:
*      Evaluate pixel standard deviations in background.
*      The error on background pixels is determined, and used
*      to generate error estimates for the previously determined
*      mean background spectrum.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      The residual between each background pixel and its associated
*      mean level is determined, and used to create a local Standard
*      Deviation near each wavelength point.
*      The pixel residuals are folded with the same triangle function
*      that is used for the background smoothing.
*      The standard deviation is not derived normally, but is correct
*      for a large number of contributing pixels.
*      The aproximation is made that the pixel errors do not vary rapidly
*      across the folding range.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER IBKG             ! background channel index
      INTEGER NSUB             ! number of subset pixels

      INTEGER*2 DSUB(NSUB)     ! Data values

      BYTE QSUB(NSUB)          ! Data Quality values

      REAL*8 RSUB(NSUB)          ! R-coordinates
      REAL*8 WSUB(NSUB)          ! W-coordinates

*   Global variables:
      INCLUDE 'CMEXTP'
      INCLUDE 'CMDEV'
      INCLUDE 'CMBKG'
      INCLUDE 'CMWAV'
      INCLUDE 'CMSIST'
      INCLUDE 'CMCEN'

*   Local variables:
      REAL*8 R                   ! temporary
      REAL*8 V                   ! residual intensity
      REAL*8 V2                  ! square of residual intensity
      REAL*8 W                   ! pixel wavelength
      REAL*8 WDEV(1200)          ! accumulated folding weights
      REAL*8 WT                  ! folding weight

      INTEGER I                ! wavelength index
      INTEGER I1               ! lower wavelength index
      INTEGER I2               ! upper wavelength index
      INTEGER J                ! pixel index

*   Set DEV undefined
      NODEV = .TRUE.

*   Zero out arrays
      DO 100 I = 1, NWAV
         SDEV(I) = 0.0
         WDEV(I) = 0.0
 100  CONTINUE

*   Go though pixel list
      DO 200 J = 1, NSUB
         W = WSUB(J)
         I = MAX(1, MIN(NWAV, NINT(REAL((W-WAV1)/DWAV)) + 1))
         IF (QBKG(I).EQ.0) THEN
            R = RSUB(J) + SCEN(I)
            IF (R.GE.RBKG(1, IBKG) .AND. R.LE.RBKG(2, IBKG)) THEN
               IF (QSUB(J).EQ.0) THEN
                  I1 = MAX(IFIX(REAL((W - BBASE - WAV1 + DWAV)
     :                              /DWAV)) + 1, 1)
                  I2 = MIN(IFIX(REAL((W + BBASE - WAV1 + DWAV)
     :                              /DWAV)), NWAV)
                  IF (I1.LE.I2) THEN
                     V = DSUB(J) - SBKG(I)
                     V2 = V*V

                     DO 102 I = I1, I2
                        WT = MAX(1.0d0 - ABS((W - WAV(I))/BBASE), 0.0d0)
                        SDEV(I) = SDEV(I) + WT*V2
                        WDEV(I) = WDEV(I) + WT
 102                 CONTINUE
                  END IF
               END IF
            END IF
         END IF
 200  CONTINUE

*   Post-process results
      DO 300 I = 1, NWAV
         IF (WDEV(I).GT.0.0) THEN
            SDEV(I) = SQRT(SDEV(I)/(WDEV(I)))
            DBKG(I) = SDEV(I)*DBKG(I)
            QDEV(I) = 0

         ELSE
            DBKG(I) = 0.0
            QDEV(I) = 1
         END IF
 300  CONTINUE

*   Set DEV defined
      NODEV = .FALSE.

      END
