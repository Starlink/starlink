      SUBROUTINE MEBKG( IBKG, NSUB, DSUB, QSUB, RSUB, WSUB )

*+
*
*   Name:
*      SUBROUTINE MEBKG
*
*   Description:
*      The mean background spectrum is formed.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      The pixel intensities in the background channel are folded with
*      a moving triangle function.
*      Pixels with non-zero Data Quality are omitted from the mean.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER IBKG             ! background channel index
      INTEGER NSUB             ! number of subset pixels

      INTEGER*2 DSUB(NSUB)     ! DATA values

      BYTE QSUB(NSUB)          ! QUAL values

      REAL*8 RSUB(NSUB)          ! R-coordinates
      REAL*8 WSUB(NSUB)          ! W-coordinates

*   Global variables:
      INCLUDE 'CMBUG'
      INCLUDE 'CMEXTP'
      INCLUDE 'CMBKG'
      INCLUDE 'CMWAV'
      INCLUDE 'CMSIST'
      INCLUDE 'CMCEN'

*   Local variables:
      REAL*8 R                   ! pixel R
      REAL*8 W                   ! pixel wavelength
      REAL*8 WBKG(1200)          ! accumulated weights
      REAL*8 WT                  ! folding weight
      REAL*8 W2BKG(1200)         ! accumulated squares of weights

      INTEGER I                ! wavelength index
      INTEGER I1               ! lower index
      INTEGER I2               ! upper index
      INTEGER J                ! pixel index
      INTEGER NUSED
      INTEGER NVOID

*.

*   Set BKG undefined
      NOBKG = .TRUE.

*   Zero out arrays
      DO I = 1, NWAV
         SBKG(I) = 0.0
         WBKG(I) = 0.0
         W2BKG(I) = 0.0
      END DO

*   Go through pixel list
      NBUSE(IBKG) = 0
      NBBAD(IBKG) = 0
      NBUND(IBKG) = 0

      DO 200 J = 1, NSUB
         W = WSUB(J)
         I = MAX(1, MIN(NWAV, NINT(REAL((W-WAV1)/DWAV)) + 1))
         R = RSUB(J) + SCEN(I)
         IF (R.GE.RBKG(1, IBKG) .AND. R.LE.RBKG(2, IBKG)) THEN
            IF (QSUB(J).EQ.0) THEN
               I1 = MAX(IFIX(REAL((W - BBASE - WAV1 + DWAV)
     :                      /DWAV)) + 1, 1)
               I2 = MIN(IFIX(REAL((W + BBASE - WAV1 + DWAV)
     :                      /DWAV)), NWAV)

               IF (I1.LE.I2) THEN
                  DO I = I1, I2
                     WT = MAX(1.0d0 - ABS(W - WAV(I))/BBASE, 0.0d0)
                     SBKG(I) = SBKG(I) + WT*DSUB(J)
                     WBKG(I) = WBKG(I) + WT
                     W2BKG(I) = W2BKG(I) + WT*WT
                  END DO
                  NBUSE(IBKG) = NBUSE(IBKG) + 1
               END IF

            ELSE
               NBUND(IBKG) = NBUND(IBKG) + 1
            END IF

         END IF

 200  CONTINUE

*   Post-process results
      NUSED = 0
      NVOID = 0

      ADBKG(IBKG) = 0.0
      ADBKG(IBKG) = 0.0

      DO 300 I = 1, NWAV
         IF (WBKG(I).GT.0.0) THEN
            SBKG(I) = SBKG(I)/WBKG(I)
            DBKG(I) = SQRT(W2BKG(I))/WBKG(I)
            QBKG(I) = 0
            ASBKG(IBKG) = ASBKG(IBKG) + SBKG(I)
            ADBKG(IBKG) = ADBKG(IBKG) + DBKG(I)*DBKG(I)
            NUSED = NUSED + 1

         ELSE
            QBKG(I) = 1
            NVOID = NVOID + 1
         END IF

         GBKG(I) = 0.0
 300  CONTINUE

*   Form spectrum wide mean
      IF ( NUSED .GT. 0 ) THEN
         ASBKG(IBKG) = ASBKG(IBKG) / DBLE(NUSED)
         ADBKG(IBKG) = SQRT(ADBKG(IBKG) / DBLE(NUSED))
      END IF

*   Set BKG defined
      NOBKG = .FALSE.

      END
