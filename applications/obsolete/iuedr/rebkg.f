      SUBROUTINE REBKG( IBKG, NSUB, RSUB, WSUB, DSUB, QSUB )

*+
*
*   Name:
*      SUBROUTINE REBKG
*
*   Description:
*      Reject pixels in background.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      The residual between each background pixel and its associated
*      mean level is determined, and those deviating more than BKGSD
*      Standard Deviations are marked with the QSPK value.
*      Note that ONLY "perfect" pixels are marked, since others would not
*      be used anyway, and also since this means that pixels marked here
*      can be unmarked later.
*      This works because QSPK severity is reserved for automatically
*      located spikes.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER IBKG             ! background channel index
      INTEGER NSUB             ! number of subset pixels

      REAL*8 RSUB(NSUB)          ! R-coordinates
      REAL*8 WSUB(NSUB)          ! W-coordinates

      INTEGER*2 DSUB(NSUB)     ! DATA values

*   Import-Export:
      BYTE QSUB(NSUB)          ! QUAL values

*   Global Variables:
      INCLUDE 'CMEXTP'
      INCLUDE 'CMBKG'
      INCLUDE 'CMDEV'
      INCLUDE 'CMWAV'
      INCLUDE 'CMSIST'
      INCLUDE 'CMCEN'

*   Local variables:
      REAL*8 B                   ! interpolated background value
      REAL*8 R                   ! R-coordinate
      REAL*8 W                   ! W-coordinate

      INTEGER I                ! wavelength index
      INTEGER J                ! pixel index

*.

      DO 100 J = 1, NSUB
         W = WSUB(J)
         I = MAX(1, MIN(NWAV, NINT(REAL((W-WAV1)/DWAV)) + 1))
         IF (QBKG(I).EQ.0) THEN
            R = RSUB(J) + SCEN(I)
            IF (R.GE.RBKG(1, IBKG) .AND. R.LE.RBKG(2, IBKG)) THEN
               IF (QSUB(J).EQ.0) THEN
                  B = RSUB(J)*GBKG(I) + SBKG(I)
                  IF (ABS(DSUB(J) - B).GT.BKGSD*SDEV(I)) THEN
                     QSUB(J) = QSPK
                     NBREJ(IBKG) = NBREJ(IBKG) + 1
                  END IF
               END IF
            END IF
         END IF
 100  CONTINUE

      END
