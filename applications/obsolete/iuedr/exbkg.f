      SUBROUTINE EXBKG( NSUB, DSUB, QSUB, RSUB, WSUB )
*+
*
*   Name:
*      SUBROUTINE EXBKG
*
*   Description:
*      Extract background - the smooth background is extracted from the
*      image subset list provided.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*      Martin Clayton     05-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*      The pixels are folded with a triangle function to produce a mean
*      background spectrum.
*      Those same pixels are then used in conjunction with that mean
*      to provide the Standard Deviation of pixels along the spectrum.
*      The errors of background pixels are used to reject deviant pixels
*      from further background determinations.
*      Each background channel is treated seperately, then they are merged
*      at the end.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NSUB             ! number of subset pixels

*   Import-Export:
      INTEGER*2 DSUB(NSUB)     ! DATA values

      BYTE QSUB(NSUB)          ! QUAL values

      REAL*8 RSUB(NSUB)        ! R-coordinates
      REAL*8 WSUB(NSUB)        ! W-coordinates

*   Global variables:
      INCLUDE 'CMEXTP'
      INCLUDE 'CMBKG'
      INCLUDE 'CMDEV'
      INCLUDE 'CMWAV'
      INCLUDE 'CMBKGS'
      INCLUDE 'CMDEVS'
      INCLUDE 'CMSIST'

*   Local variables:
      INTEGER I                ! wavelength index
      INTEGER IBKG             ! background channel index
      INTEGER ITER             ! iteration counter for background
      INTEGER NB               ! background points accumulator
      INTEGER NC               ! background points accumulator

      DO IBKG = 1, NBKG
         NBREJ(IBKG) = 0
         ITER = 0

         DO WHILE ( .TRUE. )
*         Evaluate mean background
            CALL MEBKG( IBKG, NSUB, DSUB, QSUB, RSUB, WSUB )

*         Evaluate standard deviation of pixels from mean background
            CALL SDBKG( IBKG, NSUB, DSUB, QSUB, RSUB, WSUB )

*         Finish after BKGIT iterations
            IF ( ITER .GE. BKGIT ) GO TO 100

*         Reject deviant pixels
            CALL REBKG( IBKG, NSUB, RSUB, WSUB, DSUB, QSUB )
            ITER = ITER + 1
         END DO
 100     CONTINUE

*      Unflag pixels marked as spikes
         IF ( BKGIT .GT. 0 )
     :      CALL UNBKG( IBKG, NSUB, RSUB, WSUB, DSUB, QSUB )

*      Store single background data arrays
         DO I = 1, NWAV
            SBKGS(I, IBKG) = SBKG(I)
            DBKGS(I, IBKG) = DBKG(I)
            QBKGS(I, IBKG) = QBKG(I)
            SDEVS(I, IBKG) = SDEV(I)
            QDEVS(I, IBKG) = QDEV(I)
         END DO
      END DO

*   Merge backgrounds and pixel errors
      DO I = 1, NWAV

*      Backgrounds
         SBKG(I) = 0.0
         DBKG(I) = 0.0
         QBKG(I) = 1
         NB = 0

         DO IBKG = 1, NBKG
            IF ( QBKGS(I, IBKG) .EQ. 0 ) THEN
               NB = NB + 1
               SBKG(I) = SBKG(I) + SBKGS(I, IBKG)
               DBKG(I) = DBKG(I) + DBKGS(I, IBKG) * DBKGS(I, IBKG)
               QBKG(I) = 0
            END IF
         END DO

         IF ( QBKG(I) .EQ. 0 ) THEN
            SBKG(I) = SBKG(I) / DBLE(NB)
            GBKG(I) = 0.0
            DBKG(I) = SQRT(DBKG(I)) / DBLE(NB)
         END IF

*      Pixel errors
         SDEV(I) = 0.0
         QDEV(I) = 1
         NC = 0

         DO IBKG = 1, NBKG
            IF ( QDEVS(I, IBKG) .EQ. 0 ) THEN
               SDEV(I) = SDEV(I) + SDEVS(I, IBKG)
               QDEV(I) = 0
               NC = NC + 1
            END IF
         END DO

         IF ( QDEV(I) .EQ. 0 ) SDEV(I) = SDEV(I) / DBLE(NC)
      END DO

      END
