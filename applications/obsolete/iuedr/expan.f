      SUBROUTINE EXPAN( NSUB, DSUB, QSUB, RSUB, WSUB, MAXU, MAXV, FPAN,
     :                  WPAN, QPAN )

*+
*
*   Name:
*      SUBROUTINE EXPAN
*
*   Description:
*      Extract LBLS object signal - the LBLS array is extracted from
*      the supplied image.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-NOV-88     IUEDR Vn. 2.0
*      Martin Clayton     11-AUG-94     IUEDR Vn. 3.1-2
*      Martin Clayton     05-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*      The pixels in the object channel are folded with a triangle
*      function which has the same base width as the wavelength
*      sampling rate.
*      This means that each pixel intensity is shared between two
*      wavelength points.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      INTEGER NSUB              ! Number of subset pixels
      INTEGER*2 DSUB(NSUB)      ! DATA values

      BYTE QSUB(NSUB)           ! QUAL values

      REAL*8 RSUB(NSUB)         ! R-coordinates
      REAL*8 WSUB(NSUB)         ! W-coordinates

      INTEGER MAXU              ! u-size
      INTEGER MAXV              ! v-size

*   Export:
      REAL*8 FPAN(MAXU, MAXV)     ! flux array
      REAL*8 WPAN(MAXU, MAXV)     ! weight array

      BYTE QPAN(MAXU, MAXV)     ! data quality array

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMEXTP'
      INCLUDE 'CMDISH'
      INCLUDE 'CMCEN'
      INCLUDE 'CMWAV'
      INCLUDE 'CMBIN'
      INCLUDE 'CMLBLS'

*   Local variables:
      REAL*8 F
      REAL*8 PU
      REAL*8 PV
      REAL*8 U
      REAL*8 UBASE
      REAL*8 UBOT
      REAL*8 UFWHM
      REAL*8 UMID
      REAL*8 UPEAK
      REAL*8 UTOP
      REAL*8 V
      REAL*8 VBASE
      REAL*8 VBOT
      REAL*8 VFWHM
      REAL*8 VMID
      REAL*8 VPEAK
      REAL*8 VSCALE
      REAL*8 VTOP
      REAL*8 VV
      REAL*8 WP

      INTEGER IU                ! loop index
      INTEGER IUF
      INTEGER IUL
      INTEGER IV                ! loop index
      INTEGER IVF
      INTEGER IVL
      INTEGER J                 ! loop index
      INTEGER Q                 ! pixel Data Quality value
      INTEGER QI                ! array data quality value

*   Zero out arrays
      DO IV = 1, NV
         DO IU = 1, NU
            FPAN(IU, IV) = 0.0
            WPAN(IU, IV) = 0.0
            QPAN(IU, IV) = 0
         END DO
      END DO

*   Mediate between folding in U(R) and V(W), give precendence to V(W)
      VMID = MAX(0.7071D0, GSAMP)
      UMID = MAX((1.0d0 / VMID), ABS(RSAMP))

*   U==R
      NU = NINT(REAL((RL(2) - RL(1)) / RSAMP)) + 1
      U1 = RL(1)
      DU = RSAMP

      DO IU = 1, NU
         US(IU) = DBLE(IU - 1) * DU + U1
      END DO

      CALL STR_MOVE( 'R\\', 40, ULAB )
      CALL STR_MOVE( '(pixels)\\', 40, UUNT )

*   Define U-profile range and FWHM (NEW VERSION, all in pixels)
      UBASE = UMID + 0.7071
      UPEAK = 0.5 * UBASE / (UBASE - UMID)
      UFWHM = 0.5 * UBASE

*   V==W
      NV = NWAV
      V1 = WAV1
      DV = DWAV
      DO IV = 1, NV
         VS(IV) = DBLE(IV - 1) * DV + V1
      END DO

      CALL STR_MOVE( 'Wavelength\\', 40, VLAB )
      CALL STR_MOVE( '(A)\\', 40, VUNT )

*   Define the V-profile range and FWHM (all in pixels except VFWHM)
      VBASE = VMID + 0.7071
      VPEAK = 0.5 * VBASE / (VBASE - VMID)
      VFWHM = 0.5 * VBASE / DRDW

*   Data label/units
      CALL STR_MOVE( 'Gross\\', 40, DLAB )
      IF ( PHOT ) THEN
         CALL STR_MOVE( '(FN)\\', 40, DUNT )

      ELSE
         CALL STR_MOVE( '(DN)\\', 40, DUNT )
      END IF

*   Go through data list
      DO J = 1, NSUB

*      Correct for centroid shift (delta-R(W))
         V = WSUB(J)
         VV = (V - V1) / DV
         IV = MAX(1, MIN(NINT(REAL(VV)) + 1, NV))
         U = RSUB(J) + SCEN(IV)

*      Data and data quality
         F = DSUB(J)
         CALL DQ_UTOI( QSUB(J), Q )

*      Compute range in U-grid
         UBOT = U - UFWHM
         UTOP = U + UFWHM
         IUF = MAX(IFIX(REAL((UBOT - U1 + DU) / DU)) + 1, 1)
         IUL = MIN(IFIX(REAL((UTOP - U1 + DU) / DU)), NU)

         IF ( IUF .LE. IUL ) THEN

*         Compute range in V-grid
            VBOT = V - VFWHM
            VTOP = V + VFWHM
            IVF = MAX(IFIX(REAL((VBOT - V1 + DV) / DV)) + 1, 1)
            IVL = MIN(IFIX(REAL((VTOP - V1 + DV) / DV)), NV)

            IF ( IVF .LE. IVL ) THEN

*            Fold pixel intensity into grid
               DO IV = IVF, IVL
                  DO IU = IUF, IUL
                     IF ( Q .LT. QMAX ) THEN
                        QI = QPAN(IU, IV)
                        PU = MAX(0.0d0, MIN(1.0d0,
     :                         UPEAK * (1.0d0 - ABS(U - US(IU)) /
     :                         UFWHM)))
                        PV = MAX(0.0d0, MIN(1.0d0,
     :                         VPEAK * (1.0d0 - ABS(V - VS(IV)) /
     :                         VFWHM)))
                        WP = PU * PV
                        FPAN(IU, IV) = FPAN(IU, IV) + WP * F
                        WPAN(IU, IV) = WPAN(IU, IV) + WP
                        QPAN(IU, IV) = MAX(QI, Q)

                     ELSE
                        QPAN(IU, IV) = QMAX
                     END IF
                  END DO
               END DO
            END IF
         END IF
      END DO

*   Normalise F-values and create data quality
      DO IV = 1, NV
         VSCALE = SBIN(IV) * RSAMP
         DO IU = 1, NU
            IF ( QPAN(IU, IV) .EQ. QMAX ) THEN
               FPAN(IU, IV) = 0.0

            ELSE IF ( WPAN(IU, IV) .GT. 0.0 ) THEN
               FPAN(IU, IV) = FPAN(IU, IV) * VSCALE / WPAN(IU, IV)

            ELSE
               QPAN(IU, IV) = QMAX
            END IF

            IF ( QPAN(IU, IV) .GT. 0 ) THEN
               Q = QPAN(IU, IV)
               QI = 0

               IF ( Q .EQ. QMAX ) THEN
                  CALL DQ_WRPK( 1, 1, 1, QI )

               ELSE
                  CALL DQ_WRPK( 1, 2, 1, QI )
                  CALL DQ_WRPK( Q, 5, 4, QI )
               END IF

               CALL DQ_ITOU( QI, QPAN(IU, IV) )
            END IF
         END DO
      END DO

*   Say that LBLS defined
      NOLBLS = .FALSE.
      END
