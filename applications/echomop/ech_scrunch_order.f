      SUBROUTINE ECH_SCRUNCH_ORDER(
     :           NNX,
     :           NO_OF_BINS,
     :           WAVELENGTH_SCALE,
     :           INPUT_SPECTRUM,
     :           N_ORDERS,
     :           WCOEFFS,
     :           REBIN_WORK,
     :           SCRNCHD_SCALE,
     :           REBINNED_SPECTRUM,
     :           WAVE_SCALE_INDEX
     :          )
*+
*  Name:
*     ECHOMOP - ECH_SCRUNCH_ORDER

*  Purpose:
*     Scrunch order into 1-D spectrum.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Header Files:
      INCLUDE 'ECH_INIT_RDCTN.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Arguments:
      INTEGER NNX
      INTEGER NO_OF_BINS
      DOUBLE PRECISION WAVELENGTH_SCALE( NO_OF_BINS )
      INTEGER N_ORDERS
      REAL INPUT_SPECTRUM( NNX, N_ORDERS )
      DOUBLE PRECISION WCOEFFS( MAX_POLY, N_ORDERS )
      REAL REBIN_WORK( NO_OF_BINS )
      DOUBLE PRECISION SCRNCHD_SCALE( NNX * 2, N_ORDERS )
      REAL REBINNED_SPECTRUM( NNX * 2, N_ORDERS )
      INTEGER WAVE_SCALE_INDEX( 2, 100 )

*  Local Variables:
      REAL SSKEW

      INTEGER IMODE
      INTEGER IQUAD
      INTEGER NADD
      INTEGER IORD
      INTEGER LOWLIM
      INTEGER HILIM
      INTEGER I
      INTEGER INDEX

      LOGICAL FLUX
*.

C  Loop through each cross-section of the image, generating the
C  input wavelength array and then scrunching the data accordingly.
C  Note that the image pointers have to be incremented after each
C  cross-section.
      IMODE = 1
      IQUAD = 1
      NADD = 1
      SSKEW = 0.0

      FLUX = .TRUE.
      DO IORD = 1, N_ORDERS
         DO I = 1, NNX * 2
            SCRNCHD_SCALE( I, IORD ) = 0.0
            REBINNED_SPECTRUM( I, IORD ) = 0.0
         END DO
         CALL FIG_WGEN( IORD, NNX, N_ORDERS,
     :        WCOEFFS, MAX_POLY, SCRNCHD_SCALE( 1, IORD ) )
         CALL FIG_REBIND( IMODE, IQUAD, INPUT_SPECTRUM( 1, IORD ),
     :        NNX, REBIN_WORK, NO_OF_BINS, NADD, SSKEW, FLUX,
     :        SCRNCHD_SCALE( 1, IORD ), WAVELENGTH_SCALE, .FALSE.,
     :        .FALSE. )
         LOWLIM = 1
         HILIM = 1
         DO I = 1, NO_OF_BINS
            IF ( WAVELENGTH_SCALE( I ) .GT. 0.0 ) THEN
               IF ( WAVELENGTH_SCALE( I ) .LT.
     :              SCRNCHD_SCALE( 1, IORD ) ) LOWLIM = I + 1
               IF ( WAVELENGTH_SCALE( I ) .LT.
     :              SCRNCHD_SCALE( NNX, IORD ) ) HILIM = I + 1
            END IF
         END DO
         WAVE_SCALE_INDEX( 1, IORD ) = LOWLIM
         WAVE_SCALE_INDEX( 2, IORD ) = HILIM - LOWLIM + 1
         DO INDEX = 1, NNX * 2
            SCRNCHD_SCALE( INDEX, IORD ) = 0.0
            REBINNED_SPECTRUM( INDEX, IORD ) = 0.0
         END DO
         DO INDEX = LOWLIM, HILIM
            SCRNCHD_SCALE( INDEX - LOWLIM + 1, IORD ) =
     :            WAVELENGTH_SCALE( INDEX )
            REBINNED_SPECTRUM( INDEX - LOWLIM + 1, IORD ) =
     :            REBIN_WORK( INDEX )
         END DO
      END DO

      END
