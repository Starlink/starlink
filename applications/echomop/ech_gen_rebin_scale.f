       SUBROUTINE ECH_GEN_REBIN_SCALE(
     :            SET_SCALE,
     :            NX,
     :            N_ORDERS,
     :            WCOEFFS,
     :            MAXIMUM_POLY,
     :            INP_BIN_SIZE,
     :            START_WAVE,
     :            NO_OF_BINS,
     :            NX_REBIN,
     :            STATUS
     :           )
*+
*  Name:
*     ECHOMOP - ECH_GEN_REBIN_SCALE

*  Purpose:
*     Generate the rebinned wavelength scale factors.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     26-JUN-1996 (MJC):
*       New Prologue.  Tidy up and comment.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'

*  Arguments:
      LOGICAL SET_SCALE
      INTEGER NX
      INTEGER N_ORDERS
      INTEGER MAXIMUM_POLY
      DOUBLE PRECISION WCOEFFS( MAXIMUM_POLY, N_ORDERS )
      REAL INP_BIN_SIZE
      REAL START_WAVE
      INTEGER NO_OF_BINS
      INTEGER NX_REBIN
      INTEGER STATUS

*  Local Variables:
      REAL DIST
      REAL MIN_DIST
      REAL START_BIN_LAMBDA
      REAL END_BIN_LAMBDA
      REAL BIN_SIZE
      REAL WW1,WW2,WW3,WW4
      REAL VALUE1
      REAL VALUE2

      INTEGER I
      INTEGER J
      INTEGER BINS
      INTEGER FIRST
      INTEGER CENT_ORDER
      INTEGER HI_ORDER
      INTEGER LOW_ORDER
      INTEGER CAL_ORDERS
      INTEGER CAL_SO_FAR
      INTEGER NCHAR1
      INTEGER NCHAR2

      CHARACTER*32 REF_STR1
      CHARACTER*32 REF_STR2
*.

*  Determine range of orders which are wavelength-calibrated.
      LOW_ORDER = 0
      HI_ORDER = 0
      DO I = 1, N_ORDERS
         IF ( LOW_ORDER .EQ. 0 .AND. WCOEFFS( 1, I ) .NE. 0.0 )
     :      LOW_ORDER = I
         J = N_ORDERS - I + 1
         IF ( HI_ORDER .EQ. 0 .AND. WCOEFFS( 1, J ) .NE. 0.0 )
     :      HI_ORDER = J
      END DO

*  Find middle order of those calibrated.
      CENT_ORDER = 0
      CAL_ORDERS = HI_ORDER - LOW_ORDER + 1
      CAL_SO_FAR = 0
      DO I = 1, N_ORDERS
         IF ( WCOEFFS( 1, I ) .NE. 0.0 .AND.
     :        CAL_SO_FAR .LE. CAL_ORDERS / 2 ) THEN
            CENT_ORDER = I
            CAL_SO_FAR = CAL_SO_FAR + 1
         END IF
      END DO

*  Warning regarding uncalibrated orders.
      IF ( LOW_ORDER .GT. 1 .OR. HI_ORDER .LT. N_ORDERS ) THEN
         CALL ECH_REPORT( 0, ' Wavelengths have not yet' //
     :        ' been fitted to all orders so the range of ' )
         CALL ECH_REPORT( 0, ' the generated wavelength ' //
     :        'scale will be correspondingly restricted.' )
      END IF

*  Warning about no wavelength calibration and exit.
      IF ( LOW_ORDER .EQ. 0 .AND. HI_ORDER .EQ. 0 ) THEN
          CALL ECH_REPORT( 0, ' Wavelength scales have not yet' //
     :         ' been fitted using polynomials.' )
          CALL ECH_REPORT( 0, ' Unable to proceed with scrunching.' )
          STATUS = ECH__NO_WAVEFIT

      ELSE

*     Find range of wavelengths covered.
         CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WCOEFFS( 1, HI_ORDER ), 1,
     :        1.0, WW1, STATUS )
         CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WCOEFFS( 1, HI_ORDER ), 1,
     :        FLOAT( NX ), WW2, STATUS )
         CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WCOEFFS( 1, LOW_ORDER ), 1,
     :        1.0, WW3, STATUS )
         CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WCOEFFS( 1, LOW_ORDER ), 1,
     :        FLOAT( NX ), WW4, STATUS )
         START_BIN_LAMBDA = MIN( WW1, WW2, WW3, WW4 )
         END_BIN_LAMBDA = MAX( WW1, WW2, WW3, WW4 )

*     Display wavelength range covered.
         CALL CHR_RTOC( START_BIN_LAMBDA, REF_STR1, NCHAR1 )
         CALL CHR_RTOC( END_BIN_LAMBDA, REF_STR2, NCHAR2 )
         REPORT_STRING = ' Global scrunch to wavelengths from ' //
     :                   REF_STR1( :NCHAR1 ) // ' to ' //
     :                   REF_STR2( :NCHAR2 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )

*     Determine a suitable bin size if not defined.
         IF ( INP_BIN_SIZE .EQ. 0.0 ) THEN
            CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :           WCOEFFS( 1, CENT_ORDER ), 1,
     :           FLOAT( NX / 2 + 1 ), VALUE1, STATUS )
            CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :           WCOEFFS( 1, CENT_ORDER ), 1,
     :           FLOAT( NX / 2 ), VALUE2, STATUS )
            BIN_SIZE = VALUE1 - VALUE2

*     Display and select user-supplied bin size and start wavelength.
         ELSE IF ( SET_SCALE ) THEN
            CALL CHR_RTOC( INP_BIN_SIZE, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Bin size set to user-supplied value' //
     :                      ' of ' // REF_STR1( :NCHAR1 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )

            CALL CHR_RTOC( START_WAVE, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Start wavelength set to' //
     :                      ' user-supplied value of ' //
     :                      REF_STR1( :NCHAR1 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            START_BIN_LAMBDA = START_WAVE
            BIN_SIZE = INP_BIN_SIZE

*     Use supplied bin size.
         ELSE
            BIN_SIZE = INP_BIN_SIZE
         END IF

*     Determine number of bins required.
         NO_OF_BINS = INT( ( END_BIN_LAMBDA - START_BIN_LAMBDA ) /
     :                BIN_SIZE )

*     Set up the global wavelength-scale.
         IF ( SET_SCALE ) THEN
            MIN_DIST = 1.0E20
            BINS = 0
            NX_REBIN = 0
            FIRST = 0
            IF ( START_WAVE .GT. 0. ) THEN
               DO I = LOW_ORDER, HI_ORDER
                  CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WCOEFFS( 1, I ), 1,
     :                 1.0, START_BIN_LAMBDA, STATUS )
                  DIST = ABS( START_BIN_LAMBDA - START_WAVE )
                  IF ( DIST .LT. MIN_DIST ) THEN
                     MIN_DIST = DIST
                     FIRST = I
                  END IF
               END DO
               DO I = LOW_ORDER, HI_ORDER
                  CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WCOEFFS( 1, I ), 1,
     :                 1.0, START_BIN_LAMBDA, STATUS )
                  CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WCOEFFS( 1, I ), 1,
     :                 FLOAT( NX ), END_BIN_LAMBDA, STATUS )
                  IF ( I .EQ. FIRST ) THEN
                     START_BIN_LAMBDA = MIN( START_BIN_LAMBDA,
     :                                       START_WAVE )
                  END IF
                  BINS = INT( ABS( END_BIN_LAMBDA -
     :                        START_BIN_LAMBDA ) / BIN_SIZE + 1.0 )
                  IF ( BINS .GT. NX_REBIN ) NX_REBIN = BINS
               END DO

            ELSE
               DO I = LOW_ORDER, HI_ORDER
                  CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WCOEFFS( 1, I ), 1,
     :                 1.0, START_BIN_LAMBDA, STATUS )
                  CALL ECH_FEVAL( ' ', MAXIMUM_POLY, WCOEFFS( 1, I ), 1,
     :                 FLOAT( NX ), END_BIN_LAMBDA, STATUS )
                  BINS = INT( ABS( END_BIN_LAMBDA -
     :                             START_BIN_LAMBDA ) / BIN_SIZE + 1.0 )
                  IF ( BINS .GT. NX_REBIN ) NX_REBIN = BINS
               END DO
               START_BIN_LAMBDA = MIN( WW1, WW2, WW3, WW4 )
               END_BIN_LAMBDA = MAX( WW1, WW2, WW3, WW4 )
            END IF
            IF ( NX .GT. NX_REBIN ) NX_REBIN = NX

            CALL CHR_RTOC( BIN_SIZE, REF_STR1, NCHAR1 )
            CALL CHR_ITOC( NO_OF_BINS, REF_STR2, NCHAR2 )
            REPORT_STRING = ' Scrunch bin size set to ' //
     :                      REF_STR1( :NCHAR1 ) //
     :                      ' for a total of ' //
     :                      REF_STR2( :NCHAR2 ) // ' bins.'
            CALL ECH_REPORT( 0, REPORT_STRING )

            CALL CHR_ITOC( NX_REBIN, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Maximum number of bins in any order' //
     :                      ' is ' // REF_STR1( :NCHAR1 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )

*     Information, scales determined elsewhere.
         ELSE
            CALL ECH_REPORT( 0,
     :           ' Scrunch bin size will be set on a per-order basis.' )
            NX_REBIN = NX
            NO_OF_BINS = NX * N_ORDERS
         END IF
      END IF

      END
