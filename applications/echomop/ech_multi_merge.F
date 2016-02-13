#if HAVE_CONFIG_H
#  include "config.h"
#endif
      SUBROUTINE ECH_MULTI_MERGE(
     :           NX,
     :           NO_OF_BINS,
     :           SET_SCALE,
     :           WAVELENGTH_SCALE,
     :           INPUT_SPECTRUM,
     :           INPUT_VARIANCE,
     :           N_ORDERS,
     :           NX_REBIN,
     :           FRACTION,
     :           SSKEW,
     :           LOGR,
     :           FLUX,
     :           INTERPOLATE,
     :           QUAD,
     :           IMODE,
     :           NADD,
     :           AIR_TO_VAC,
     :           MIN_XGOOD,
     :           MAX_XGOOD,
     :           WEIGHTING,
     :           MAXIMUM_POLY,
     :           WAVE_COEFFS,
     :           SCRNCHD_SCALE,
     :           SPECTRUM,
     :           VARIANCE,
     :           REBINNED_SPECTRUM,
     :           REBINNED_VARIANCE,
     :           WAVE_SCALE_INDEX,
     :           REBIN_WORK,
     :           FILTERI,
     :           FILTERO,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_MULTI_MERGE

*  Purpose:
*     Scrunch orders in to 1 spectra (optionally merge).

*  Description:
*     This routine rebins a spectrum order from a wavelength polynomial
*     scale (as calibrated) to a linear wavelength scale.

*  Invocation:
*     CALL ECH_MULTI_MERGE(
*     :    NX,
*     :    NO_OF_BINS,
*     :    SET_SCALE,
*     :    WAVELENGTH_SCALE,
*     :    INPUT_SPECTRUM,
*     :    INPUT_VARIANCE,
*     :    N_ORDERS,
*     :    NX_REBIN,
*     :    FRACTION,
*     :    SSKEW,
*     :    LOGR,
*     :    FLUX,
*     :    INTERPOLATE,
*     :    QUAD,
*     :    IMODE,
*     :    NADD,
*     :    AIR_TO_VAC,
*     :    MIN_XGOOD,
*     :    MAX_XGOOD,
*     :    WEIGHTING,
*     :    MAXIMUM_POLY,
*     :    WAVE_COEFFS,
*     :    SCRNCHD_SCALE,
*     :    SPECTRUM,
*     :    VARIANCE,
*     :    REBINNED_SPECTRUM,
*     :    REBINNED_VARIANCE,
*     :    WAVE_SCALE_INDEX,
*     :    REBIN_WORK,
*     :    FILTERI,
*     :    FILTERO,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     ORDER_NUMBER = INTEGER (Given)
*        Order in echellogram.
*     NX_REBIN = INTEGER (Given)
*        Number of bins in scrunched order.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum degree of polynomials used.
*     SSKEW = REAL (Given)
*
*     FRACTION = REAL (Given)
*
*     LOGR = LOGICAL (Given)
*
*     FLUX = LOGICAL (Given)
*
*     INTERPOLATE = LOGICAL (Given)
*
*     QUAD = LOGICAL (Given)
*
*     IMODE = INTEGER (Given)
*
*     NADD = INTEGER (Given)
*
*     NO_OF_BINS = INTEGER (Given)
*        Number of bins in output scale.
*     INPUT_SPECTRUM = REAL (Given)
*        Input order spectrum.
*     INPUT_VARIANCE = REAL (Given)
*        Input order spectrum variances.
*     WAVE_COEFFS = DOUBLE (Given)
*        Polynomial coefficients of fit.
*     SCRNCHD_SCALE = DOUBLE (Given)
*        Scale to scrunch into.
*     WAVELENGTH_SCALE = DOUBLE (Given)
*        Wavelength Scale to scrunch into.
*     SPECTRUM = REAL (Returned)
*        1D linear Rebinned and merged spectrum.
*     VARIANCE = REAL (Returned)
*        1D linear Rebinned and merged variances.
*     REBINNED_SPECTRUM = REAL (Returned)
*        Rebinned order spectrum.
*     REBINNED_VARIANCE = REAL (Returned)
*        Rebinned order spectrum variances.
*     WAVE_SCALE_INDEX = INTEGER (Temporary Workspace)
*        Indicies into full wavelength scale.
*     REBIN_WORK = REAL (Temporary Workspace)
*        Workspace for rebinner.
*     SET_SCALE = LOGICAL (Given)
*        Set TRUE when global wavlength scale used.
*     AIR_TO_VAC = LOGICAL (Given)
*        Set TRUE when air to vacuum conversion required.
*     MIN_XGOOD = INTEGER (Given)
*        Minimum x pixels to be merged.
*     MAX_XGOOD = INTEGER (Given)
*        Maximum x pixels to be merged.
*     WEIGHTING = CHAR (Given)
*        Type of weighting for merge.
*     FILTERI = FLOAT (Given and Returned)
*        Work array for merge.
*     FILTERO = FLOAT (Given and Returned)
*        Work array for merge.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Setup/Examine mode variables
*     Read in list of reduction files whose spectra are to be scrunched
*     together
*     Loop thru orders
*        Initialise output arrays
*        If not a global scale then fill up relevant section of 'wavelength_scale'
*        Initialise this orders' start/end bin indices
*          Loop thru wavelength scale array
*           If less than first scrunched wavelength, set min index
*           If less than last scrunched wavelength, set min index
*          End loop
*        Set wavelength scale indices for order
*        Rebin order spectrum into new wavelength scale
*     End loop
*     Re-initialise output arrays
*     Loop thru limits of wavelength scale for this order
*        Copy scrunched data/scale into output arrays
*     End loop

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_USE_DIMEN.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER N_ORDERS
      INTEGER NO_OF_BINS
      INTEGER NX_REBIN
      INTEGER MAXIMUM_POLY
      INTEGER MIN_XGOOD
      INTEGER MAX_XGOOD
      LOGICAL AIR_TO_VAC
      CHARACTER*( * ) WEIGHTING
      LOGICAL SET_SCALE
      REAL INPUT_SPECTRUM( NX, N_ORDERS )
      REAL INPUT_VARIANCE( NX, N_ORDERS )
      DOUBLE PRECISION WAVELENGTH_SCALE( NO_OF_BINS )
      DOUBLE PRECISION SCRNCHD_SCALE( NX_REBIN, N_ORDERS )
      DOUBLE PRECISION WAVE_COEFFS( MAXIMUM_POLY, N_ORDERS, 2 )
      REAL SSKEW
      LOGICAL LOGR
      LOGICAL FLUX
      LOGICAL INTERPOLATE
      LOGICAL QUAD
      INTEGER IMODE
      INTEGER NADD

*  Arguments Returned:
      REAL SPECTRUM( NO_OF_BINS )
      REAL VARIANCE( NO_OF_BINS )
      REAL REBINNED_SPECTRUM( NX_REBIN, N_ORDERS )
      REAL REBINNED_VARIANCE( NX_REBIN, N_ORDERS )
      REAL FRACTION

*  Workspace:
      REAL FILTERI( NO_OF_BINS )
      REAL FILTERO( NO_OF_BINS )
      REAL REBIN_WORK( NO_OF_BINS,2 )
      INTEGER WAVE_SCALE_INDEX( 2, N_ORDERS )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER  MAX_FRAMES
      PARAMETER ( MAX_FRAMES = 500 )

*  Local Variables:
      DOUBLE PRECISION START_BIN_LAMBDA
      DOUBLE PRECISION END_BIN_LAMBDA

      REAL VEL( MAX_FRAMES )
      REAL DDAY

      INTEGER DUMDIM( MAX_DIMENSIONS )
      INTEGER YY
      INTEGER DD
      INTEGER MM
      INTEGER HOUR
      INTEGER MIN
      INTEGER SEC
      INTEGER YEAR
      INTEGER IAT
      INTEGER DAY_O_YEAR
      INTEGER WORK_SPECTRA
      INTEGER WORK_VARIANCE
      INTEGER WORK_COEFFS
      INTEGER WORK_REBSPECTRA
      INTEGER WORK_REBVARIANCE
      INTEGER NOBJ
      INTEGER IOBJ
      INTEGER AWORK_SPECTRA
      INTEGER AWORK_VARIANCE
      INTEGER AWAVE_COEFFS
      INTEGER AREBINNED_SWORK
      INTEGER AREBINNED_VWORK
      INTEGER ADDRESS
      INTEGER HANDLE
      INTEGER FLEN
      INTEGER ODLEN
      INTEGER LLUN
      INTEGER I
      INTEGER IORD
      INTEGER IQUAD
      INTEGER LOWLIM
      INTEGER HILIM
      INTEGER HI_ORDER
      INTEGER LOW_ORDER
      INTEGER SORD
      INTEGER EORD

      CHARACTER*80 EXTOBJ_NAME( MAX_FRAMES )
      CHARACTER*80 WORK_STRING
      CHARACTER*80 FNAME
      CHARACTER*80 RDC_NAME
      CHARACTER*10 OBS_DATE
      CHARACTER*10 OBS_TIME

      LOGICAL EXTOBJ_FILE( MAX_FRAMES )
      LOGICAL USDOWN

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :     CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      IF ( .NOT. set_scale ) THEN
         IF ( no_of_bins .NE. nx_rebin*n_orders ) THEN
            REPORT_STRING = ' Cannot scrunch: please regenerate' //
     :           ' wavelengths (option 12.2).'
            CALL ECH_REPORT( 0, REPORT_STRING )
            RETURN
         ENDIF
      ENDIF

      CALL ECH_REPORT( 0, ' Generating wavelengths for scrunch.' )

      LOW_ORDER = 0
      HI_ORDER = 0
      SORD = 1
      EORD = N_ORDERS
      DO I = SORD, EORD
         IF ( LOW_ORDER .EQ. 0 .AND.
     :        INT( WAVE_COEFFS( 1, I, 1 ) ) .NE. 0 )
     :      LOW_ORDER = I
         IF ( HI_ORDER .EQ. 0 .AND.
     :        INT( WAVE_COEFFS( 1, N_ORDERS - I + 1 , 1 ) ) .NE. 0 )
     :      HI_ORDER = N_ORDERS - I + 1
      END DO

      sord = low_order
      eord = hi_order
      IF ( hi_order - low_order + 1 .LT. n_orders ) THEN
         REPORT_STRING = ' Unable to scrunch some orders' //
     :        ' because they are not yet wavelength calibrated.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      ENDIF

      IF ( LOW_ORDER .GT. 0 .AND. HI_ORDER .GT. 0 ) THEN
        CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :       WAVE_COEFFS( 1, HI_ORDER, 1 ), 1, 1.0, START_BIN_LAMBDA,
     :       STATUS )
        CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :       WAVE_COEFFS( 1, LOW_ORDER, 1 ), 1, FLOAT( NX ),
     :       END_BIN_LAMBDA, STATUS )

      IF ( START_BIN_LAMBDA .GT. END_BIN_LAMBDA ) THEN
        CALL ECH_FEVAL( ' ',MAXIMUM_POLY,
     :       WAVE_COEFFS( 1, LOW_ORDER, 1 ), 1, 1.0, START_BIN_LAMBDA,
     :       STATUS )
        CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :       WAVE_COEFFS( 1, HI_ORDER, 1 ), 1, FLOAT( NX ),
     :       END_BIN_LAMBDA, STATUS )
      ENDIF

      IF ( SET_SCALE ) THEN
         CALL FIG_WFILLD( DBLE( START_BIN_LAMBDA ),
     :        DBLE( END_BIN_LAMBDA ), .FALSE., NO_OF_BINS,
     :        WAVELENGTH_SCALE )
      ENDIF

      DO I = 1, NO_OF_BINS
         SPECTRUM( I ) = 0.0
         VARIANCE( I ) = 0.0
      END DO

*     Setup/Examine mode variables
      IF ( QUAD ) THEN
         IQUAD = 1

      ELSE
         IQUAD = 0
      END IF

*     Read in list of reduction files whose spectra are to be scrunched
*     together
      CALL ECH_OPEN_FILE( 'NAMES.LIS', 'TEXT', 'OLD', .FALSE.,
     :     LLUN, FNAME, STATUS )
      IF ( STATUS .NE. 0 ) RETURN
      NOBJ = 1
      DO WHILE ( STATUS  .EQ. 0 )
         READ ( LLUN, '( A )', END = 100 ) RDC_NAME
         NOBJ = NOBJ + 1
      END DO
 100  CONTINUE
      CALL ECH_OPEN_FILE( 'NAMES.LIS', 'CLOSE', 'OLD', .FALSE.,
     :     LLUN, FNAME, STATUS )
      CALL ECH_OPEN_FILE( 'NAMES.LIS', 'TEXT', 'OLD', .FALSE.,
     :     LLUN, FNAME, STATUS )
      CALL ECH_ACCESS_OBJECT( 'WORKSPACE', 'MAP', 'FLOAT',
     :     NX * N_ORDERS * NOBJ * 4, WORK_SPECTRA, HANDLE,
     :     DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
      CALL ECH_ACCESS_OBJECT( 'WORKSPACE', 'MAP', 'FLOAT',
     :     NX * N_ORDERS * NOBJ * 4, WORK_VARIANCE, HANDLE,
     :     DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
      CALL ECH_ACCESS_OBJECT( 'WORKSPACE', 'MAP', 'FLOAT',
     :     NX_REBIN * N_ORDERS * NOBJ * 4, WORK_REBSPECTRA, HANDLE,
     :     DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
      CALL ECH_ACCESS_OBJECT( 'WORKSPACE', 'MAP', 'FLOAT',
     :     NX_REBIN * N_ORDERS * NOBJ * 4, WORK_REBVARIANCE, HANDLE,
     :     DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
      CALL ECH_ACCESS_OBJECT( 'WORKSPACE', 'MAP', 'DOUBLE',
     :     MAXIMUM_POLY * N_ORDERS * NOBJ * 8, WORK_COEFFS, HANDLE,
     :     DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )

      DO IOBJ = 1, NOBJ
         extobj_file( iobj ) = .FALSE.
         IF ( iobj .GT. 1 ) THEN
            FNAME = 'CLONE'
            FLEN = 5
            READ ( LLUN, '( A )', END = 101 ) RDC_NAME
            CALL ECH_ACCESS_OBJECT( 'CLONE', 'OPEN', 'STRUCTURE',
     :           0, 0, 0, DUMDIM, MAX_DIMENSIONS, 0, RDC_NAME, STATUS )

         ELSE
            FNAME = 'ECH_RDCTN'
            RDC_NAME = ' '
            FLEN = 9
         END IF

         ADDRESS = WORK_SPECTRA + ( IOBJ - 1 ) * NX * N_ORDERS * 4
         WORK_STRING = FNAME( :FLEN ) // '.MAIN.EXTRACTED_OBJ'
         CALL ECH_ACCESS_OBJECT( WORK_STRING,
     :        'READ', 'FLOAT', NX * N_ORDERS, ADDRESS, HANDLE,
     :        DIMENSIONS, MAX_DIMENSIONS, NUM_DIM, ' ', STATUS )
         IF ( STATUS .NE. 0 ) THEN
            IF ( IOBJ .EQ. 1 ) THEN
              CALL ECH_REPORT ( 0,
     :           ' Cannot find extracted spectra in reduction file.')
              CALL ECH_REPORT ( 0,
     :           ' Use an extraction option before attempting merge.')
              RETURN

            ELSE
               EXTOBJ_FILE( IOBJ ) = .TRUE.
               EXTOBJ_NAME( IOBJ ) = RDC_NAME
               WORK_STRING = FNAME( :FLEN ) // '.Z.DATA'
               CALL ECH_ACCESS_OBJECT( WORK_STRING,
     :              'READ', 'FLOAT', NX * N_ORDERS, ADDRESS, HANDLE,
     :              DIMENSIONS, MAX_DIMENSIONS, NUM_DIM, ' ', STATUS )
               IF ( STATUS .EQ. 0 ) THEN
                  WRITE ( REPORT_STRING, 1006 ) IOBJ
                  CALL ECH_REPORT ( 0,REPORT_STRING )

               ELSE
                  CALL ECH_REPORT( 0,
     :            ' Cannot find extracted spectra in specified file.' )
                  CALL ECH_REPORT( 0,
     :            ' Use an extraction option before attempting merge.' )
                  RETURN
               ENDIF
            ENDIF
         ENDIF

         IF ( MIN_XGOOD .GT. 0 .OR. MAX_XGOOD .GT. 0 ) THEN
            CALL CLIPENDS( MIN_XGOOD, MAX_XGOOD, %VAL( ADDRESS ),
     :           NX, N_ORDERS )
         ENDIF

         ADDRESS = WORK_VARIANCE + ( IOBJ - 1 ) * NX * N_ORDERS * 4
         IF ( .NOT. EXTOBJ_FILE( IOBJ ) ) THEN
            WORK_STRING = FNAME( :FLEN ) // '.MAIN.EXTR_OBJ_VAR'
            CALL ECH_ACCESS_OBJECT( WORK_STRING,
     :           'READ', 'FLOAT', NX * N_ORDERS, ADDRESS, HANDLE,
     :           DIMENSIONS, MAX_DIMENSIONS, NUM_DIM, ' ', STATUS )

         ELSE
            WORK_STRING = FNAME( :FLEN ) // '.Z.VARIANCE'
            CALL ECH_ACCESS_OBJECT( WORK_STRING,
     :           'READ', 'FLOAT', NX * N_ORDERS, ADDRESS, HANDLE,
     :           DIMENSIONS,MAX_DIMENSIONS,NUM_DIM, ' ', STATUS )
            ENDIF
            IF ( MIN_XGOOD .GT. 0 .OR. MAX_XGOOD .GT. 0 ) THEN
               CALL CLIPENDS( MIN_XGOOD, MAX_XGOOD, %VAL( ADDRESS ),
     :              NX, N_ORDERS )
            ENDIF
            ADDRESS = WORK_COEFFS + ( IOBJ - 1 ) * MAXIMUM_POLY *
     :                N_ORDERS * 8
            IF ( .NOT. extobj_file(iobj) ) THEN
             WORK_STRING = FNAME( :FLEN ) // '.MAIN.WAVE_FIT_1D.W_POLY'
             CALL ECH_ACCESS_OBJECT ( WORK_STRING, 'READ', 'DOUBLE',
     :            maximum_poly*n_orders,address, handle,
     :            dimensions,max_dimensions,num_dim, ' ', status )
            END IF
            WORK_STRING = FNAME( :FLEN ) // '.OBS_DATE'
            CALL ECH_ACCESS_OBJECT( WORK_STRING, 'READ', 'CHAR',
     :           ODLEN, 0, 0, DUMDIM, MAX_DIMENSIONS, 0, OBS_DATE,
     :           STATUS )
            vel( iobj ) = 0.0
            IF ( status .EQ. 0 ) THEN
             WORK_STRING = FNAME( :FLEN ) // '.OBS_UTTIME'
             CALL ECH_ACCESS_OBJECT( WORK_STRING, 'READ', 'CHAR',
     :            ODLEN, 0, 0, DUMDIM, MAX_DIMENSIONS, 0, OBS_TIME,
     :            STATUS )
             IF ( STATUS .EQ. 0 )
     :          READ ( OBS_DATE, '(I2,1X,I2,1X,I2)', IOSTAT = STATUS )
     :               DD, MM, YY
             IF ( STATUS .EQ. 0 )
     :          READ ( OBS_TIME, '(I2,1X,I2,1X,I2)', IOSTAT = STATUS )
     :               HOUR, MIN, SEC
             IF ( STATUS .EQ. 0 )
     :          CALL SLA_CALYD( YY, MM, DD, YEAR, DAY_O_YEAR, STATUS )
             IF ( STATUS .EQ. 0 )
     :          CALL SLA_DTF2D( HOUR, MIN, SEC, DDAY, STATUS )
             IF ( STATUS .EQ. 0 ) THEN
                CALL SLA_EARTH( YEAR, DAY_O_YEAR, DDAY, VEL( IOBJ ) )
                WRITE ( REPORT_STRING, 1005 )
     :                IOBJ, OBS_DATE, OBS_TIME, VEL( IOBJ )
                CALL ECH_REPORT( 0, REPORT_STRING )
             ENDIF

            ELSE
              REPORT_STRING = ' No observation date/time in' //
     :              ' reduction file ' // RDC_NAME
              CALL ECH_REPORT( 0, REPORT_STRING )
              WRITE ( report_string, 1004 ) iobj
              CALL ECH_REPORT ( 0, report_string )
            ENDIF
            STATUS = 0
            IF ( IOBJ .GT. 1 )
     :         CALL ECH_ACCESS_OBJECT( 'CLONE', 'UNMAP', 'STRUCTURE',
     :              0, 0, 0, DUMDIM, MAX_DIMENSIONS, 0,
     :              RDC_NAME, STATUS )
            STATUS = 0
      END DO
 101  CONTINUE

       DO IOBJ = 1, NOBJ
          IF ( EXTOBJ_FILE( IOBJ ) )
     :         CALL ECH_ACCESS_OBJECT( 'CLONE', 'OPEN', 'STRUCTURE',
     :              0, 0, 0, DUMDIM, MAX_DIMENSIONS, 0,
     :              EXTOBJ_NAME( IOBJ ), STATUS )
          AWAVE_COEFFS = WORK_COEFFS + ( IOBJ - 1 ) *
     :          maximum_poly * n_orders * 8

*     Loop thru orders
      DO iord = 1, n_orders
         WRITE ( report_string, 1000 ) iord,iobj
         CALL ECH_REPORT ( 0, report_string )
         IF ( INT(wave_coeffs ( 1, iord, 1 )) .NE. 0 ) THEN

*        Initialise output arrays
         DO i = 1, nx_rebin

*!!!! NOTE, the array scrnchd_scale is used as temporary workspace to hold
*!!!!        the polynomial wavelength scale for input to the rebin routine.
*!!!!        In this context only the first 'nx' entries have values.
*!!!!        After rebinning has been done, the actual scrunched wavelengths
*!!!!        are copied into the array, utilising all 'nx_rebin' elements.

            scrnchd_scale ( i, iord ) = 0.0
            rebinned_spectrum ( i, iord ) = 0.0
            rebinned_variance ( i, iord ) = 0.0
         END DO

         DO i = 1, no_of_bins
            rebin_work ( i,1 ) = 0.0
            rebin_work ( i,2 ) = 0.0
         END DO

         IF ( extobj_file(iobj) ) THEN
#if HAVE_PERCENTLOC
          address = %LOC(scrnchd_scale(1,iord))
#elif HAVE_INTRINSIC_LOC
          address = LOC(scrnchd_scale(1,iord))
#else
          Do not have LOC or %LOC
#endif
          WRITE ( fname, 1003 ) iord
          CALL ECH_ACCESS_OBJECT (  fname(1:19),
     :                               'READ', 'DOUBLE',
     :                               nx, address, handle,
     :                           dimensions,max_dimensions,num_dim,
     :                           ' ', status )

         ELSE
          CALL FIG_WGEN( iord,
     :                     nx,
     :                     n_orders,
     :                     %VAL(awave_coeffs),
     :                     maximum_poly,
     :                     scrnchd_scale ( 1, iord ) )
         ENDIF

         IF ( INT(vel(iobj)) .NE. 0 ) THEN
           CALL VCHLCON_DBLE( scrnchd_scale(1,iord),
     :                   nx,vel(iobj),.TRUE.,air_to_vac)
         ENDIF


*        If not a global scale then fill up relevant section of 'wavelength_scale'
         IF ( iobj .EQ. 1 ) THEN
          IF ( .NOT. set_scale ) THEN
           usdown = .FALSE.
           CALL ECH_FEVAL ( ' ',maximum_poly,
     :                   wave_coeffs(1,1,1), 1,
     :                   1., start_bin_lambda, status)
           CALL ECH_FEVAL ( ' ',maximum_poly,
     :                   wave_coeffs(1,n_orders,1), 1,
     :                   FLOAT ( nx ), end_bin_lambda, status)
           IF ( start_bin_lambda .GT. end_bin_lambda ) usdown = .TRUE.
           CALL ECH_FEVAL ( ' ',maximum_poly,
     :                   wave_coeffs(1,iord,1), 1,
     :                   1., start_bin_lambda, status)
           CALL ECH_FEVAL ( ' ',maximum_poly,
     :                   wave_coeffs(1,iord,1), 1,
     :                   FLOAT ( nx ), end_bin_lambda, status)
           IF ( start_bin_lambda .GT. end_bin_lambda ) THEN
             CALL ECH_FEVAL ( ' ',maximum_poly,
     :                   wave_coeffs(1,iord,1), 1,
     :                   FLOAT(nx), start_bin_lambda, status)
             CALL ECH_FEVAL ( ' ',maximum_poly,
     :                   wave_coeffs(1,iord,1), 1,
     :                   1., end_bin_lambda, status)
           ENDIF
           IF ( usdown ) THEN
              CALL FIG_WFILLD( DBLE(start_bin_lambda),
     :                      DBLE(end_bin_lambda),.FALSE.,
     :                      nx_rebin,
     :                wavelength_scale((n_orders-iord)*nx_rebin+1) )
              lowlim = (n_orders-iord)*nx_rebin+1
              hilim = (n_orders-iord+1)*nx_rebin-1
           ELSE
              CALL FIG_WFILLD( DBLE(start_bin_lambda),
     :                      DBLE(end_bin_lambda),.FALSE.,
     :                      nx_rebin,
     :                      wavelength_scale((iord-1)*nx_rebin+1) )
              lowlim = (iord-1)*nx_rebin+1
              hilim = iord*nx_rebin-1
           ENDIF

*        Initialise this orders' start/end bin indices
          ELSE
           lowlim = 1
           hilim = 1

*          Loop thru wavelength scale array

           DO i = 1, no_of_bins

*           If less than first scrunched wavelength, set min index
            IF ( wavelength_scale ( i ) .GT. 0.0 ) THEN

               IF ( lowlim .EQ. 1 ) lowlim = i
               IF ( wavelength_scale ( i ) .LT.
     :              scrnchd_scale ( 1, iord ) ) lowlim = i+1

*           If less than last scrunched wavelength, set min index
               IF ( wavelength_scale ( i ) .LE.
     :              scrnchd_scale ( nx, iord ) ) hilim = i

            ENDIF
           END DO
           lowlim = lowlim + 1 + nx / nx_rebin
           hilim = hilim - 1 - nx / nx_rebin

          ENDIF

*        Set wavelength scale indices for order
          wave_scale_index ( 1, iord ) = lowlim
          wave_scale_index ( 2, iord ) = hilim - lowlim + 1
         ELSE
           lowlim = wave_scale_index ( 1, iord )
           hilim = wave_scale_index ( 2, iord ) + lowlim - 1
         ENDIF

*        Rebin order spectrum into new wavelength scale
         awork_spectra = work_spectra + (iobj-1)*4*nx*n_orders +
     :                   (iord-1)*4*nx
         CALL FIG_REBIND ( IMODE,
     :                     IQUAD,
     :                     %VAL(awork_spectra),
     :                     nx,
     :                     rebin_work(1,1),
     :                     no_of_bins,
     :                     NADD,
     :                     SSKEW,
     :                     FLUX,
     :                     scrnchd_scale ( 1, iord ),
     :                     wavelength_scale,
     :                     .FALSE.,
     :                     .FALSE.    )

         arebinned_swork = work_rebspectra +
     :                         (iobj-1)*4*nx_rebin*n_orders
         CALL COPY_SCRNCD ( nx_rebin, lowlim, hilim,
     :                      no_of_bins, n_orders, iord,
     :                      wavelength_scale, scrnchd_scale,
     :                      rebin_work, %VAL ( arebinned_swork ) )

                  IF ( EXTOBJ_FILE( IOBJ ) ) THEN
#if HAVE_PERCENTLOC
                     ADDRESS = %LOC( SCRNCHD_SCALE( 1, IORD ) )
#elif HAVE_INTRINSIC_LOC
                     ADDRESS = LOC( SCRNCHD_SCALE( 1, IORD ) )
#else
          Do not have LOC or %LOC
#endif
                     WRITE ( fname, 1003 ) iord
                     CALL ECH_ACCESS_OBJECT( FNAME( 1 : 19 ), 'READ',
     :                    'DOUBLE', NX, ADDRESS, HANDLE, DIMENSIONS,
     :                     MAX_DIMENSIONS, NUM_DIM, ' ', STATUS )

                  ELSE
                     CALL FIG_WGEN( IORD, NX, N_ORDERS,
     :                    %VAL( AWAVE_COEFFS ), MAXIMUM_POLY,
     :                    SCRNCHD_SCALE( 1, IORD ) )
                  ENDIF

                  IF ( INT( VEL( IOBJ ) ) .NE. 0 ) THEN
                     CALL VCHLCON_DBLE( SCRNCHD_SCALE( 1, IORD ),
     :                    NX, VEL( IOBJ ), .TRUE., AIR_TO_VAC )
                  ENDIF

                  AWORK_VARIANCE = WORK_VARIANCE + ( IOBJ - 1 ) * 4 *
     :                             NX * N_ORDERS + ( IORD - 1 ) * 4 * NX
                  CALL FIG_REBIND( IMODE, IQUAD, %VAL( AWORK_VARIANCE ),
     :                 NX, REBIN_WORK( 1, 2 ), NO_OF_BINS, NADD, SSKEW,
     :                 FLUX, SCRNCHD_SCALE( 1, IORD ), WAVELENGTH_SCALE,
     :                 .FALSE., .FALSE. )
                  AREBINNED_VWORK = WORK_REBVARIANCE + ( IOBJ - 1 ) *
     :                              4 * NX_REBIN * N_ORDERS
                  CALL COPY_SCRNCD( NX_REBIN, LOWLIM, HILIM,
     :                 NO_OF_BINS, N_ORDERS, IORD,
     :                 WAVELENGTH_SCALE, SCRNCHD_SCALE,
     :                 REBIN_WORK( 1, 2 ), %VAL( AREBINNED_VWORK ) )

               ELSE
                  REPORT_STRING = ' Order has no wavelength ' //
     :                 'scale, no scrunching done.'
                  CALL ECH_REPORT( 0, REPORT_STRING )
               ENDIF

            END DO

            IF ( EXTOBJ_FILE( IOBJ ) )
     :         CALL ECH_ACCESS_OBJECT( 'CLONE', 'UNMAP', 'STRUCTURE',
     :              0, 0, 0, DUMDIM, MAX_DIMENSIONS, 0,
     :              EXTOBJ_NAME( IOBJ ), STATUS )
         END DO

         CALL ECH_MERGE_SPECTRA( NX_REBIN, N_ORDERS, NOBJ, WEIGHTING,
     :        %VAL( WORK_REBSPECTRA ), %VAL( WORK_REBVARIANCE ),
     :        REBINNED_SPECTRUM, REBINNED_VARIANCE )

         DO IORD = 1, N_ORDERS
            IAT = WAVE_SCALE_INDEX( 1, IORD )
            CALL ECH_MMERGE_ORDERS( NX_REBIN, 1,
     :           REBINNED_SPECTRUM( 1, IORD ),
     :           REBINNED_VARIANCE( 1, IORD ), 51, 10.0,
     :           SPECTRUM( IAT ), VARIANCE( IAT ), FILTERI, FILTERO,
     :           STATUS )
         END DO

         CALL ECH_ACCESS_OBJECT( 'WORKSPACE', 'UNMAP', 'FLOAT',
     :        NX * N_ORDERS * NOBJ * 4, WORK_SPECTRA, HANDLE, DUMDIM,
     :        MAX_DIMENSIONS, 0, ' ', STATUS )
         CALL ECH_ACCESS_OBJECT( 'WORKSPACE', 'UNMAP', 'FLOAT',
     :        NX * N_ORDERS * NOBJ * 4, WORK_VARIANCE, HANDLE, DUMDIM,
     :        MAX_DIMENSIONS, 0, ' ', STATUS )
         CALL ECH_ACCESS_OBJECT( 'WORKSPACE', 'UNMAP', 'FLOAT',
     :        NX_REBIN * N_ORDERS * NOBJ * 4, WORK_REBSPECTRA, HANDLE,
     :        DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
         CALL ECH_ACCESS_OBJECT( 'WORKSPACE', 'UNMAP', 'FLOAT',
     :        NX_REBIN * N_ORDERS * NOBJ * 4, WORK_REBVARIANCE, HANDLE,
     :        DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
         CALL ECH_ACCESS_OBJECT( 'WORKSPACE', 'UNMAP', 'DOUBLE',
     :        MAXIMUM_POLY * N_ORDERS * NOBJ * 8, WORK_COEFFS, HANDLE,
     :        DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )

      ELSE
         REPORT_STRING = ' Unable to scrunch' //
     :        ' as reduction is not yet wavelength calibrated.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      ENDIF

 1000 FORMAT ( 1X, 'Scrunching spectrum order ',I5,
     :               ' in object ',I4 )
 1001 FORMAT ( 1X, 'Scrunching blaze spectrum for order ',I5 )
 1003 FORMAT ( 'CLONE.X.DATA[1,',I3.3,']')
 1004 FORMAT ( 1X,'Frame ',I4,3X,'Unknown   Unknown   ',
     :               ' No Heliocentric correction' )
 1005 FORMAT ( 1X,'Frame ',I4,3X,A,A,
     :               ' Heliocentric correction = ',F12.4,' Km/s')
 1006 FORMAT ( 1X, 'Reduction file ', I3,
     :               ' is an extracted orders file' )

      END


      SUBROUTINE ECH_MMERGE_ORDERS ( NX,NY,IMAGE,err_image,
     :                              BOX,CUTOFF,OUTPUT,
     :                      err_output,ifilt,ofilt,status)
*
*                          F I G _ E C H A D D I N
*
*  Routine name:
*     FIG_ECHADDIN
*
*  Function:
*     Adds in orders from a scrunched echellograms to a 1D spectrum.
*
*  Description:
*     Loop through all the orders in the input image. For each, calculate
*     a median filtered version of both the input and the current state
*     of the output. Then use the median filtered values as weights when
*     combining the orders from the input image and the output image. Where
*     the higher signal is less than CUTOFF times the lower signal, do not
*     take any contribution from the lower signal (since to do so would
*     probably degrade the signal to noise ratio).
*
*  Language:
*     FORTRAN
*
*  Call:
*     CALL FIG_ECHADDIN NX,NY,IMAGE,BOX,CUTOFF,OUTPUT)
*
*  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
*
*     (>) NX            (Integer,ref) Size of X dimension of both the images
*     (>) NY            (Integer,ref) Size of Y dimension of IMAGE
*     (>) IMAGE(NX,NY)  (Real array,ref) Input image
*     (>) BOX           (Integer,ref) Width of box used for median filtering of
*                       orders as part of weights estimation
*     (>) CUTOFF        (Real,ref) Maximum ratio of stronger signal to weaker
*                       signal that will still allow a contribution from the
*                       weaker signal
*     (<) OUTPUT(NX)    (Real array,ref) The output spectrum containing the
*                       merged orders
*
*  External variables used:
*
*     None
*
*  External subroutines / functions used:
*
*     KPG1_BMEDR
*
*  Prior requirements:
*     None
*
*  Support: William Lupton, AAO
*
*  Version date: 08-Jun-88
*-
*  History:
*     2004 August 20 (norman@astro.gla.ac.uk): replace Figaro routine
*        GEN_MEDFLT with kaplibs routine KPG1_BMEDR.
*

*
*     Parameter declarations
*
      INTEGER NX,NY,BOX
      INTEGER            status
      REAL IMAGE(NX,NY),CUTOFF,OUTPUT(NX),err_output(nx)
      REAL err_IMAGE(NX,NY)
*
*     Constant parameter declarations
*
      INTEGER MAXBOX
      PARAMETER (MAXBOX=501)
*
*     Local variable declarations
*
      INTEGER I,J
      REAL WORK(MAXBOX),IFILT(NX),OFILT(NX),IWEIGHT,OWEIGHT
      INTEGER WORKI(MAXBOX)
*
*     Loop through all the orders in the input image. For each, calculate
*     a median filtered version of both the input and the current state
*     of the output. Then use the median filtered values as weights when
*     combining the orders from the input image and the output image. Where
*     the higher signal is less than CUTOFF times the lower signal, do not
*     take any contribution from the lower signal (since to do so would
*     probably degrade the signal to noise ratio).
*
      status  = 0
      DO i = 1, nx
         ifilt ( i ) = 0.0
         ofilt ( i ) = 0.0
      END DO

      DO I = 1,NY
*     This used to call Figaro's GEN_MEDFLT routine, with arguments
*         CALL GEN_MEDFLT( IMAGE( 1, I ), NX, 1, BOX, 1, WORK, IFILT )
*         CALL GEN_MEDFLT( OUTPUT, NX, 1, BOX, 1, WORK, OFILT )
*     The kaplibs routine has a slightly different way of specifying the
*     size of the box, but since gen_medflt actually used a box with a
*     width rounded down to the next odd number, the result is the same
*     whether BOX is even or odd.
         CALL KPG1_BMEDR( .FALSE., .FALSE., .FALSE.,
     :        NX, 1, IMAGE(1,I), BOX/2, 0, 1,
     :        IFILT, BADOUT, WORK, WORKI, STATUS )
         CALL KPG1_BMEDR( .FALSE., .FALSE., .FALSE.,
     :        NX, 1, OUTPUT, BOX/2, 0, 1,
     :        OFILT, BADOUT, WORK, WORKI, STATUS )
         DO J = 1,NX
            IWEIGHT = MAX(IFILT(J),0.0)
            OWEIGHT = MAX(OFILT(J),0.0)
            IF (IWEIGHT.GT.CUTOFF*OWEIGHT) THEN
               OUTPUT(J) = IMAGE(J,I)
               err_OUTPUT(J) = err_IMAGE(J,I)
            ELSE IF (OWEIGHT.GT.CUTOFF*IWEIGHT) THEN
               CONTINUE
            ELSE IF (IWEIGHT+OWEIGHT.LT.1E-6) THEN
               CONTINUE
            ELSE
               OUTPUT(J) = (IWEIGHT*IMAGE(J,I) + OWEIGHT*OUTPUT(J)) /
     :                                (IWEIGHT + OWEIGHT)
               err_OUTPUT(J) = (IWEIGHT*err_IMAGE(J,I) +
     :                                  OWEIGHT*err_OUTPUT(J)) /
     :                                (IWEIGHT + OWEIGHT)
            ENDIF
         ENDDO
      ENDDO

      END


      SUBROUTINE COPY_SCRNCD( nx_rebin, lowlim, hilim,
     :           no_of_bins, n_orders, iord,
     :           wavelength_scale, scrnchd_scale,
     :           rebin_work, rebinned_spec )

      IMPLICIT NONE

      INTEGER nx_rebin
      INTEGER lowlim
      INTEGER hilim
      INTEGER no_of_bins
      INTEGER n_orders
      INTEGER iord
      INTEGER index

      DOUBLE PRECISION wavelength_scale( no_of_bins )
      DOUBLE PRECISION scrnchd_scale( nx_rebin, n_orders )

      REAL rebin_work( no_of_bins )
      REAL rebinned_spec( nx_rebin, n_orders )

*  Re-initialise output arrays.
      DO index = 1, nx_rebin
         scrnchd_scale( index, iord ) = 0.0
         rebinned_spec( index, iord ) = 0.0
      END DO

*     Loop thru limits of wavelength scale for this order
      DO index = lowlim, hilim

*     Copy scrunched data/scale into output arrays
         scrnchd_scale( index - lowlim + 1, iord ) =
     :         wavelength_scale( index )
         rebinned_spec( index - lowlim + 1, iord ) =
     :         rebin_work( index )
      END DO

      END

      SUBROUTINE CLIPENDS( min_xgood, max_xgood, spectra,
     :           nx, n_orders )

      IMPLICIT NONE

      INTEGER nx, i, j
      INTEGER n_orders
      INTEGER min_xgood
      INTEGER max_xgood
      REAL spectra( nx, n_orders )

      DO i = 1, nx
         DO j = 1, n_orders
            IF ( min_xgood .GT. 0 .AND. min_xgood .LT. i ) THEN
               spectra ( i,j ) = 0.0
            ENDIF
            IF ( max_xgood .GT. 0 .AND. nx-i .LT. max_xgood ) THEN
               spectra ( i,j ) = 0.0
            ENDIF
         END DO
      END DO

      END


      SUBROUTINE ECH_MERGE_SPECTRA( nx_rebin, n_orders, nobj, weighting,
     :           reb_specs, reb_vars, rebinned_spectrum,
     :           rebinned_variance )

      IMPLICIT NONE

      INCLUDE 'ECH_REPORT.INC'

      INTEGER max_frames
      PARAMETER ( max_frames = 500 )


      INTEGER nx_rebin,i,j,iobj
      INTEGER n_orders
      INTEGER nobj
      CHARACTER*( * ) weighting
      REAL weights( max_frames )
      REAL reb_specs( nx_rebin, n_orders, nobj )
      REAL reb_vars( nx_rebin, n_orders, nobj )
      REAL rebinned_spectrum( nx_rebin, n_orders )
      REAL rebinned_variance( nx_rebin, n_orders )
      REAL sum

      REPORT_STRING = ' Weighting set to ' // WEIGHTING
      CALL ECH_REPORT( 0, REPORT_STRING )
      sum = 0.0
      DO iobj = 1, nobj
         weights ( iobj ) = 0.0
         DO i = 1, n_orders
            DO j = 1, nx_rebin
               weights(iobj) = weights(iobj) + ABS(reb_specs(j,i,iobj))/
     :                      SQRT(MAX(1.,ABS(reb_vars(j,i,iobj))) )
            END DO
         END DO
         weights(iobj) = weights(iobj) / FLOAT(nx_rebin*n_orders)
         sum = sum + weights(iobj)
      END DO

      DO iobj = 1, nobj
         weights(iobj) = weights(iobj) / sum
         WRITE ( report_string, 1000 ) iobj,
     :                   '(from S/N)',weights(iobj)
         CALL ECH_REPORT ( 0, report_string )
      END DO

      CALL ECH_REPORT ( 0,' Merging weighted rebinned spectra' )

      DO iobj = 1, nobj
         DO i = 1, n_orders
            DO j = 1, nx_rebin
               rebinned_spectrum(j,i) = rebinned_spectrum(j,i) +
     :                 reb_specs(j,i,iobj) * weights(iobj)
               rebinned_variance(j,i) = rebinned_variance(j,i) +
     :                 reb_vars(j,i,iobj) * weights(iobj)
            END DO
         END DO
      END DO

 1000 FORMAT ( 1X, 'Frame ',I4,' weight ',A,' is ',F12.4)

      END
