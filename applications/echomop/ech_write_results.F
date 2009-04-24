#if HAVE_CONFIG_H
# include "config.h"
#endif
      SUBROUTINE ECH_WRITE_RESULTS( ORDER_NUMBER, RESULT_TYPE,
     :           RESULT_FORMAT, TUNE_ARCHIVE, TUNE_USEAAA, TUNE_AAACODE,
     :           STATUS )
*+
*  Name:
*     ECHOMOP - ECH_WRITE_RESULTS

*  Purpose:
*     Writes out results files.

*  Invocation:
*     CALL ECH_WRITE_RESULTS( ORDER_NUMBER, RESULT_TYPE, RESULT_FORMAT,
*    :     TUNE_ARCHIVE, TUNE_USEAAA, TUNE_AAACODE, STATUS )

*  Arguments:
*     ORDER_NUMBER = INTEGER (Given)
*        Number of order in case of single order mode.
*     RESULT_TYPE = CHAR (Given)
*        Type of result array to output.
*     RESULT_FORMAT = CHAR (Given)
*        Format of output STACK/NDF/ASCII.
*     TUNE_ARCHIVE = LOGICAL (Given)
*        TRUE when we can archive the results.
*     TUNE_USEAAA = LOGICAL (Given)
*        TRUE if to use the AAA object typing scheme.
*     TUNE_AAACODE = INTEGER (Given)
*        AAA code number for object type.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     17-APR-1997 (MJC):
*       Reworking to support DIPSO Stacks and correct some bugs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_IMG_NAMES.INC'

*  Arguments Given:
      CHARACTER*( * ) RESULT_TYPE
      CHARACTER*( * ) RESULT_FORMAT

      LOGICAL TUNE_ARCHIVE
      LOGICAL TUNE_USEAAA

      INTEGER TUNE_AAACODE
      INTEGER ORDER_NUMBER

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL REQ_ORDER
      REAL DDAY

      INTEGER DUMDIM( 7 )
      INTEGER LOCADDR_XDIMEN
      INTEGER LOCADDR_YDIMEN
      INTEGER LOCADDR_XDATA
      INTEGER LOCADDR_YDATA
      INTEGER LOCADDR_ZDATA
      INTEGER LOCADDR_ERRORS
      INTEGER IAM1( 2 )
      INTEGER ODLEN
      INTEGER YY
      INTEGER DD
      INTEGER MM
      INTEGER HOUR
      INTEGER MIN
      INTEGER SEC
      INTEGER YEAR
      INTEGER DAY_O_YEAR
      INTEGER IOSTAT

      CHARACTER*80 TYPE_OF_SPECTRUM
      CHARACTER*16 WORK_STRING
      CHARACTER*16 WORK_STRING1
      CHARACTER*10 OBS_DATE
      CHARACTER*10 OBS_TIME

      LOGICAL DO_XAXIS
      LOGICAL DO_YAXIS
      LOGICAL DO_HELIO

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER CHR_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      STATUS = 0
      REQ_ORDER = FLOAT( ORDER_NUMBER )
      CALL ECH_SET_PARAMETER( 'IDX_NUM_ORDERS', 'INT', REQ_ORDER, 0,
     :     ' ', STATUS )

*  Determine type of output data required
*  ======================================

*  Extracted orders to DIPSO stack.
      IF ( RESULT_FORMAT .EQ. 'STACK' .OR.
     :     RESULT_FORMAT .EQ. 'STK' ) THEN
         WORK_STRING1 = 'DUMMY_DIPSTK'
         WORK_STRING = 'DUMMY_STK'
         CALL ECH_MODULE_INIT( WORK_STRING1, STATUS )
         LOCADDR_XDIMEN = IADDR_NX_PIXELS
         LOCADDR_YDIMEN = IADDR_NO_OF_ORDERS
         LOCADDR_XDATA = IADDR_FITTED_WAVES
         LOCADDR_YDATA = IADDR_WORDER_NUMS

*  Extracted orders.
      ELSE IF ( RESULT_TYPE( : 3 ) .EQ. 'EXT' ) THEN
         WORK_STRING1 = 'DUMMY_EXTRES'
         WORK_STRING = 'DUMMY_EXT'
         CALL ECH_MODULE_INIT( WORK_STRING1, STATUS )
         LOCADDR_XDIMEN = IADDR_NX_PIXELS
         LOCADDR_YDIMEN = IADDR_NO_OF_ORDERS
         LOCADDR_XDATA = IADDR_FITTED_WAVES
         LOCADDR_YDATA = IADDR_WORDER_NUMS
         DO_XAXIS = .TRUE.
         DO_YAXIS = .TRUE.

*  Scrunched orders.
      ELSE IF ( RESULT_TYPE( : 3 ) .EQ. 'SCR' ) THEN
         WORK_STRING1 = 'DUMMY_SCRRES'
         WORK_STRING = 'DUMMY_SCR'
         CALL ECH_MODULE_INIT( WORK_STRING1, STATUS )
         LOCADDR_XDIMEN = IADDR_NX_REBIN
         LOCADDR_YDIMEN = IADDR_NO_OF_ORDERS
         LOCADDR_XDATA = IADDR_SCRNCHD_WAVES
         LOCADDR_YDATA = IADDR_WORDER_NUMS
         DO_XAXIS = .TRUE.
         DO_YAXIS = .TRUE.

*  Merged spectrum.
      ELSE IF ( RESULT_TYPE( 2 : 6 ) .EQ. 'SPECT' ) THEN
         WORK_STRING1 = 'DUMMY_ESPECT'
         CALL ECH_MODULE_INIT( WORK_STRING1, STATUS )
         LOCADDR_XDIMEN = IADDR_NO_OF_BINS
         LOCADDR_XDATA = IADDR_WAVELENGTH
         IAM1( 1 ) = 1
#if HAVE_PERCENTLOC
         LOCADDR_YDIMEN = %LOC( IAM1 )
#elif HAVE_INTRINSIC_LOC
         LOCADDR_YDIMEN = LOC( IAM1 )
#else
          Do not have LOC or %LOC
#endif
         DO_XAXIS = .TRUE.
         DO_YAXIS = .FALSE.

*  Unknown result type.
      ELSE
         CALL ECH_REPORT( 0, ' Unknown result type requested.')
         REPORT_STRING = ' Valid types are: EXTOBJ, EXTARC, ' //
     :         'SCROBJ, SCRARC, OSPECT, CSPECT.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         GO TO 999
      END IF

*  Check status of map of objects for this output type.
      IF ( ECH_FATAL_ERROR( STATUS ) ) THEN
         GO TO 999
      END IF


*  Determine which data are to be saved
*  ====================================

*  Save object orders.
      IF ( RESULT_TYPE( 4 : 6 ) .EQ. 'OBJ' .AND.
     :     RESULT_TYPE( : 3 ) .NE. 'SCR' ) THEN
         WORK_STRING = WORK_STRING( : 9 ) // 'OBJ'
         CALL ECH_MODULE_INIT( WORK_STRING, STATUS )
         TYPE_OF_SPECTRUM = 'Extracted object order(s)'
         LOCADDR_ZDATA = IADDR_EXTRACTED_OBJ
         LOCADDR_ERRORS = IADDR_EXTR_OBJ_VAR

*  Save arc orders.
      ELSE IF ( RESULT_TYPE( 4 : 6 ) .EQ. 'ARC' .AND.
     :          RESULT_TYPE( : 3 ) .NE. 'SCR' ) THEN
         WORK_STRING = WORK_STRING( : 9 ) // 'ARC'
         CALL ECH_MODULE_INIT( WORK_STRING, STATUS )
         TYPE_OF_SPECTRUM = 'Extracted arc order(s)'
         LOCADDR_ZDATA = IADDR_EXTRACTED_ARC
         LOCADDR_ERRORS = IADDR_EXTR_ARC_VAR

*  Save calibration spectrum oredrs.
      ELSE IF ( RESULT_TYPE( 4 : 6 ) .EQ. 'CAL' .AND.
     :          RESULT_TYPE( : 3 ) .NE. 'SCR' ) THEN
         WORK_STRING = WORK_STRING( : 9 ) // 'CAL'
         CALL ECH_MODULE_INIT( WORK_STRING, STATUS )
         TYPE_OF_SPECTRUM = 'Extracted flux calibration order(s)'
         LOCADDR_ZDATA = IADDR_EXTRACTED_STAR
         LOCADDR_ERRORS = IADDR_EXTR_STAR_VAR

*  Save scrunched object orders.
      ELSE IF ( RESULT_TYPE .EQ. 'SCROBJ' ) THEN
         WORK_STRING = 'DUMMY_SRCOBJ'
         CALL ECH_MODULE_INIT( WORK_STRING, STATUS )
         TYPE_OF_SPECTRUM = 'Scrunched object order(s)'
         LOCADDR_ZDATA = IADDR_SCRNCHD_OBJ
         LOCADDR_ERRORS = IADDR_SCRNCHD_OBJV
         LOCADDR_XDATA = IADDR_SCRNCHD_WAVES
         LOCADDR_YDATA = IADDR_WORDER_NUMS

*  Save scrunched arc orders.
      ELSE IF ( RESULT_TYPE .EQ. 'SCRARC' ) THEN
         WORK_STRING = 'DUMMY_SCRARC'
         CALL ECH_MODULE_INIT( WORK_STRING, STATUS )
         TYPE_OF_SPECTRUM = 'Scrunched arc order(s)'
         LOCADDR_ZDATA = IADDR_SCRNCHD_ARC
         LOCADDR_ERRORS = IADDR_SCRNCHD_ARCV
         LOCADDR_XDATA = IADDR_SCRNCHD_WAVES
         LOCADDR_YDATA = IADDR_WORDER_NUMS

*  Save scrunched calibration orders.
      ELSE IF ( RESULT_TYPE .EQ. 'SCRCAL' ) THEN
         WORK_STRING = 'DUMMY_SCRCAL'
         CALL ECH_MODULE_INIT( WORK_STRING, STATUS )
         TYPE_OF_SPECTRUM = 'Scrunched flux calibration order(s)'
         LOCADDR_ZDATA = IADDR_SCRNCHD_STAR
         LOCADDR_ERRORS = IADDR_SCRNCHD_STRV
         LOCADDR_XDATA = IADDR_SCRNCHD_WAVES
         LOCADDR_YDATA = IADDR_WORDER_NUMS

*  Save scrunched object spectrum.
      ELSE IF ( RESULT_TYPE .EQ. 'OSPECT' ) THEN
         WORK_STRING = 'DUMMY_OSPECT'
         CALL ECH_MODULE_INIT( WORK_STRING, STATUS )
         TYPE_OF_SPECTRUM = 'Scrunched merged object spectrum'
         LOCADDR_ZDATA = IADDR_SPECTRUM
         LOCADDR_ERRORS = IADDR_ERR_SPECTRUM

*  Save scrunched calibration spectrum.
      ELSE IF ( RESULT_TYPE .EQ. 'CSPECT' ) THEN
         WORK_STRING = 'DUMMY_CSPECT'
         CALL ECH_MODULE_INIT( WORK_STRING, STATUS )
         TYPE_OF_SPECTRUM = 'Scrunched merged flux calibration spectrum'
         LOCADDR_ZDATA = IADDR_FSPECTRUM
         LOCADDR_ERRORS = IADDR_ERR_FSPECTRUM
      END IF

*  Check status of map of data for this type of output object.
      IF ( ECH_FATAL_ERROR( STATUS ) ) THEN
         GO TO 999
      END IF


*  Save extracted object orders in DIPSO stack
*  ===========================================
      IF ( RESULT_FORMAT .EQ. 'STACK' .OR.
     :     RESULT_FORMAT .EQ. 'STK' .OR.
     :     RESULT_TYPE( : 3 ) .EQ. 'STK' ) THEN
         CALL ECH_SAVE_DIPSTK(
     :        OUT_STACK,
     :        %VAL( LOCADDR_XDIMEN ),
     :        %VAL( LOCADDR_YDIMEN ),
     :        %VAL( LOCADDR_XDATA ),
     :        %VAL( LOCADDR_ZDATA ),
     :        TYPE_OF_SPECTRUM,
     :        USR_TUNE_AIRTOVAC,
     :        STATUS )
         CALL ECH_MODULE_TIDYUP( WORK_STRING, STATUS )
         CALL ECH_MODULE_TIDYUP( WORK_STRING1, STATUS )

*     Cancel the STACK parameter.
         CALL ECH_SET_PARAMETER( 'STACK', 'CANCEL', 0.0, 0, ' ',
     :     STATUS )
         GO TO 999
      END IF


*  Other types of save
*  ===================

*  Informational message.
      REPORT_STRING = ' Saving ' //
     :      TYPE_OF_SPECTRUM( :CHR_LEN( TYPE_OF_SPECTRUM ) ) //
     :      '.'
      CALL ECH_REPORT( 0, REPORT_STRING )

*  Copy the data to output areas.
      CALL ECH_SAVE_ORDERS(
     :     %VAL( LOCADDR_XDIMEN ), %VAL( LOCADDR_YDIMEN ),
     :     TYPE_OF_SPECTRUM, DO_XAXIS, DO_YAXIS,
     :     %VAL( LOCADDR_XDATA ), %VAL( LOCADDR_YDATA ),
     :     %VAL( LOCADDR_ZDATA ), %VAL( LOCADDR_ERRORS ),
     :     CSTR_RESULT_LABELX, CSTR_RESULT_LABELY,
     :     CSTR_RESULT_LABELZ, %VAL( IADDR_RESULT_DATAX ),
     :     %VAL( IADDR_RESULT_DATAY ), %VAL( IADDR_RESULT_DATAZ ),
     :     %VAL( IADDR_RESULT_ERRORS ), STATUS )

*  Make adjustments to wavelength scale.
      IF ( DO_XAXIS ) THEN
         CALL ECH_ACCESS_OBJECT( 'ECH_RDCTN.OBS_DATE',
     :        'READ', 'CHAR', ODLEN, 0, 0, DUMDIM, 7, 0,
     :        OBS_DATE, STATUS )
         DO_HELIO = .FALSE.
         IF ( STATUS .EQ. 0 ) THEN
            CALL ECH_ACCESS_OBJECT( 'ECH_RDCTN.OBS_UTTIME',
     :           'READ', 'CHAR', ODLEN, 0, 0, DUMDIM, 7, 0,
     :           OBS_TIME, STATUS )
            IF ( STATUS .EQ. 0 ) THEN
               READ ( OBS_DATE, '( I2, 1X, I2, 1X, I2 )',
     :              IOSTAT = IOSTAT ) DD, MM, YY
               IF ( IOSTAT .EQ. 0 )
     :            READ ( OBS_TIME, '( I2, 1X, I2, 1X, I2 )',
     :                 IOSTAT = IOSTAT ) HOUR, MIN, SEC
               IF ( IOSTAT .EQ. 0 ) THEN
                  CALL SLA_CALYD( YY, MM, DD, YEAR, DAY_O_YEAR,
     :                 STATUS )
                  CALL SLA_DTF2D( HOUR, MIN, SEC, DDAY, STATUS )
                  CALL SLA_EARTH( YEAR, DAY_O_YEAR, DDAY,
     :                 USR_HELIO_COR )
               END IF
               IF ( STATUS .EQ. 0 .AND. IOSTAT .EQ. 0 ) THEN
                  WRITE ( REPORT_STRING, 1002 )
     :                  OBS_DATE, OBS_TIME, USR_HELIO_COR
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  DO_HELIO = .TRUE.
               END IF
            END IF

         ELSE
            CALL ECH_REPORT( 0,
     :           ' No observation date/time in reduction file.' )
            WRITE ( REPORT_STRING, 1001 )
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
         IF ( USR_TUNE_AIRTOVAC .OR. DO_HELIO ) THEN
            IF ( USR_TUNE_AIRTOVAC ) THEN
               REPORT_STRING = ' Applying air-to-vacuum ' //
     :               'conversion to wavelength scale'
               CALL ECH_REPORT( 0, REPORT_STRING )
            END IF
            IF ( DO_HELIO ) THEN
               REPORT_STRING = ' Applying Heliocentric ' //
     :               'correction to wavelength scale'
               CALL ECH_REPORT( 0, REPORT_STRING )
            END IF
            CALL VCHLCON( %VAL( IADDR_RESULT_DATAX ),
     :           %VAL( LOCADDR_XDIMEN ), USR_HELIO_COR,
     :           DO_HELIO, USR_TUNE_AIRTOVAC )
         END IF
      END IF

*  Make ASCII dump if requested.
      IF ( RESULT_FORMAT .EQ. 'ASCII' ) THEN
         CALL ECH_MODULE_INIT( 'ASCII', STATUS )
         CALL ECH_SAVE_ASCII(
     :        OUT_FILE,
     :        %VAL( LOCADDR_XDIMEN ), %VAL( LOCADDR_YDIMEN ),
     :        DO_XAXIS, DO_YAXIS,
     :        %VAL( LOCADDR_ZDATA ), %VAL( LOCADDR_ERRORS ),
     :        %VAL( IADDR_RESULT_DATAX ),
     :        %VAL( IADDR_RESULT_DATAY ),
     :        STATUS )
         CALL ECH_MODULE_TIDYUP( 'ASCII', STATUS )
      END IF

*  Unmap the output data.
      CALL ECH_MODULE_TIDYUP( WORK_STRING, STATUS )
      CALL ECH_MODULE_TIDYUP( WORK_STRING1, STATUS )

*  Cancel the output file paramater.
      CALL ECH_SET_PARAMETER( 'ECH_RDUCD', 'CANCEL', 0.0, 0, ' ',
     :     STATUS )

*  Handle archive requests if archivable type of results file
      IF ( TUNE_ARCHIVE ) THEN
         IF ( RESULT_TYPE .EQ. 'OSPECT' .OR.
     :        RESULT_TYPE .EQ. 'EXTOBJ' .OR.
     :        RESULT_TYPE .EQ. 'OBJEXT' .OR.
     :        RESULT_TYPE .EQ. 'OBJSCR' .OR.
     :        RESULT_TYPE .EQ. 'SCROBJ' ) THEN
             CALL ECH_PACK_RESULTS( TUNE_USEAAA,
     :            TUNE_AAACODE, STATUS )
         END IF
      END IF

*  Jump to here if an error occurs.
  999 CONTINUE

 1001 FORMAT ( 1X, 'Frame epoch: Unknown   Unknown   ',
     :       ' No Heliocentric correction.' )
 1002 FORMAT ( 1X, 'Frame epoch: ', A, A,
     :       ' Heliocentric correction = ', F12.4, ' Km/s.' )

      END


      SUBROUTINE VCHLCON( ARRAY, NX, VEL, VELLG, VACLG )

*+
*  Description:
*     This routine does the calculation of the new wavelength scale,
*     with conversion for one or both of air to vacuum and correction
*     for recession velocity VEL.
*     ARRAY is the (data mapped) output array, which also holds the
*     input data when entering this routine.
*     NX is the size of the X data (wavelength array).
*     VEL is the recession velocity in Km/s which is to be corrected
*     for (negative if approach velocity).
*     VELLG = .TRUE. if velocity correction is to be done
*     VACLG = .TRUE. if air to vacuum correction is to be done

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments:
      INTEGER NX
      REAL ARRAY( NX )
      REAL VEL
      LOGICAL VELLG
      LOGICAL VACLG

*  Local Constants:
      DOUBLE PRECISION C          ! Velocity of light
      PARAMETER ( C = 2.99792D+5 )

*  Local Variables:
      DOUBLE PRECISION SUM
      DOUBLE PRECISION REFIND

      REAL WAVM
      REAL VFACT

      INTEGER I

*.

*  Do air-to-vacuum conversion if requested.
      IF ( VACLG ) THEN
         DO I = 1, NX
            WAVM = REAL( 1.0D0 / ( ARRAY( I ) * 1.0D-4 ) ** 2 )
            SUM = 64.328 + 29498.1 / ( 146.0 - WAVM ) + 255.4 /
     :            ( 41.0 - WAVM )
            REFIND = SUM * 1.0D-6 + 1.0
            ARRAY( I ) = ARRAY( I ) * REFIND
         END DO
      END IF

*  Do velocity conversion if requested.
      IF ( VELLG ) THEN
         VFACT = REAL( SQRT( ( 1.0D0 - DBLE( VEL ) / C ) /
     :         ( 1.0 + DBLE( VEL ) / C ) ) )
         DO I = 1, NX
            ARRAY( I ) = ARRAY( I ) * VFACT
         END DO
      END IF

      END

      SUBROUTINE VCHLCON_DBLE( ARRAY, NX, VEL, VELLG, VACLG )

*+
*  Description:
*     This routine does the calculation of the new wavelength scale,
*     with conversion for one or both of air to vacuum and correction
*     for recession velocity VEL.
*     ARRAY is the (data mapped) output array, which also holds the
*     input data when entering this routine.
*     NX is the size of the X data (wavelength array).
*     VEL is the recession velocity in Km/s which is to be corrected
*     for (negative if approach velocity).
*     VELLG = .TRUE. if velocity correction is to be done
*     VACLG = .TRUE. if air to vacuum correction is to be done

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments:
      INTEGER NX
      DOUBLE PRECISION ARRAY( NX )
      REAL VEL
      LOGICAL VELLG
      LOGICAL VACLG

*  Local Constants:
      DOUBLE PRECISION C          ! Velocity of light
      PARAMETER ( C = 2.99792D+5 )

*  Local Variables:
      DOUBLE PRECISION SUM
      DOUBLE PRECISION REFIND

      REAL VFACT
      REAL WAVM

      INTEGER I

*.

*  Do air-to-vacuum conversion if requested.
      IF ( VACLG ) THEN
         DO I = 1, NX
            WAVM = REAL( 1.0D0 / ( ARRAY( I ) * 1.0D-4 ) ** 2 )
            SUM = 64.328 + 29498.1 / ( 146.0 - WAVM ) + 255.4 /
     :            ( 41.0 - WAVM )
            REFIND = SUM * 1.0D-6 + 1.0
            ARRAY( I ) = ARRAY( I ) * REFIND
         END DO
      END IF

*  Do velocity conversion if requested.
      IF ( VELLG ) THEN
         VFACT = REAL( SQRT( ( 1.0D0 - DBLE( VEL ) / C ) /
     :         ( 1.0 + DBLE( VEL ) / C ) ) )
         DO I = 1, NX
            ARRAY( I ) = ARRAY( I ) * VFACT
         END DO
      END IF

      END
