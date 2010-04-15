      SUBROUTINE ECH_ECHOMOP_OPTION( INP_MODULE_NAME, ORDER_NUMBER,
     :           STATUS )
*+
*  Name:
*     ECHOMOP - ECH_ECHOMOP_OPTION

*  Purpose:
*     Handles task dispatch for monolith.

*  Invocation:
*     CALL ECH_ECHOMOP_OPTION( INP_MODULE_NAME, ORDER_NUMBER, STATUS )

*  Arguments:
*     INP_MODULE_NAME = INTEGER (Given)
*        Name of module required.
*     ORDER_NUMBER = INTEGER (Given)
*        Order number or zero for all orders.
*     STATUS = INTEGER (Given and Returned)
*        Error/Report status.

*  Method:
*     If order(s) to be processed one by one then
*      if needs multiple arc frames (potentially) then
*       Set 'num_orders' indexing to zero, which denotes whole array mapping
*     Else if needs multiple arc frames (potentially) then
*     Else must be an option which needs all order info at once
*     Endif

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
      INCLUDE 'ECH_INIT_RDCTN.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_SERVER.INC'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_ECHOMOP.INC'

*  Arguments Given:
      INTEGER ORDER_NUMBER
      CHARACTER*( ECH__MNMSIZ ) INP_MODULE_NAME

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAX_FTRS
      PARAMETER ( MAX_FTRS = 5000 )

*  Local Variables:
      REAL FEATURES( MAX_FTRS )
      REAL STRENGTHS( MAX_FTRS )
      REAL DUMMY

      INTEGER ISF
      INTEGER IORD
      INTEGER IFRM
      INTEGER NCHAR
      INTEGER START_ORDER
      INTEGER END_ORDER
      INTEGER START_FRAME
      INTEGER END_FRAME

      LOGICAL LDUMMY
      LOGICAL NO_ERRORS
      LOGICAL MENU

      CHARACTER*( ECH__MNMSIZ ) OPTION_MODULE_NAME
      CHARACTER*80 FTRDB_SOURCE
      CHARACTER*4 REF_STR

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER ECH_MODULE_NAME_INDEX
      INTEGER ECH_DEREF_INT
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

* If order(s) are to be processed one-by-one.
      option_module_name = inp_module_name
      IF ( once_per_order(
     :     ECH_MODULE_NAME_INDEX( option_module_name ) ) ) THEN

* If needs multiple arc frames (potentially) then.
       IF ( OPTION_MODULE_NAME .EQ. 'ECH_EXT_OPT' .OR.
     :      OPTION_MODULE_NAME .EQ. 'ECH_REBIN_ARCORDER' ) THEN
          CALL ECH_SET_PARAMETER( 'IDX_NREF_FRAME', 'INT',
     :         0., 0, ' ', STATUS  )
          CALL ECH_MODULE_INIT( 'ECH_DUMMY2', status )
          START_FRAME = 1
          END_FRAME = ECH_DEREF_INT( %VAL( IADDR_NREF_FRAME ) )
          DO IFRM = START_FRAME, END_FRAME
             CALL ECH_SET_PARAMETER( 'IDX_NREF_FRAME', 'INT',
     :            FLOAT( IFRM ), 0, ' ', STATUS )
             IF ( .NOT. ECH_FATAL_ERROR( status ) ) THEN

*           Set 'num_orders' indexing to zero, which denotes whole array mapping
            CALL ECH_SET_PARAMETER( 'IDX_NUM_ORDERS', 'INT',
     :           0.0, 0, ' ', status )
            CALL ECH_MODULE_INIT( 'ECH_DUMMY', status )
            IF ( .NOT. ECH_FATAL_ERROR ( status ) ) THEN
             CALL CHR_ITOC( IFRM, REF_STR, NCHAR )
             WRITE ( report_string, 1002 )
     :             REF_STR( :NCHAR )
             CALL ECH_REPORT( 0, report_string )
             IF ( ORDER_NUMBER .EQ. 0 ) THEN
                START_ORDER = 1
                END_ORDER = ECH_DEREF_INT( %VAL(IADDR_NO_OF_ORDERS) )

             ELSE
                START_ORDER = ORDER_NUMBER
                END_ORDER = ORDER_NUMBER
             END IF
             DO IORD = START_ORDER, END_ORDER
               CALL ECH_SET_PARAMETER( 'IDX_NUM_ORDERS', 'INT',
     :              FLOAT( IORD ), 0, ' ', STATUS )
               CALL ECH_MODULE_INIT( OPTION_MODULE_NAME, STATUS )
               IF ( .NOT. ECH_FATAL_ERROR ( status ) ) THEN
                  IF ( usr_tune_clone .EQ. 'NULL' ) THEN
                    CALL CHR_ITOC( IORD, REF_STR, NCHAR )
                    WRITE ( report_string, 1001 )
     :                    REF_STR( :NCHAR )
                    CALL ECH_REPORT( 0, report_string )
                   ENDIF
                   IF ( usr_tune_clone .NE. 'NULL' ) THEN
                      CALL ECH_MODULE_TIDYUP(
     :                     option_module_name, status )
                      GO TO 300

                   ELSE IF ( option_module_name .EQ.
     :                       'ECH_EXT_OPT' ) THEN
                    no_errors = .FALSE.
                    IF ( iaddr_inptime .EQ. 0 ) no_errors = .TRUE.
                    CALL ECH_EXT_OPT(
     :                   %VAL( iaddr_inptim ),
     :                   %VAL( iaddr_inptimq ),
     :                   %VAL( iaddr_inptime ),
     :                   usr_tune_noarc,
     :                   usr_tune_noflat,
     :                   no_errors,
     :                   %VAL( iaddr_arc ),
     :                   %VAL( iaddr_nx_pixels ),
     :                   %VAL( iaddr_ny_pixels ),
     :                   %VAL( iaddr_no_of_orders ),
     :                   usr_extract_mode,
     :                   usr_tune_skvrcorr,
     :                   usr_tune_crclean,
     :                   usr_tune_mxskypix,
     :                   %VAL( iaddr_sky_mask ),
     :                   %VAL( iaddr_obj_mask ),
     :                   %VAL( iaddr_dek_below ),
     :                   %VAL( iaddr_dek_above ),
     :                   usr_tune_skypoly,
     :                   usr_tune_skyrej,
     :                   usr_tune_skyrthr,
     :                   %VAL( iaddr_readout_noise ),
     :                   %VAL( iaddr_photon_to_adu ),
     :                   usr_tune_objpoly,
     :                   usr_tune_maxpoly,
     :                   %VAL( iaddr_trc_poly ),
     :                   usr_tune_pflssamp,
     :                   %VAL( iaddr_model_profile ),
     :                   %VAL( iaddr_fitted_pfl ),
     :                   %VAL( iaddr_extracted_obj ),
     :                   %VAL( iaddr_extr_obj_var ),
     :                   %VAL( iaddr_extracted_arc ),
     :                   %VAL( iaddr_extr_arc_var ),
     :                   %VAL( iaddr_x_trace_coord ),
     :                   %VAL( iaddr_y_trace_coord ),
     :                   %VAL( iaddr_fitted_sky ),
     :                   %VAL( iaddr_fsky_errors ),
     :                   %VAL( iaddr_var_matrix ),
     :                   %VAL( iaddr_fitted_flat ),
     :                   %VAL( iaddr_flat_errors ),
     :                   status
     :                  )

                   ELSE IF ( option_module_name .EQ.
     :                       'ECH_REBIN_ARCORDER' ) THEN
                    IF ( usr_tune_noarc ) THEN
                      CALL ECH_REPORT( 0,
     :                  ' Cannot process because TUNE_NOARC is set.' )

                    ELSE
                     no_errors = .FALSE.
                     IF ( iaddr_arce .EQ. 0 ) no_errors = .TRUE.
                     CALL ECH_REBIN_ARCORDER(
     :                    %VAL( iaddr_nx_pixels ),
     :                    %VAL( iaddr_ny_pixels ),
     :                    usr_tune_noflat,
     :                    no_errors,
     :                    %VAL( iaddr_arc ),
     :                    %VAL( iaddr_arce ),
     :                    %VAL( iaddr_arcq ),
     :                    %VAL( iaddr_no_of_bins ),
     :                    usr_tune_mxskypix,
     :                    %VAL( iaddr_readout_noise),
     :                    %VAL( iaddr_nx_rebin ),
     :                    usr_w2_nx_poly,
     :                    usr_w2_ny_poly,
     :                    %VAL( iaddr_w_poly_2d ),
     :                    usr_tune_maxpoly,
     :                    %VAL( iaddr_w_poly ),
     :                    %VAL( iaddr_scrnchd_waves),
     :                    %VAL( iaddr_dek_below ),
     :                    %VAL( iaddr_dek_above ),
     :                    %VAL( iaddr_trc_poly ),
     :                    %VAL( iaddr_fitted_flat ),
     :                    %VAL( iaddr_flat_errors ),
     :                    %VAL( iaddr_rebin_arc ),
     :                    %VAL( iaddr_rebin_earc ),
     :                    %VAL( iaddr_rebin_quality ),
     :                    %VAL( iaddr_x_trace_coord ),
     :                    %VAL( iaddr_y_trace_coord ),
     :                    %VAL( iaddr_fit_work_xdouble ),
     :                    %VAL( iaddr_fit_work_xdouble2 ),
     :                    %VAL( iaddr_two_d_dlambda ),
     :                    status
     :                    )
                     ENDIF
                   ENDIF
                   CALL ECH_MODULE_TIDYUP(
     :                  option_module_name, status )

                  ELSE IF ( usr_tune_client ) THEN
                     CALL ECH_MODULE_TIDYUP(
     :                    option_module_name, status )
                  ENDIF
             END DO
  300        CONTINUE
            ENDIF
          ENDIF
        END DO

       ELSE

*         Set 'num_orders' indexing to zero, which denotes whole array mapping
          CALL ECH_SET_PARAMETER( 'IDX_NUM_ORDERS', 'INT',
     :         0.0, 0, ' ', STATUS )
          CALL ECH_SET_PARAMETER( 'IDX_NREF_FRAME', 'INT',
     :         1.0, 0, ' ', STATUS  )
          CALL ECH_MODULE_INIT( 'ECH_DUMMY', STATUS )
          IF ( .NOT. ECH_FATAL_ERROR( STATUS ) ) THEN
             MENU = .TRUE.
             IF ( OPTION_MODULE_NAME .EQ.
     :            'ECH_GEN_FLAT' ) THEN
                IF ( .NOT. USR_TUNE_NOFLAT ) THEN
                   CALL ECH_SET_PARAMETER( 'IDX_NUM_ORDERS', 'INT',
     :                  1.0, 0, ' ', STATUS )
                   CALL ECH_MODULE_INIT(
     :                  OPTION_MODULE_NAME, STATUS )
                   CALL ECH_UNITY_FLAT(
     :                  %VAL( IADDR_NX_PIXELS ),
     :                  %VAL( IADDR_NY_PIXELS ),
     :                  %VAL( IADDR_RESULT_DATAZ ),
     :                  %VAL( IADDR_RESULT_ERRORS ),
     :                  STATUS
     :                  )
                   CALL ECH_MODULE_TIDYUP(
     :                  OPTION_MODULE_NAME, STATUS )

                ELSE
                   CALL ECH_REPORT( 0,
     :                  ' No flat-field output as TUNE_NOFLAT is set.' )
                ENDIF
             END IF

             IF ( ORDER_NUMBER .EQ. 0 ) THEN
                START_ORDER = 1
                END_ORDER = ECH_DEREF_INT( %VAL( IADDR_NO_OF_ORDERS) )

             ELSE
                START_ORDER = ORDER_NUMBER
                END_ORDER = ORDER_NUMBER
             END IF
             DO IORD = START_ORDER, END_ORDER
               CALL ECH_SET_PARAMETER( 'IDX_NUM_ORDERS', 'INT',
     :              FLOAT( IORD ), 0, ' ', STATUS )
                IF ( .NOT. ECH_FATAL_ERROR( STATUS ) ) THEN
                  CALL ECH_MODULE_INIT(
     :                 OPTION_MODULE_NAME, STATUS )
                  IF ( .NOT. ECH_FATAL_ERROR( STATUS ) ) THEN
                     IF ( usr_tune_clone .EQ. 'NULL' ) THEN
                        CALL CHR_ITOC( IORD, REF_STR, NCHAR )
                        WRITE ( report_string, 1001 )
     :                        REF_STR( :NCHAR )
                        CALL ECH_REPORT( 0, report_string )
                     ENDIF
                   IF ( USR_TUNE_CLONE .NE. 'NULL' ) THEN
                      CALL ECH_MODULE_TIDYUP(
     :                     OPTION_MODULE_NAME, STATUS )
                      GO TO 500

                   ELSE IF ( OPTION_MODULE_NAME .EQ.
     :                       'ECH_TRACE_ORDER' ) THEN
                    CALL ECH_TRACE_ORDER(
     :                   %VAL( iaddr_tracim ),
     :                   %VAL( iaddr_tracimq ),
     :                   %VAL( iaddr_nx_pixels ),
     :                   %VAL( iaddr_ny_pixels ),
     :                   %VAL( iaddr_no_of_orders ),
     :                   %VAL( iaddr_order_ypos ),
     :                   %VAL( iaddr_trace_width ),
     :                   iord,
     :                   %VAL( iaddr_order_slope ),
     :                   usr_trace_mode,
     :                   usr_tune_xbox,
     :                   usr_tune_mxsmp,
     :                   usr_tune_mxbadsmp,
     :                   usr_use_median,
     :                   usr_tune_maxpoly,
     :                   usr_trcfit,
     :                   usr_trc_npoly,
     :                   %VAL( iaddr_trace ),
     :                   %VAL( iaddr_trc_in_dev ),
     :                   %VAL( iaddr_trc_poly ),
     :                   %VAL( iaddr_x_trace_coord ),
     :                   %VAL( iaddr_y_trace_coord ),
     :                   status
     :                   )

                   ELSE IF ( option_module_name .EQ.
     :                       'ECH_FIT_ORDER_TRACE' ) THEN
                    CALL ECH_FIT_ORDER_TRACE(
     :                   %VAL( iaddr_trace ),
     :                   %VAL( iaddr_nx_pixels ),
     :                   iord,
     :                   usr_tune_maxpoly,
     :                   usr_trcfit,
     :                   usr_trc_npoly,
     :                   %VAL( iaddr_trc_poly ),
     :                   usr_trc_interact,
     :                   %VAL( iaddr_trc_in_dev ),
     :                   %VAL( iaddr_trc_out_dev ),
     :                   usr_tune_clpmxdev,
     :                   usr_tune_clpby,
     :                   %VAL( iaddr_trc_clipped ),
     :                   %VAL( iaddr_x_trace_coord ),
     :                   %VAL( iaddr_y_trace_coord ),
     :                   menu,
     :                   status
     :                   )

                   ELSE IF ( option_module_name .EQ.
     :                       'ECH_MODEL_FLAT' ) THEN
                    IF ( .NOT. usr_tune_noflat ) THEN
                      no_errors = .FALSE.
                      IF ( iaddr_ffielde .EQ. 0 ) no_errors = .TRUE.
                      CALL ECH_MODEL_FLAT(
     :                     %VAL( iaddr_ffield ),
     :                     %VAL( iaddr_ffieldq ),
     :                     %VAL( iaddr_ffielde ),
     :                     usr_tune_noflat,
     :                     no_errors,
     :                     %VAL( iaddr_nx_pixels ),
     :                     %VAL( iaddr_ny_pixels ),
     :                     iord,
     :                     usr_tune_mxskypix,
     :                     usr_tune_ffinter,
     :                     usr_tune_ffsubsmp,
     :                     usr_tune_prebal,
     :                     %VAL( iaddr_dek_below ),
     :                     %VAL( iaddr_dek_above ),
     :                     usr_tune_ffnxply,
     :                     usr_tune_ffnyply,
     :                     usr_tune_ffnxrej,
     :                     usr_tune_ffnyrej,
     :                     usr_tune_ffthresh,
     :                     usr_tune_fflmed,
     :                     usr_tune_fflsmp,
     :                     usr_tune_maxpoly,
     :                     usr_fltfit,
     :                     %VAL( iaddr_trc_poly ),
     :                     %VAL( iaddr_fitted_flat ),
     :                     %VAL( iaddr_flat_errors ),
     :                     %VAL( iaddr_x_trace_coord ),
     :                     %VAL( iaddr_y_trace_coord ),
     :                     %VAL( iaddr_dsample_xcoord ),
     :                     %VAL( iaddr_dsample_ycoord ),
     :                     %VAL( iaddr_fit_work_xreal ),
     :                     %VAL( iaddr_fit_work_xreal2 ),
     :                     %VAL( iaddr_fit_work_xreal3 ),
     :                     status
     :                     )

                    ELSE
                       CALL ECH_REPORT( 0,
     :         ' No flat-field modelling as TUNE_NOFLAT is set.' )
                    ENDIF

                   ELSE IF ( OPTION_MODULE_NAME .EQ.
     :                       'ECH_GEN_FLAT' ) THEN
                    IF ( .NOT. USR_TUNE_NOFLAT ) THEN
                      CALL ECH_GEN_FLAT(
     :                     %VAL( iaddr_nx_pixels ),
     :                     %VAL( iaddr_ny_pixels ),
     :                     %VAL( iaddr_result_dataz ),
     :                     %VAL( iaddr_result_errors ),
     :                     usr_tune_mxskypix,
     :                     %VAL( iaddr_dek_below ),
     :                     %VAL( iaddr_dek_above ),
     :                     usr_tune_maxpoly,
     :                     %VAL( iaddr_trc_poly ),
     :                     %VAL( iaddr_fitted_flat ),
     :                     %VAL( iaddr_flat_errors ),
     :                     %VAL( iaddr_x_trace_coord ),
     :                     %VAL( iaddr_y_trace_coord ),
     :                     status
     :                     )

                    ELSE
                       CALL ECH_REPORT( 0,
     :         ' No flat-field output as TUNE_NOFLAT is set.' )
                    ENDIF

                   ELSE IF ( option_module_name .EQ.
     :                       'ECH_MODEL_SKY' ) THEN
                    no_errors = .FALSE.
                    IF ( iaddr_inptime .EQ. 0 ) no_errors = .TRUE.
                    CALL ECH_MODEL_SKY (
     :                   %VAL( iaddr_inptim ),
     :                   %VAL( iaddr_inptimq ),
     :                   %VAL( iaddr_inptime ),
     :                   %VAL( iaddr_nx_pixels ),
     :                   %VAL( iaddr_ny_pixels ),
     :                   iord,
     :                   usr_tune_skyinter,
     :                   usr_tune_noflat,
     :                   no_errors,
     :                   %VAL( iaddr_fitted_flat ),
     :                   %VAL( iaddr_flat_errors ),
     :                   usr_tune_mxskypix,
     :                   %VAL( iaddr_sky_mask ),
     :                   %VAL( iaddr_dek_below ),
     :                   %VAL( iaddr_dek_above ),
     :                   usr_tune_skypoly,
     :                   usr_tune_skyrej,
     :                   usr_tune_skyrthr,
     :                   usr_tune_skysim,
     :                   usr_tune_skyxply,
     :                   usr_tune_skylinw,
     :                   usr_tune_skylthr,
     :                   %VAL( iaddr_readout_noise ),
     :                   %VAL( iaddr_photon_to_adu ),
     :                   usr_tune_maxpoly,
     :                   usr_skyfit,
     :                   %VAL( iaddr_trc_poly ),
     :                   %VAL( iaddr_sky_spectrum ),
     :                   %VAL( iaddr_sky_variance ),
     :                   %VAL( iaddr_fitted_sky ),
     :                   %VAL( iaddr_fsky_errors ),
     :                   %VAL( iaddr_x_trace_coord ),
     :                   %VAL( iaddr_y_trace_coord ),
     :                   %VAL( iaddr_dsample_xcoord ),
     :                   %VAL( iaddr_dsample_ycoord ),
     :                   %VAL( iaddr_diagonal ),
     :                   %VAL( iaddr_diagonal2 ),
     :                   %VAL( iaddr_smooth_diag ),
     :                   %VAL( iaddr_fit_work_xreal ),
     :                   %VAL( iaddr_fit_work_xreal2 ),
     :                   %VAL( iaddr_fit_work_xreal3 ),
     :                   %VAL( iaddr_fit_work_xreal4 ),
     :                   status
     :                   )

                   ELSE IF ( option_module_name .EQ.
     :                       'ECH_FIT_2D_DISTORTION' ) THEN
                    IF ( usr_tune_noarc ) THEN
                       CALL ECH_REPORT( 0,
     :                  ' Cannot process because TUNE_NOARC is set.' )
                    ELSE
                     CALL ECH_FIT_2D_DISTORTION(
     :                    %VAL( iaddr_nx_pixels ),
     :                    %VAL( iaddr_nx_pixels ),
     :                    %VAL( iaddr_arc ),
     :                    usr_2d_interact,
     :                    %VAL( iaddr_dek_below ),
     :                    %VAL( iaddr_dek_above ),
     :                    iord,
     :                    usr_tune_maxpoly,
     :                    %VAL( iaddr_trc_poly ),
     :                    %VAL( iaddr_w_poly ),
     :                    %VAL( iaddr_ref_line_fwhm ),
     :                    usr_tune_maxrfln,
     :                    %VAL( iaddr_id_count ),
     :                    %VAL( iaddr_id_lines ),
     :                    %VAL( iaddr_id_waves ),
     :                    usr_w2_nx_poly,
     :                    usr_w2_ny_poly,
     :                    %VAL( iaddr_w_poly_2d ),
     :                    %VAL( iaddr_x_trace_coord ),
     :                    %VAL( iaddr_y_trace_coord ),
     :                    %VAL( iaddr_f2d_dx_coord ),
     :                    %VAL( iaddr_f2d_dy_coord ),
     :                    %VAL( iaddr_f2d_f_of_xy ),
     :                    %VAL( iaddr_f2d_deviation ),
     :                    %VAL( iaddr_f2d_fitted_f ),
     :                    %VAL( iaddr_f2d_xv ),
     :                    %VAL( iaddr_f2d_weights ),
     :                    %VAL( iaddr_f2d_linecent ),
     :                    %VAL( iaddr_f2d_work_nag ),
     :                    status
     :                    )
                    ENDIF

                   ELSE IF ( option_module_name .EQ.
     :                       'ECH_REBIN_ORDER' ) THEN
                    no_errors = .FALSE.
                    IF ( iaddr_inptime .EQ. 0 ) no_errors = .TRUE.
                    CALL ECH_REBIN_ORDER(
     :                   %VAL( iaddr_nx_pixels ),
     :                   %VAL( iaddr_ny_pixels ),
     :                   usr_tune_noflat,
     :                   no_errors,
     :                   %VAL( iaddr_inptim ),
     :                   %VAL( iaddr_inptime ),
     :                   %VAL( iaddr_inptimq ),
     :                   %VAL( iaddr_no_of_bins ),
     :                   usr_tune_mxskypix,
     :                   %VAL( iaddr_readout_noise ),
     :                   %VAL( iaddr_nx_rebin ),
     :                   usr_w2_nx_poly,
     :                   usr_w2_ny_poly,
     :                   %VAL( iaddr_w_poly_2d ),
     :                   usr_tune_maxpoly,
     :                   %VAL( iaddr_w_poly ),
     :                   %VAL( iaddr_scrnchd_waves ),
     :                   %VAL( iaddr_dek_below ),
     :                   %VAL( iaddr_dek_above ),
     :                   %VAL( iaddr_trc_poly ),
     :                   %VAL( iaddr_fitted_flat ),
     :                   %VAL( iaddr_flat_errors ),
     :                   %VAL( iaddr_rebin_order ),
     :                   %VAL( iaddr_rebin_eorder ),
     :                   %VAL( iaddr_rebin_quality ),
     :                   %VAL( iaddr_x_trace_coord ),
     :                   %VAL( iaddr_y_trace_coord ),
     :                   %VAL( iaddr_fit_work_xdouble ),
     :                   %VAL( iaddr_fit_work_xdouble2 ),
     :                   %VAL( iaddr_two_d_dlambda ),
     :                   status
     :                   )

                   ELSE IF ( option_module_name .EQ.
     :                       'ECH_MODEL_SSKY' ) THEN
                    CALL ECH_MODEL_SSKY(
     :                   %VAL( iaddr_rebin_order ),
     :                   %VAL( iaddr_rebin_eorder ),
     :                   %VAL( iaddr_rebin_quality ),
     :                   %VAL( iaddr_nx_rebin ),
     :                   %VAL( iaddr_ny_pixels ),
     :                   usr_tune_mxskypix,
     :                   %VAL( iaddr_sky_mask ),
     :                   %VAL( iaddr_dek_below ),
     :                   %VAL( iaddr_dek_above ),
     :                   usr_tune_skypoly,
     :                   usr_tune_skyrej,
     :                   usr_tune_skyrthr,
     :                   usr_tune_skysim,
     :                   usr_tune_skyxply,
     :                   usr_tune_skylinw,
     :                   usr_tune_skylthr,
     :                   %VAL( iaddr_readout_noise ),
     :                   %VAL( iaddr_photon_to_adu ),
     :                   usr_tune_maxpoly,
     :                   usr_skyfit,
     :                   %VAL( iaddr_trc_poly ),
     :                   %VAL( iaddr_ssky_spectrum ),
     :                   %VAL( iaddr_ssky_variance ),
     :                   %VAL( iaddr_fitted_ssky ),
     :                   %VAL( iaddr_fssky_errors ),
     :                   %VAL( iaddr_dsample_xcoord ),
     :                   %VAL( iaddr_dsample_ycoord ),
     :                   %VAL( iaddr_diagonal ),
     :                   %VAL( iaddr_diagonal2 ),
     :                   %VAL( iaddr_smooth_diag ),
     :                   %VAL( iaddr_fit_work_xreal ),
     :                   %VAL( iaddr_fit_work_xreal2 ),
     :                   %VAL( iaddr_fit_work_xreal3 ),
     :                   status
     :                   )

                   ELSE IF ( option_module_name .EQ.
     :                       'ECH_2D_EXT_OPT' ) THEN
                    CALL ECH_2DEXT_OPT ( %VAL( iaddr_rebin_order ),
     :                   %VAL( iaddr_rebin_eorder ),
     :                   %VAL( iaddr_rebin_quality ),
     :                   usr_tune_noarc,
     :                   %VAL( iaddr_rebin_arc ),
     :                   %VAL( iaddr_nx_rebin ),
     :                   %VAL( iaddr_ny_pixels ),
     :                   %VAL( iaddr_no_of_orders ),
     :                   usr_extract_mode,
     :                   usr_tune_skvrcorr,
     :                   usr_tune_crclean,
     :                   usr_tune_mxskypix,
     :                   %VAL( iaddr_sky_mask ),
     :                   %VAL( iaddr_obj_mask ),
     :                   %VAL( iaddr_dek_below ),
     :                   %VAL( iaddr_dek_above ),
     :                   usr_tune_skypoly,
     :                   usr_tune_skyrej,
     :                   usr_tune_skyrthr,
     :                   %VAL( iaddr_readout_noise ),
     :                   %VAL( iaddr_photon_to_adu ),
     :                   usr_tune_objpoly,
     :                   usr_tune_maxpoly,
     :                   %VAL( iaddr_trc_poly ),
     :                   usr_tune_pflssamp,
     :                   %VAL( iaddr_model_profile ),
     :                   %VAL( iaddr_fitted_pfl ),
     :                   %VAL( iaddr_scrnchd_obj ),
     :                   %VAL( iaddr_scrnchd_objv ),
     :                   %VAL( iaddr_scrnchd_arc ),
     :                   %VAL( iaddr_scrnchd_arcv ),
     :                   %VAL( iaddr_fitted_ssky ),
     :                   %VAL( iaddr_fssky_errors ),
     :                   %VAL( iaddr_var_matrix ),
     :                   status
     :                   )

*               Fit order blaze.
                   ELSE IF ( OPTION_MODULE_NAME .EQ.
     :                       'ECH_FIT_ORDER_BLAZE' ) THEN
                      IF ( USR_TUNE_NOFLAT ) THEN
                         CALL ECH_REPORT( 0,
     :                   ' Cannot process because TUNE_NOFLAT is set.' )

                      ELSE
                         CALL ECH_FIT_ORDER_BLAZE (
     :                        %VAL( iaddr_nx_pixels ),
     :                        %VAL( iaddr_ny_pixels ),
     :                        %VAL( iaddr_ffield ),
     :                        %VAL( iaddr_ffieldq ),
     :                        iord,
     :                        %VAL( iaddr_dek_below ),
     :                        %VAL( iaddr_dek_above ),
     :                        usr_tune_maxpoly,
     :                        usr_blzfit,
     :                        usr_blz_npoly,
     :                        %VAL( iaddr_trc_poly ),
     :                        usr_blz_interact,
     :                        %VAL( iaddr_trc_out_dev ),
     :                        %VAL( iaddr_blaze_spect ),
     :                        %VAL( iaddr_x_trace_coord ),
     :                        %VAL( iaddr_y_trace_coord ),
     :                        status
     :                        )
                      END IF

                   ELSE IF ( option_module_name .EQ.
     :                       'ECH_FLATTEN_ORDERS' ) THEN
                    CALL ECH_FLATTEN_ORDERS(
     :                   %VAL( iaddr_nx_pixels ),
     :                   usr_tune_blzrset,
     :                   usr_tune_yblaze,
     :                   %VAL( iaddr_blaze_spect ),
     :                   %VAL( iaddr_blaze_medians ),
     :                   %VAL( iaddr_extracted_obj ),
     :                   %VAL( iaddr_extr_obj_var ),
     :                   status
     :                   )

                   ELSE IF ( option_module_name .EQ.
     :                       'ECH_FIT_REF_FWHMS' ) THEN
                    CALL ECH_FIT_REF_FWHMS(
     :                   %VAL( iaddr_extracted_arc ),
     :                   %VAL( iaddr_nx_pixels ),
     :                   usr_tune_maxrfln,
     :                   usr_tune_maxpoly,
     :                   %VAL( iaddr_ref_line_fwhm ),
     :                   %VAL( iaddr_id_count ),
     :                   %VAL( iaddr_id_lines ),
     :                   %VAL( iaddr_id_waves ),
     :                   %VAL( iaddr_id_widths ),
     :                   %VAL( iaddr_wid_poly ),
     :                   status
     :                   )
                   ENDIF
                   CALL ECH_MODULE_TIDYUP(
     :                  OPTION_MODULE_NAME, STATUS )

                  ELSE IF ( usr_tune_client ) THEN
                     CALL ECH_MODULE_TIDYUP (
     :                          option_module_name, status )
                  ENDIF
                ENDIF
             END DO
          ENDIF
        ENDIF
  500 CONTINUE

*     Else if needs multiple arc frames (potentially) then
      ELSE IF ( option_module_name .EQ. 'ECH_GET_REF_FWHM' .OR.
     :          option_module_name .EQ. 'ECH_EXTR_QUICK' .OR.
     :          option_module_name .EQ. 'ECH_LOCATE_REF_LINES' .OR.
     :          option_module_name .EQ. 'ECH_GEN_REBIN_SCALE' .OR.
     :          option_module_name .EQ. 'ECH_WAVE_CONSISTENCY' .OR.
     :          option_module_name .EQ. 'ECH_WAVELENGTH_CALIB' ) THEN
        CALL ECH_SET_PARAMETER( 'IDX_NUM_ORDERS', 'INT',
     :       0.0, 0, ' ', STATUS )
        CALL ECH_SET_PARAMETER( 'IDX_NREF_FRAME', 'INT',
     :       0.0, 0, ' ', STATUS )
        CALL ECH_MODULE_INIT( 'ECH_DUMMY2', STATUS )
        IF ( .NOT. ECH_FATAL_ERROR ( STATUS ) ) THEN
        START_FRAME = 1
        END_FRAME = ECH_DEREF_INT( %VAL( IADDR_NREF_FRAME ) )
        DO IFRM = START_FRAME, END_FRAME
           CALL ECH_SET_PARAMETER( 'IDX_NREF_FRAME', 'INT',
     :          FLOAT( IFRM ), 0, ' ', STATUS )
           IF ( .NOT. ECH_FATAL_ERROR( STATUS ) ) THEN
            CALL ECH_MODULE_INIT( option_module_name, status )
            IF ( .NOT. ECH_FATAL_ERROR ( status ) ) THEN
             CALL CHR_ITOC( IFRM, REF_STR, NCHAR )
             WRITE ( report_string, 1002 )
     :             REF_STR( :NCHAR )
              CALL ECH_REPORT( 0, report_string )
              IF ( usr_tune_clone .NE. 'NULL' ) THEN
                 CONTINUE

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_GET_REF_FWHM' ) THEN
               IF ( usr_tune_noarc ) THEN
                  CALL ECH_REPORT( 0,
     :            ' Cannot process because TUNE_NOARC is set.' )
               ELSE
                CALL ECH_GET_REF_FWHM(
     :               %VAL( iaddr_arc ),
     :               %VAL( iaddr_nx_pixels ),
     :               %VAL( iaddr_ny_pixels ),
     :               %VAL( iaddr_no_of_orders ),
     :               usr_tune_maxpoly,
     :               %VAL( iaddr_trc_poly ),
     :               %VAL( iaddr_ref_line_fwhm ),
     :               %VAL( iaddr_x_trace_coord ),
     :               %VAL( iaddr_y_trace_coord ),
     :               %VAL( iaddr_avg_spectrum ),
     :               %VAL( iaddr_ref_spectrum ),
     :               status
     :               )
              ENDIF

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_LOCATE_REF_LINES' ) THEN
               IF ( usr_tune_noarc ) THEN
                  CALL ECH_REPORT( 0,
     :            ' Cannot process because TUNE_NOARC is set.' )

               ELSE
                CALL ECH_LOCATE_REF_LINES(
     :               %VAL( iaddr_nx_pixels ),
     :               %VAL( iaddr_ny_pixels ),
     :               %VAL( iaddr_no_of_orders ),
     :               usr_tune_maxpoly,
     :               %VAL( iaddr_trc_poly ),
     :               usr_tune_maxrfln,
     :               usr_tune_rflnthr,
     :               %VAL( iaddr_ref_line_fwhm ),
     :               %VAL( iaddr_obs_lines ),
     :               %VAL( iaddr_obs_inten ),
     :               %VAL( iaddr_extracted_arc ),
     :               %VAL( iaddr_x_trace_coord ),
     :               %VAL( iaddr_y_trace_coord ),
     :               %VAL( iaddr_ref_spectrum ),
     :               %VAL( iaddr_avg_spectrum ),
     :               %VAL( iaddr_ref_continuum ),
     :               %VAL( iaddr_ref_lines ),
     :               status
     :               )
               ENDIF

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_EXTR_QUICK' ) THEN
                    no_errors = .FALSE.
                    IF ( iaddr_inptime .EQ. 0 ) no_errors = .TRUE.
                    CALL ECH_EXTR_QUICK(
     :                   %VAL( iaddr_nx_pixels ),
     :                   %VAL( iaddr_ny_pixels ),
     :                   %VAL( iaddr_no_of_orders ),
     :                   usr_tune_noarc,
     :                   no_errors,
     :                   %VAL( iaddr_obj_mask ),
     :                   %VAL( iaddr_sky_mask ),
     :                   usr_tune_maxpoly,
     :                   %VAL( iaddr_trc_poly ),
     :                   %VAL( iaddr_dek_below ),
     :                   %VAL( iaddr_dek_above ),
     :                   %VAL( iaddr_readout_noise ),
     :                   %VAL( iaddr_photon_to_adu ),
     :                   usr_tune_mxskypix,
     :                   %VAL( iaddr_inptim ),
     :                   %VAL( iaddr_inptime ),
     :                   %VAL( iaddr_inptimq ),
     :                   %VAL( iaddr_arc ),
     :                   %VAL( iaddr_extracted_obj ),
     :                   %VAL( iaddr_extr_obj_var ),
     :                   %VAL( iaddr_extracted_arc ),
     :                   %VAL( iaddr_extr_arc_var ),
     :                   %VAL( iaddr_x_trace_coord ),
     :                   %VAL( iaddr_y_trace_coord ),
     :                   status
     :                   )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_WAVELENGTH_CALIB' ) THEN
               CALL ECH_WAVELENGTH_CALIB(
     :              %VAL( iaddr_nx_pixels ),
     :              usr_low_wave,
     :              usr_hi_wave,
     :              %VAL( iaddr_extracted_arc ),
     :              %VAL( iaddr_windex_size ),
     :              usr_tune_db_scope,
     :              %VAL( iaddr_qindex_size ),
     :              %VAL( iaddr_fdb_wavelength ),
     :              %VAL( iaddr_fdb_intensity ),
     :              %VAL( iaddr_fdb_database ),
     :              %VAL( iaddr_fdb_left ),
     :              %VAL( iaddr_fdb_right ),
     :              %VAL( iaddr_fdb_wave_index),
     :              %VAL( iaddr_fdb_quick_index ),
     :              %VAL( iaddr_fdb_quick_value ),
     :              usr_id_interactive,
     :              usr_tune_revchk,
     :              %VAL( iaddr_no_of_orders ),
     :              usr_min_dispersion,
     :              usr_max_dispersion,
     :              usr_tune_idmxdif,
     :              usr_tune_idinmx,
     :              usr_tune_idsdlt,
     :              usr_tune_idmdlt,
     :              usr_tune_idstrng,
     :              %VAL( iaddr_central_wave ),
     :              %VAL( iaddr_central_onum ),
     :              usr_tune_maxrfln,
     :              %VAL( iaddr_obs_lines ),
     :              %VAL( iaddr_obs_inten ),
     :              usr_w_npoly,
     :              %VAL( iaddr_w_poly ),
     :              usr_tune_maxpoly,
     :              usr_wavfit,
     :              %VAL( iaddr_id_count ),
     :              %VAL( iaddr_id_lines ),
     :              %VAL( iaddr_id_status ),
     :              %VAL( iaddr_id_waves ),
     :              %VAL( iaddr_wsear_start ),
     :              %VAL( iaddr_wsear_end ),
     :              %VAL( iaddr_order_idnum ),
     :              %VAL( iaddr_fitted_waves ),
     :              %VAL( iaddr_fit_waves ),
     :              %VAL( iaddr_fit_waves2 ),
     :              %VAL( iaddr_fit_waves_work ),
     :              %VAL( iaddr_wleft_offset ),
     :              %VAL( iaddr_wright_offset ),
     :              %VAL( iaddr_wprev_index ),
     :              %VAL( iaddr_wnext_index ),
     :              %VAL( iaddr_wratios ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_WAVE_CONSISTENCY' ) THEN
                CALL ECH_WAVE_CONSISTENCY(
     :               %VAL( iaddr_nx_pixels ),
     :               %VAL( iaddr_windex_size ),
     :               %VAL( iaddr_fdb_wavelength ),
     :               %VAL( iaddr_no_of_orders ),
     :               usr_id_interactive,
     :               usr_tune_cnsdev,
     :               usr_tune_trcns,
     :               %VAL( iaddr_wsear_start ),
     :               %VAL( iaddr_wsear_end ),
     :               usr_tune_mxsmp,
     :               usr_tune_maxpoly,
     :               usr_w_npoly,
     :               %VAL( iaddr_w_poly ),
     :               usr_tune_maxrfln,
     :               %VAL( iaddr_obs_lines ),
     :               %VAL( iaddr_obs_inten ),
     :               %VAL( iaddr_order_idnum ),
     :               %VAL( iaddr_id_count ),
     :               %VAL( iaddr_id_lines ),
     :               %VAL( iaddr_id_status ),
     :               %VAL( iaddr_id_waves ),
     :               %VAL( iaddr_x_trace_coord ),
     :               %VAL( iaddr_trace_re_fits ),
     :               %VAL( iaddr_fit_work_xreal ),
     :               %VAL( iaddr_fit_work_xdouble ),
     :               %VAL( iaddr_fit_work_3xdouble ),
     :               status
     :               )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_GEN_REBIN_SCALE' ) THEN
               CALL ECH_GEN_REBIN_SCALE(
     :              usr_set_wscale,
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_no_of_orders ),
     :              %VAL( iaddr_w_poly ),
     :              usr_tune_maxpoly,
     :              usr_bin_size,
     :              usr_start_wave,
     :              %VAL( iaddr_no_of_bins ),
     :              %VAL( iaddr_nx_rebin ),
     :              status
     :              )
              ENDIF
              CALL ECH_MODULE_TIDYUP( option_module_name, status )

            ELSE IF ( usr_tune_client ) THEN
               CALL ECH_MODULE_TIDYUP( option_module_name, status )
            ENDIF
          ENDIF
        END DO
        ENDIF

*     Else must be an option which needs all order info at once
      ELSE
          CALL ECH_SET_PARAMETER( 'IDX_NUM_ORDERS', 'INT',
     :         0.0, 0, ' ', status )

          IF ( .NOT. ECH_FATAL_ERROR( status ) ) THEN
            IF  ( option_module_name .EQ. 'ECH_WRITE_RESULTS') THEN
               CALL ECH_SET_PARAMETER( 'IDX_NREF_FRAME', 'INT',
     :              1.0, 0, ' ', status )
            ENDIF

            IF  ( option_module_name( :12) .EQ. 'ECH_SCRUNCH_' .OR.
     :            option_module_name .EQ. 'ECH_NORMAL_YBLAZE'  .OR.
     :            option_module_name .EQ. 'ECH_MULTI_MERGE'    .OR.
     :            option_module_name .EQ. 'ECH_FLATTEN_YBLAZE' ) THEN
               CALL ECH_SET_PARAMETER( 'IDX_NREF_FRAME', 'INT',
     :              0.0, 0, ' ', status )
             IF  ( option_module_name .EQ. 'ECH_SCRUNCH_OBJ') THEN
              CALL ECH_SET_PARAMETER( 'SCRUNCH_TYPE', 'CHAR',
     :             0.0, 0, 'OBJ', status )
             ENDIF
             IF  ( option_module_name .EQ. 'ECH_SCRUNCH_ARC') THEN
              CALL ECH_SET_PARAMETER( 'SCRUNCH_TYPE', 'CHAR',
     :             0.0, 0, 'ARC', status )
             ENDIF
            ENDIF

            IF ( OPTION_MODULE_NAME .EQ. 'ECH_GET_SLIT_EXTENT' ) THEN
               IF ( USR_PFL_MODE( :1 ) .NE. 'E' ) THEN
                  CALL ECH_SET_PARAMETER( 'PFL_MODE', 'CHAR',
     :                 0., 0, 'D ', STATUS )

               ELSE
                  CALL ECH_SET_PARAMETER( 'PFL_MODE', 'CHAR',
     :                 0., 0, 'DE', STATUS )
               END IF

            ELSE IF ( OPTION_MODULE_NAME .EQ. 'ECH_GET_OBJ_PROFILE' )
     :                THEN
               IF ( USR_PFL_MODE( :1 ) .NE. 'E' ) THEN
                  CALL ECH_SET_PARAMETER( 'PFL_MODE', 'CHAR',
     :                 0., 0, 'O ', STATUS )

               ELSE
                  CALL ECH_SET_PARAMETER( 'PFL_MODE', 'CHAR',
     :                 0., 0, 'OE', STATUS )
               END IF

            ELSE IF ( OPTION_MODULE_NAME .EQ. 'ECH_GET_STAR_PROFILE' )
     :                THEN
               IF ( USR_PFL_MODE( :1 ) .NE. 'E' ) THEN
                  CALL ECH_SET_PARAMETER( 'PFL_MODE', 'CHAR',
     :                 0., 0, 'S ', STATUS )

               ELSE
                  CALL ECH_SET_PARAMETER( 'PFL_MODE', 'CHAR',
     :                 0., 0, 'SE', STATUS )
               END IF

            ELSE IF ( OPTION_MODULE_NAME .EQ.
     :                'ECH_DISABLE_ORDER' ) THEN
              CALL ECH_SET_PARAMETER( 'BAD_ORDER', 'CANCEL',
     :             0., 0, ' ', status )

            ELSE IF ( OPTION_MODULE_NAME .EQ.
     :                'ECH_MODEL_PROFILE' ) THEN
              CALL ECH_GET_PARAMETER( 'TUNE_OBJPOLY', 'INT',
     :             dummy, .FALSE., ' ', 0, status )
              IF ( INT( dummy ) .GT. 0 ) THEN
                 option_module_name = 'ECH_POLY_PROFILE'
                 usr_tune_objpoly = INT( dummy )
              ENDIF

            ELSE IF ( option_module_name .EQ.
     :                'ECH_NORMAL_YBLAZE' ) THEN
              CALL ECH_GET_PARAMETER( 'TUNE_YBLAZE', 'LOGICAL',
     :             0., ldummy, ' ', 0, status )
              usr_tune_yblaze = ldummy
              IF ( .NOT. usr_tune_yblaze ) GO TO 999

            ELSE IF ( option_module_name .EQ.
     :                'ECH_FLATTEN_YBLAZE' ) THEN
              CALL ECH_GET_PARAMETER( 'TUNE_YBLAZE', 'LOGICAL',
     :             0., ldummy, ' ', 0, status )
              usr_tune_yblaze = ldummy
              IF ( .NOT. usr_tune_yblaze ) GO TO 999

            ELSE IF ( option_module_name .EQ.
     :                'ECH_DECOSMIC_1' ) THEN
              CALL ECH_GET_PARAMETER( 'TUNE_CRTRC', 'LOGICAL',
     :             0., ldummy, ' ', 0, status )
              usr_tune_crtrc = ldummy
              IF ( .NOT. usr_tune_crtrc ) GO TO 999
            ENDIF

            CALL ECH_MODULE_INIT( OPTION_MODULE_NAME, STATUS )
            IF ( .NOT. ECH_FATAL_ERROR( STATUS ) ) THEN
              IF ( usr_tune_clone .NE. 'NULL' ) THEN
                 CONTINUE

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_CHECK_FRAME' ) THEN
               CALL ECH_CHECK_FRAME(
     :              %VAL( iaddr_inptim ),
     :              %VAL( iaddr_tracim ),
     :              nx,
     :              ny,
     :              usr_tune_fcheck,
     :              usr_var_nbadrow,
     :              %VAL( iaddr_bad_rows ),
     :              usr_var_nbadcol,
     :              %VAL( iaddr_bad_cols ),
     :              usr_tune_satrtn,
     :              cstr_rdctn_arc,
     :              %VAL( iaddr_nref_frame ),
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              %VAL( iaddr_inptimq ),
     :              %VAL( iaddr_tracimq )
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_DETERMINE_SLOPE' ) THEN
               CALL ECH_DETERMINE_SLOPE(
     :              %VAL( iaddr_tracim ),
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              usr_tune_xbox,
     :              usr_use_median,
     :              usr_tune_fcheck,
     :              %VAL( iaddr_order_slope ),
     :              %VAL( iaddr_dsample_xcoord ),
     :              %VAL( iaddr_dsample_ycoord ),
     :              %VAL( iaddr_diagonal ),
     :              %VAL( iaddr_diagonal2 ),
     :              %VAL( iaddr_smooth_diag ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_COUNT_ORDERS' ) THEN
               CALL ECH_COUNT_ORDERS(
     :              %VAL( iaddr_tracim ),
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              usr_tune_xbox,
     :              usr_use_median,
     :              usr_tune_autloc,
     :              usr_tune_partord,
     :              %VAL( iaddr_order_slope ),
     :              %VAL( iaddr_trace_width),
     :              %VAL( iaddr_no_of_orders ),
     :              %VAL( iaddr_convolved ),
     :              %VAL( iaddr_convolved2 ),
     :              %VAL( iaddr_correlate ),
     :              %VAL( iaddr_correl_count ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_LOCATE_ORDERS' ) THEN
               CALL ECH_LOCATE_ORDERS(
     :              %VAL( iaddr_tracim ),
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              usr_tune_xbox,
     :              usr_use_median,
     :              usr_tune_autloc,
     :              usr_tune_partord,
     :              %VAL( iaddr_order_slope ),
     :              %VAL( iaddr_no_of_orders ),
     :              usr_tune_twthr,
     :              %VAL( iaddr_trace_width ),
     :              %VAL( iaddr_order_ypos ),
     :              %VAL( iaddr_convolved ),
     :              %VAL( iaddr_convolved2 ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_GET_SLIT_EXTENT' ) THEN
               CALL ECH_GET_PROFILE(
     :              %VAL( iaddr_slitim ),
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              %VAL( iaddr_no_of_orders ),
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_trc_poly ),
     :              usr_pfl_interact,
     :              usr_pfl_mode,
     :              usr_tune_use_nxf,
     :              usr_tune_pflssamp,
     :              usr_tune_mxskypix,
     :              usr_tune_dekthr,
     :              usr_tune_skyhilim,
     :              %VAL( iaddr_dek_below ),
     :              %VAL( iaddr_dek_above ),
     :              usr_tune_dekblw,
     :              usr_tune_dekabv,
     :              0,
     :              0,
     :              %VAL( iaddr_contin_profile ),
     :              %VAL( iaddr_x_trace_coord ),
     :              %VAL( iaddr_y_trace_coord ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_GET_OBJ_PROFILE' ) THEN
               CALL ECH_GET_PROFILE(
     :              %VAL( iaddr_inptim ),
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              %VAL( iaddr_no_of_orders ),
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_trc_poly ),
     :              usr_pfl_interact,
     :              usr_pfl_mode,
     :              usr_tune_use_nxf,
     :              usr_tune_pflssamp,
     :              usr_tune_mxskypix,
     :              usr_tune_dekthr,
     :              usr_tune_skyhilim,
     :              %VAL( iaddr_dek_below ),
     :              %VAL( iaddr_dek_above ),
     :              usr_tune_objblw,
     :              usr_tune_objabv,
     :              %VAL( iaddr_obj_mask ),
     :              %VAL( iaddr_sky_mask ),
     :              %VAL( iaddr_object_profile ),
     :              %VAL( iaddr_x_trace_coord ),
     :              %VAL( iaddr_y_trace_coord ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_GET_STAR_PROFILE' ) THEN
               CALL ECH_GET_PROFILE(
     :              %VAL( iaddr_star ),
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              %VAL( iaddr_no_of_orders ),
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_trc_poly ),
     :              usr_pfl_interact,
     :              usr_pfl_mode,
     :              usr_tune_use_nxf,
     :              usr_tune_pflssamp,
     :              usr_tune_mxskypix,
     :              usr_tune_dekthr,
     :              usr_tune_skyhilim,
     :              %VAL( iaddr_dek_below ),
     :              %VAL( iaddr_dek_above ),
     :              usr_tune_objblw,
     :              usr_tune_objabv,
     :              %VAL( iaddr_calobj_mask ),
     :              %VAL( iaddr_calsky_mask ),
     :              %VAL( iaddr_star_profile ),
     :              %VAL( iaddr_x_trace_coord ),
     :              %VAL( iaddr_y_trace_coord ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_SCRUNCH_OBJ' ) THEN
               usr_scrunch_type = 'OBJ'
               CALL ECH_SCRUNCH_ORDERS(
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_no_of_bins ),
     :              order_number,
     :              usr_scrunch_type,
     :              usr_set_wscale,
     :              %VAL( iaddr_wavelength ),
     :              %VAL( iaddr_extracted_obj ),
     :              %VAL( iaddr_extr_obj_var ),
     :              %VAL( iaddr_blaze_spect ),
     :              %VAL( iaddr_no_of_orders ),
     :              %VAL( iaddr_nx_rebin ),
     :              usr_tune_scfract,
     :              usr_tune_skew,
     :              usr_tune_log,
     :              usr_tune_flux,
     :              usr_tune_intr,
     :              usr_tune_quad,
     :              usr_tune_scrmode,
     :              usr_tune_scradd,
     :              usr_tune_merge,
     :              usr_tune_yblaze,
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_w_poly ),
     :              %VAL( iaddr_scrnchd_waves ),
     :              %VAL( iaddr_spectrum ),
     :              %VAL( iaddr_err_spectrum ),
     :              %VAL( iaddr_scrnchd_obj ),
     :              %VAL( iaddr_scrnchd_objv ),
     :              %VAL( iaddr_wvscale_index ),
     :              %VAL( iaddr_rebin_work ),
     :              %VAL( iaddr_mfilter1 ),
     :              %VAL( iaddr_mfilter2 ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_SCRUNCH_ARC' ) THEN
               usr_scrunch_type = 'ARC'
               CALL ECH_SCRUNCH_ORDERS(
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_no_of_bins ),
     :              order_number,
     :              usr_scrunch_type,
     :              usr_set_wscale,
     :              %VAL( iaddr_wavelength ),
     :              %VAL( iaddr_extracted_arc ),
     :              %VAL( iaddr_extr_arc_var ),
     :              %VAL( iaddr_blaze_spect ),
     :              %VAL( iaddr_no_of_orders ),
     :              %VAL( iaddr_nx_rebin ),
     :              usr_tune_scfract,
     :              usr_tune_skew,
     :              usr_tune_log,
     :              usr_tune_flux,
     :              usr_tune_intr,
     :              usr_tune_quad,
     :              usr_tune_scrmode,
     :              usr_tune_scradd,
     :              usr_tune_merge,
     :              usr_tune_yblaze,
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_w_poly ),
     :              %VAL( iaddr_scrnchd_waves ),
     :              %VAL( iaddr_fspectrum ),
     :              %VAL( iaddr_err_fspectrum ),
     :              %VAL( iaddr_scrnchd_arc ),
     :              %VAL( iaddr_scrnchd_arcv ),
     :              %VAL( iaddr_wvscale_index ),
     :              %VAL( iaddr_rebin_work ),
     :              %VAL( iaddr_mfilter1 ),
     :              %VAL( iaddr_mfilter2 ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_SCRUNCH_STAR' ) THEN
               usr_scrunch_type = 'STAR'
               CALL ECH_SCRUNCH_ORDERS(
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_no_of_bins ),
     :              order_number,
     :              usr_scrunch_type,
     :              usr_set_wscale,
     :              %VAL( iaddr_wavelength ),
     :              %VAL( iaddr_extracted_star ),
     :              %VAL( iaddr_extr_star_var ),
     :              %VAL( iaddr_blaze_spect ),
     :              %VAL( iaddr_no_of_orders ),
     :              %VAL( iaddr_nx_rebin ),
     :              usr_tune_scfract,
     :              usr_tune_skew,
     :              usr_tune_log,
     :              usr_tune_flux,
     :              usr_tune_intr,
     :              usr_tune_quad,
     :              usr_tune_scrmode,
     :              usr_tune_scradd,
     :              usr_tune_merge,
     :              usr_tune_yblaze,
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_w_poly ),
     :              %VAL( iaddr_scrnchd_waves ),
     :              %VAL( iaddr_fspectrum ),
     :              %VAL( iaddr_err_fspectrum ),
     :              %VAL( iaddr_scrnchd_star ),
     :              %VAL( iaddr_scrnchd_strv ),
     :              %VAL( iaddr_wvscale_index ),
     :              %VAL( iaddr_rebin_work ),
     :              %VAL( iaddr_mfilter1 ),
     :              %VAL( iaddr_mfilter2 ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_MULTI_MERGE' ) THEN
               CALL ECH_MULTI_MERGE(
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_no_of_bins ),
     :              usr_set_wscale,
     :              %VAL( iaddr_wavelength ),
     :              %VAL( iaddr_extracted_obj ),
     :              %VAL( iaddr_extr_obj_var ),
     :              %VAL( iaddr_no_of_orders ),
     :              %VAL( iaddr_nx_rebin ),
     :              usr_tune_scfract,
     :              usr_tune_skew,
     :              usr_tune_log,
     :              usr_tune_flux,
     :              usr_tune_intr,
     :              usr_tune_quad,
     :              usr_tune_scrmode,
     :              usr_tune_scradd,
     :              usr_tune_airtovac,
     :              usr_tune_mrgminx,
     :              usr_tune_mrgmaxx,
     :              usr_tune_mrgwght,
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_w_poly ),
     :              %VAL( iaddr_scrnchd_waves ),
     :              %VAL( iaddr_spectrum ),
     :              %VAL( iaddr_err_spectrum ),
     :              %VAL( iaddr_scrnchd_obj ),
     :              %VAL( iaddr_scrnchd_objv ),
     :              %VAL( iaddr_wvscale_index ),
     :              %VAL( iaddr_rebin_work ),
     :              %VAL( iaddr_mfilter1 ),
     :              %VAL( iaddr_mfilter2 ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_PLOT_TRACES' ) THEN
               CALL ECH_PLOT_TRACES(
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              %VAL( iaddr_tracim ),
     :              %VAL( iaddr_no_of_orders ),
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_trc_poly ),
     :              %VAL( iaddr_x_trace_coord ),
     :              %VAL( iaddr_y_trace_coord ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_IMAGE_TRACE' ) THEN
               CALL ECH_IMAGE_TRACE(
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              %VAL( iaddr_no_of_orders ),
     :              %VAL( iaddr_tracim ),
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_trc_poly ),
     :              %VAL( iaddr_output_image ),
     :              %VAL( iaddr_x_trace_coord ),
     :              %VAL( iaddr_y_trace_coord ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_TRACE_CONSISTENCY' ) THEN
               CALL ECH_TRACE_CONSISTENCY(
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              %VAL( iaddr_tracim ),
     :              %VAL( iaddr_no_of_orders ),
     :              usr_trc_interact,
     :              usr_tune_cnsdev,
     :              usr_tune_trcns,
     :              usr_tune_mxsmp,
     :              usr_tune_maxpoly,
     :              usr_trc_npoly,
     :              %VAL( iaddr_trc_poly ),
     :              %VAL( iaddr_x_trace_coord ),
     :              %VAL( iaddr_trace_re_fits ),
     :              %VAL( iaddr_fit_work_xint ),
     :              %VAL( iaddr_fit_work_xdouble ),
     :              %VAL( iaddr_fit_work_3xdouble ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_NORMAL_YBLAZE' ) THEN
               CALL ECH_NORMAL_YBLAZE(
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_no_of_orders ),
     :              %VAL( iaddr_blaze_medians ),
     :              usr_blz_npoly,
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_fitted_waves ),
     :              %VAL( iaddr_extracted_obj ),
     :              %VAL( iaddr_extr_obj_var ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_FLATTEN_YBLAZE' ) THEN
               CALL ECH_FLATTEN_YBLAZE(
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_no_of_orders ),
     :              %VAL( iaddr_blaze_medians ),
     :              %VAL( iaddr_extracted_obj ),
     :              %VAL( iaddr_extr_obj_var ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_DECOSMIC_1' ) THEN
               CALL ECH_DECOSMIC_1(
     :              %VAL( iaddr_tracim ),
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              usr_tune_crxbox,
     :              usr_tune_crybox,
     :              usr_tune_mincr,
     :              usr_tune_crtrc,
     :              usr_tune_crmax,
     :              usr_tune_crinter,
     :              %VAL( iaddr_xmedtmp ),
     :              %VAL( iaddr_ymedtmp ),
     :              %VAL( iaddr_tracimq ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_IMAGE_COSMIC' ) THEN
               CALL ECH_TRANSFER_INT( %VAL( iaddr_so_far ), isf )
               IF ( isf .LE. 3 ) THEN
                 CALL ECH_REPORT( 0,
     :               ' Creating image of trace-frame Cosmic rays.' )
                 CALL ECH_IMAGE_COSMIC(
     :                %VAL( iaddr_nx_pixels ),
     :                %VAL( iaddr_ny_pixels ),
     :                %VAL( iaddr_tracim ),
     :                %VAL( iaddr_tracimq ),
     :                %VAL( iaddr_output_image ),
     :                status
     :                )

               ELSE
                 CALL ECH_REPORT( 0,
     :                ' Creating image of object-frame Cosmic Rays.' )
                 CALL ECH_IMAGE_COSMIC(
     :                %VAL( iaddr_nx_pixels ),
     :                %VAL( iaddr_ny_pixels ),
     :                %VAL( iaddr_inptim ),
     :                %VAL( iaddr_inptimq ),
     :                %VAL( iaddr_output_image ),
     :                status
     :                )
               ENDIF

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_DECOSMIC_2' ) THEN
               CALL ECH_DECOSMIC_2(
     :              %VAL( iaddr_inptim ),
     :              %VAL( iaddr_inptimq ),
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              %VAL( iaddr_no_of_orders ),
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_trc_poly ),
     :              usr_tune_mxskypix,
     :              %VAL( iaddr_obj_mask ),
     :              %VAL( iaddr_sky_mask ),
     :              %VAL( iaddr_dek_below ),
     :              %VAL( iaddr_dek_above ),
     :              usr_tune_fincply,
     :              %VAL( iaddr_ply_orderincs ),
     :              usr_tune_dsgmthr,
     :              usr_tune_dprbthr,
     :              %VAL( iaddr_sky_spectrum ),
     :              %VAL( iaddr_x_trace_coord ),
     :              %VAL( iaddr_y_trace_coord ),
     :              %VAL( iaddr_decos_data ),
     :              %VAL( iaddr_decos_sdata ),
     :              %VAL( iaddr_decos_xaxis ),
     :              %VAL( iaddr_decos_yaxis ),
     :              %VAL( iaddr_decos_gauss ),
     :              %VAL( iaddr_decos_index_x  ),
     :              %VAL( iaddr_decos_index_y ),
     :              %VAL( iaddr_fit_work_xreal ),
     :              %VAL( iaddr_fit_work_xreal2 ),
     :              %VAL( iaddr_fit_work_xreal3 ),
     :              %VAL( iaddr_fit_work_xreal4 ),
     :              %VAL( iaddr_fit_work_xint ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_POLY_PROFILE' ) THEN
                  CALL ECH_POLY_PROFILE(
     :                 %VAL( iaddr_inptim ),
     :                 %VAL( iaddr_inptimq ),
     :                 %VAL( iaddr_nx_pixels ),
     :                 %VAL( iaddr_ny_pixels ),
     :                 order_number,
     :                 %VAL( iaddr_no_of_orders ),
     :                 usr_tune_maxpoly,
     :                 usr_objfit,
     :                 %VAL( iaddr_trc_poly ),
     :                 usr_pfl_interact,
     :                 usr_tune_pflssamp,
     :                 usr_tune_mxskypix,
     :                 usr_tune_use_nxf,
     :                 %VAL( iaddr_dek_below ),
     :                 %VAL( iaddr_dek_above ),
     :                 %VAL( iaddr_obj_mask ),
     :                 %VAL( iaddr_sky_mask ),
     :                 %VAL( iaddr_fitted_sky ),
     :                 usr_tune_objpoly,
     :                 usr_tune_objrej,
     :                 usr_tune_objrthr,
     :                 %VAL( iaddr_fitted_pfl ),
     :                 %VAL( iaddr_x_trace_coord ),
     :                 %VAL( iaddr_y_trace_coord ),
     :                 %VAL( iaddr_wpfl_total ),
     :                 %VAL( iaddr_wpfl_tcount ),
     :                 %VAL( iaddr_fit_work_xdouble ),
     :                 status
     :                 )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_MODEL_PROFILE' ) THEN
                  CALL ECH_MODEL_PROFILE(
     :                 %VAL( iaddr_inptim ),
     :                 %VAL( iaddr_inptimq ),
     :                 %VAL( iaddr_nx_pixels ),
     :                 %VAL( iaddr_ny_pixels ),
     :                 order_number,
     :                 %VAL( iaddr_no_of_orders ),
     :                 usr_tune_maxpoly,
     :                 %VAL( iaddr_trc_poly ),
     :                 usr_tune_pflssamp,
     :                 usr_tune_mxskypix,
     :                 usr_tune_use_nxf,
     :                 %VAL( iaddr_dek_below ),
     :                 %VAL( iaddr_dek_above ),
     :                 %VAL( iaddr_obj_mask ),
     :                 %VAL( iaddr_fitted_sky ),
     :                 %VAL( iaddr_model_profile ),
     :                 %VAL( iaddr_x_trace_coord ),
     :                 %VAL( iaddr_y_trace_coord ),
     :                 %VAL( iaddr_wpfl_total ),
     :                 %VAL( iaddr_wpfl_tcount ),
     :                 status  )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_DISABLE_ORDER' ) THEN
               CALL ECH_DISABLE_ORDER(
     :              usr_bad_order,
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_no_of_orders ),
     :              %VAL( iaddr_trc_poly ),
     :              status )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_PLOTTER' ) THEN
               CALL ECH_PLOTTER( %VAL( iaddr_no_of_orders ),
     :              usr_tune_maxpoly,
     :              %VAL( iaddr_trc_poly ),
     :              usr_tune_mxskypix,
     :              status )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_MODEL_BACK' ) THEN
               no_errors = .FALSE.
               IF ( iaddr_inptime .EQ. 0 ) no_errors = .TRUE.
               CALL ECH_MODEL_BACK(
     :              %VAL( iaddr_inptim ),
     :              %VAL( iaddr_inptimq ),
     :              %VAL( iaddr_inptime ),
     :              %VAL( iaddr_nx_pixels ),
     :              %VAL( iaddr_ny_pixels ),
     :              %VAL( iaddr_no_of_orders ),
     :              usr_tune_noflat,
     :              no_errors,
     :              %VAL( iaddr_fitted_flat ),
     :              %VAL( iaddr_flat_errors ),
     :              usr_tune_mxskypix,
     :              %VAL( iaddr_sky_mask ),
     :              %VAL( iaddr_dek_below ),
     :              %VAL( iaddr_dek_above ),
     :              usr_tune_skypoly,
     :              usr_tune_skyrej,
     :              usr_tune_skyrthr,
     :              usr_tune_skysim,
     :              %VAL( iaddr_readout_noise ),
     :              %VAL( iaddr_photon_to_adu ),
     :              usr_tune_maxpoly,
     :              usr_skyfit,
     :              %VAL( iaddr_trc_poly ),
     :              %VAL( iaddr_fitted_sky ),
     :              %VAL( iaddr_fsky_errors ),
     :              %VAL( iaddr_x_trace_coord ),
     :              %VAL( iaddr_y_trace_coord ),
     :              %VAL( iaddr_fit_work_xreal ),
     :              %VAL( iaddr_fit_work_xreal2 ),
     :              %VAL( iaddr_fit_work_xreal3 ),
     :              %VAL( iaddr_fit_work_xreal4 ),
     :              status
     :              )

              ELSE IF ( option_module_name .EQ.
     :                  'ECH_WRITE_RESULTS' ) THEN
               CALL ECH_WRITE_RESULTS( order_number,
     :              usr_result_type,
     :              usr_result_format,
     :              usr_tune_archive,
     :              usr_tune_useaaa,
     :              usr_tune_aaacode,
     :              status )
              ENDIF

              CALL ECH_MODULE_TIDYUP(
     :             OPTION_MODULE_NAME, STATUS )

            ELSE IF ( USR_TUNE_CLIENT ) THEN
              CALL ECH_MODULE_TIDYUP(
     :             OPTION_MODULE_NAME, STATUS )
            ENDIF
            IF ( USR_PFL_MODE( 2:2 ) .EQ. 'E' ) THEN
               CALL ECH_SET_PARAMETER( 'PFL_MODE', 'CHAR',
     :              0., 0, 'E', STATUS )
            END IF
          END IF
      END IF

 999  CONTINUE

      IF ( .NOT. ECH_FATAL_ERROR( STATUS ) ) THEN
         IF ( .NOT. USR_TUNE_BATCH .AND. .NOT. USR_TUNE_CLIENT ) THEN
            IF ( OPTION_MODULE_NAME .EQ. 'ECH_WAVELENGTH_CALIB' ) THEN
               CALL ECH_SET_PARAMETER( 'AUTO_ID', 'CANCEL',
     :              0.0, 0, ' ', STATUS )

            ELSE IF ( OPTION_MODULE_NAME .EQ.
     :                'ECH_FIT_ORDER_TRACE' ) THEN
               USR_TRC_INTERACT = .TRUE.

            ELSE IF ( OPTION_MODULE_NAME .EQ.
     :                'ECH_FIT_2D_DISTORTION' ) THEN
               USR_2D_INTERACT = .TRUE.
            END IF
         END IF
      END IF

      CALL ECH_DATAFILE_TIDYUP( STATUS )
      IF ( OPTION_MODULE_NAME .EQ. 'ECH_WRITE_RESULTS' ) THEN
         CALL ECH_SET_PARAMETER( 'RESULT_TYPE', 'CANCEL',
     :        0.0, 0, ' ', STATUS )
         CALL ECH_SET_PARAMETER( 'RESULT_FORMAT', 'CANCEL',
     :        0.0, 0, ' ', STATUS )
      END IF

 1001 FORMAT ( 1X, 'Processing order ', A, '...' )
 1002 FORMAT ( 1X, 'Processing reference frame ', A, '...' )

      END

      INTEGER FUNCTION ECH_DEREF_INT( VALUE )

      IMPLICIT NONE

      INTEGER VALUE

      ECH_DEREF_INT = VALUE

      END

      REAL FUNCTION ECH_DEREF_REAL( VALUE )

      IMPLICIT NONE

      REAL VALUE

      ECH_DEREF_REAL = VALUE

      END
