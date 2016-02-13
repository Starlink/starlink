#if HAVE_CONFIG_H
# include "config.h"
#endif
      SUBROUTINE ECH_SETUP_OBJECT_REF(
     :           REF_NAME,
     :           MAPPED_ADDRESS,
     :           MAPPED_QADDRESS,
     :           MAPPED_EADDRESS,
     :           STRING,
     :           VALUE,
     :           BOOLEAN_VALUE,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_SETUP_OBJECT_REF

*  Purpose:
*     Map object to storage variable in common.

*  Description:
*     Setup an object reference (for mapped objects), a value (always REAL)
*     or a string variable depending upon the object.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables and Constants:
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_INIT_RDCTN.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_SERVER.INC'
      INCLUDE 'ECH_IMG_NAMES.INC'
      INCLUDE 'ECH_FOREIGN_PAR.INC'

*  Arguments:
      CHARACTER*( * ) REF_NAME
      INTEGER MAPPED_ADDRESS
      INTEGER MAPPED_QADDRESS
      INTEGER MAPPED_EADDRESS
      CHARACTER*( * ) STRING
      REAL VALUE
      LOGICAL BOOLEAN_VALUE
      INTEGER STATUS

*  Local Variables:
      REAL VALARRY( 2 )

      INTEGER DUMDIM( 7 )
      INTEGER I
      INTEGER IOBJ
      INTEGER DATA_ADDRESS
      INTEGER DATA_EADDRESS
      INTEGER DATA_QADDRESS
      INTEGER ILOOP
      INTEGER ECH_OBJ_IND
      INTEGER INDEX_NUMBER

      LOGICAL BOOLVALARRY( 2 )
      LOGICAL FOUND

      CHARACTER*33 COMP1
      CHARACTER*33 COMP2
*.

*  Potentially dynamic parameters (i.e. IDX_ array indices) are
*  checked first as they will be often used.
      IF ( ref_name( :4 ) .EQ. 'IDX_' ) THEN
         index_number = 0
         comp1 = ref_name // ' '
         DO iloop = 1, dynamic_indices_used
            comp2 = dynamic_index_name( iloop ) // ' '
            IF ( comp1 .EQ. comp2 ) THEN
               index_number = iloop
            END IF
         END DO
         IF ( index_number .EQ. 0 ) THEN
            index_number = dynamic_indices_used + 1
            dynamic_indices_used = dynamic_indices_used + 1
            dynamic_index_name( index_number ) = comp1
         END IF
         dynamic_index_value( index_number ) = INT( value )
         GO TO 999
      END IF

*  Each 'required_object' has its own piece of code. The required
*  objects are defined at initialisation in ECH_DEFINE_MODULES.
      IF ( ICHAR( REF_NAME( :1 ) ) .LT. 65 ) THEN
         GO TO 26

      ELSE
         GO TO ( 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13,
     :          14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26 ),
     :         ICHAR( REF_NAME( :1 ) ) - 64
      END IF

*  'A'
    1 IF ( REF_NAME .EQ. 'ARC' .OR.
     :     ( REF_NAME( :3 ) .EQ. 'ARC' .AND.
     :     REF_NAME(4:4) .GE. '1' .AND.
     :     REF_NAME(4:4) .LE. '9' ) ) THEN
         IF ( STATUS .EQ. ECH__IMAGE_LIST ) THEN
            CSTR_RDCTN_ARC = STRING

         ELSE
            IADDR_ARC = MAPPED_ADDRESS
            IADDR_ARCQ = MAPPED_QADDRESS
            IADDR_ARCE = MAPPED_EADDRESS
            IMG_ARC = STRING
         END IF
         IF ( STRING .EQ. 'NONE' .OR. STRING .EQ. 'none' ) THEN
            USR_TUNE_NOARC = .TRUE.

         ELSE
            USR_TUNE_NOARC = .FALSE.
         END IF

      ELSE IF ( ref_name .EQ. 'ARC_TYPE' ) THEN
         usr_arc_type = string

      ELSE IF ( ref_name .EQ. 'ASCII_FILE' ) THEN
         out_file = string

      ELSE IF ( ref_name .EQ. 'AUTO_ID' ) THEN
         usr_id_interactive = boolean_value

      ELSE IF ( ref_name .EQ. 'AVG_SPECTRUM' ) THEN
         iaddr_avg_spectrum = mapped_address
      END IF
      GO TO 999

*  'B'
    2 IF ( ref_name .EQ. 'BAD_COLS' ) THEN
         iaddr_bad_cols = mapped_address
         usr_var_nbadcol = mapped_qaddress

      ELSE IF ( ref_name .EQ. 'BAD_ROWS' ) THEN
         iaddr_bad_rows = mapped_address
         usr_var_nbadrow = mapped_qaddress

      ELSE IF ( ref_name .EQ. 'BAD_ORDER' ) THEN
         usr_bad_order = INT( value )

      ELSE IF ( ref_name .EQ. 'BIN_SIZE' ) THEN
         usr_bin_size = value

      ELSE IF ( ref_name .EQ. 'BLAZE_MEDIANS' ) THEN
         iaddr_blaze_medians = mapped_address

      ELSE IF ( ref_name .EQ. 'BLAZE_SPECT' ) THEN
         iaddr_blaze_spect = mapped_address

      ELSE IF ( ref_name .EQ. 'BLZ_INTERACT' ) THEN
         usr_blz_interact = boolean_value

      ELSE IF ( ref_name .EQ. 'BLZ_NPOLY' ) THEN
         usr_blz_npoly = INT( value )

      ELSE IF ( ref_name .EQ. 'BLZFIT' ) THEN
         usr_blzfit = string
      END IF
      GO TO 999

*  'C'
    3 IF ( ref_name .EQ. 'CALOBJ_MASK' ) THEN
         iaddr_calobj_mask = mapped_address

      ELSE IF ( ref_name .EQ. 'CALSKY_MASK' ) THEN
         iaddr_calsky_mask = mapped_address

      ELSE IF ( ref_name .EQ. 'CENTRAL_ONUM' ) THEN
         iaddr_central_onum = mapped_address

      ELSE IF ( ref_name .EQ. 'CENTRAL_WAVE' ) THEN
         iaddr_central_wave = mapped_address

      ELSE IF ( ref_name .EQ. 'CONTIN_PROFILE' ) THEN
         iaddr_contin_profile = mapped_address

      ELSE IF ( ref_name .EQ. 'CONVOLVED' ) THEN
         iaddr_convolved = mapped_address

      ELSE IF ( ref_name .EQ. 'CONVOLVED2' ) THEN
         iaddr_convolved2 = mapped_address

      ELSE IF ( ref_name .EQ. 'CORREL_COUNT' ) THEN
         iaddr_correl_count = mapped_address

      ELSE IF ( ref_name .EQ. 'CORRELATE' ) THEN
         iaddr_correlate = mapped_address

      ELSE IF ( ref_name .EQ. 'CRX_MEDIAN' ) THEN
         iaddr_xmedtmp = mapped_address

      ELSE IF ( ref_name .EQ. 'CRY_MEDIAN' ) THEN
         iaddr_ymedtmp = mapped_address
      END IF
      GO TO 999

*  'D'
    4 IF ( ref_name .EQ. 'DECOS_DATA' ) THEN
         iaddr_decos_data = mapped_address

      ELSE IF ( ref_name .EQ. 'DECOS_GAUSS' ) THEN
         iaddr_decos_gauss = mapped_address

      ELSE IF ( ref_name .EQ. 'DECOS_INDEX_X' ) THEN
         iaddr_decos_index_x = mapped_address

      ELSE IF ( ref_name .EQ. 'DECOS_INDEX_Y' ) THEN
         iaddr_decos_index_y = mapped_address

      ELSE IF ( ref_name .EQ. 'DECOS_SDATA' ) THEN
         iaddr_decos_sdata = mapped_address

      ELSE IF ( ref_name .EQ. 'DECOS_XAXIS' ) THEN
         iaddr_decos_xaxis = mapped_address

      ELSE IF ( ref_name .EQ. 'DECOS_YAXIS' ) THEN
         iaddr_decos_yaxis = mapped_address

      ELSE IF ( ref_name .EQ. 'DEK_ABOVE' ) THEN
         iaddr_dek_above = mapped_address

      ELSE IF ( ref_name .EQ. 'DEK_BELOW' ) THEN
         iaddr_dek_below = mapped_address

      ELSE IF ( ref_name .EQ. 'DIAGONAL' ) THEN
         iaddr_diagonal = mapped_address

      ELSE IF ( ref_name .EQ. 'DIAGONAL2' ) THEN
         iaddr_diagonal2 = mapped_address

      ELSE IF ( ref_name .EQ. 'DISPLAY' ) THEN
         usr_display = boolean_value

      ELSE IF ( ref_name .EQ. 'DSAMPLE_XCOORD' ) THEN
         iaddr_dsample_xcoord = mapped_address

      ELSE IF ( ref_name .EQ. 'DSAMPLE_YCOORD' ) THEN
         iaddr_dsample_ycoord = mapped_address
      END IF
      GO TO 999

*  'E'
    5 IF ( ref_name .EQ. 'ECH_RDCTN' ) THEN
         rdctn_file_main = string

      ELSE IF ( ref_name .EQ. 'ECH_RDUCD' ) THEN
         rdctn_file_rducd = string

      ELSE IF ( ref_name .EQ. 'ECH_FTRDB' ) THEN
         rdctn_file_ftrdb = string

      ELSE IF ( ref_name .EQ. 'ECH_ECHAR' ) THEN
         rdctn_file_echarc = string

      ELSE IF ( ref_name .EQ. 'ECHARC_LABELX' ) THEN
         cstr_echarc_labelx =  string

      ELSE IF ( ref_name .EQ. 'ECHARC_LABELY' ) THEN
         cstr_echarc_labely =  string

      ELSE IF ( ref_name .EQ. 'ECHARC_LABELZ' ) THEN
         cstr_echarc_labelz =  string

      ELSE IF ( ref_name .EQ. 'ECHARC_ORDNUM' ) THEN
         iaddr_echarc_datay = mapped_address

      ELSE IF ( ref_name .EQ. 'ECHARC_REFFTR' ) THEN
         iaddr_echarc_dataz = mapped_address

      ELSE IF ( ref_name .EQ. 'ECHARC_REFWAV' ) THEN
         iaddr_echarc_datax = mapped_address

      ELSE IF ( REF_NAME .EQ. 'EFTRDB_IND_SIZ' ) THEN
         iaddr_windex_size = mapped_address

      ELSE IF ( ref_name .EQ. 'EFTRDB_LABELX' ) THEN
         cstr_eftrdb_labelx =  string

      ELSE IF ( ref_name .EQ. 'EFTRDB_LABELZ' ) THEN
         cstr_eftrdb_labelz =  string

      ELSE IF ( ref_name .EQ. 'EFTRDB_SOURCE' ) THEN
         cstr_eftrdb_source =  string

      ELSE IF ( ref_name .EQ. 'EFTRDB_INTENSITY' ) THEN
         iaddr_fdb_intensity = mapped_address

      ELSE IF ( ref_name .EQ. 'EFTRDB_WAVELENGTH' ) THEN
         iaddr_fdb_wavelength = mapped_address

      ELSE IF ( ref_name .EQ. 'EFTRDB_WSAMPLES' ) THEN
         iaddr_fdb_wsamples = mapped_address

      ELSE IF ( ref_name .EQ. 'EFTRDB_WSTATUS' ) THEN
         iaddr_fdb_wstatus = mapped_address

      ELSE IF ( ref_name .EQ. 'EFTRDB_DATABASE' ) THEN
         iaddr_fdb_database = mapped_address

      ELSE IF ( ref_name .EQ. 'EFTRDB_LEFT' ) THEN
         iaddr_fdb_left = mapped_address

      ELSE IF ( ref_name .EQ. 'EFTRDB_RIGHT' ) THEN
         iaddr_fdb_right = mapped_address

      ELSE IF ( ref_name .EQ. 'EFTRDB_WAVE_INDEX' ) THEN
         iaddr_fdb_wave_index = mapped_address

      ELSE IF ( REF_NAME .EQ. 'EFTRDB_QIND_SIZ' ) THEN
         iaddr_qindex_size = mapped_address

      ELSE IF ( ref_name .EQ. 'EFTRDB_QUICK_INDEX' ) THEN
         iaddr_fdb_quick_index = mapped_address

      ELSE IF ( ref_name .EQ. 'EFTRDB_QUICK_VALUE' ) THEN
         iaddr_fdb_quick_value = mapped_address

      ELSE IF ( ref_name .EQ. 'ERR_SPECTRUM' ) THEN
         iaddr_err_spectrum = mapped_address

      ELSE IF ( ref_name .EQ. 'ERR_STDSPECTRUM' ) THEN
         iaddr_err_fspectrum = mapped_address

      ELSE IF ( ref_name .EQ. 'EXTR_ARC_VAR' ) THEN
         iaddr_extr_arc_var = mapped_address

      ELSE IF ( ref_name .EQ. 'EXTR_OBJ_VAR' ) THEN
         iaddr_extr_obj_var = mapped_address

      ELSE IF ( ref_name .EQ. 'EXTR_STAR_VAR' ) THEN
         iaddr_extr_star_var = mapped_address

      ELSE IF ( ref_name .EQ. 'EXTRACT_MODE' ) THEN
         usr_extract_mode =  string

      ELSE IF ( ref_name .EQ. 'EXTRACTED_ARC' ) THEN
         iaddr_extracted_arc = mapped_address

      ELSE IF ( ref_name .EQ. 'EXTRACTED_OBJ' ) THEN
         iaddr_extracted_obj = mapped_address

      ELSE IF ( ref_name .EQ. 'EXTRACTED_STAR' ) THEN
         iaddr_extracted_star = mapped_address
      END IF
      GO TO 999

*  'F'
    6 IF ( REF_NAME .EQ. 'FFIELD' ) THEN
         IADDR_FFIELD = MAPPED_ADDRESS
         IADDR_FFIELDQ = MAPPED_QADDRESS
         IADDR_FFIELDE = MAPPED_EADDRESS
         IMG_FFIELD = STRING
         IF ( STATUS .NE. ECH__IS_ACCESSED )
     :      CSTR_RDCTN_FFIELD = STRING
         IF ( STRING .EQ. 'NONE' .OR. STRING .EQ. 'none' ) THEN
            USR_TUNE_NOFLAT = .TRUE.

         ELSE
            USR_TUNE_NOFLAT = .FALSE.
         END IF

      ELSE IF ( ref_name .EQ. 'F2D_DX_COORD' ) THEN
         iaddr_f2d_dx_coord = mapped_address

      ELSE IF ( ref_name .EQ. 'F2D_DY_COORD' ) THEN
         iaddr_f2d_dy_coord = mapped_address

      ELSE IF ( ref_name .EQ. 'F2D_F_OF_XY' ) THEN
         iaddr_f2d_f_of_xy = mapped_address

      ELSE IF ( ref_name .EQ. 'F2D_DEVIATION' ) THEN
         iaddr_f2d_deviation = mapped_address

      ELSE IF ( ref_name .EQ. 'F2D_FITTED_F' ) THEN
         iaddr_f2d_fitted_f = mapped_address

      ELSE IF ( ref_name .EQ. 'F2D_XV' ) THEN
         iaddr_f2d_xv = mapped_address

      ELSE IF ( ref_name .EQ. 'F2D_WEIGHTS' ) THEN
         iaddr_f2d_weights = mapped_address

      ELSE IF ( ref_name .EQ. 'F2D_WORK_NAG' ) THEN
         iaddr_f2d_work_nag = mapped_address

      ELSE IF ( ref_name .EQ. 'F2D_LINECENT' ) THEN
         iaddr_f2d_linecent = mapped_address

      ELSE IF ( ref_name .EQ. 'FITTED_FLAT' ) THEN
         iaddr_fitted_flat = mapped_address

      ELSE IF ( ref_name .EQ. 'FLAT_ERRORS' ) THEN
         iaddr_flat_errors = mapped_address

      ELSE IF ( ref_name .EQ. 'FITTED_WAVES' ) THEN
         iaddr_fitted_waves = mapped_address

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XINT' ) THEN
         iaddr_fit_work_xint = mapped_address

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XREAL' ) THEN
         iaddr_fit_work_xreal = mapped_address

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XREAL2' ) THEN
         iaddr_fit_work_xreal2 = mapped_address

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XREAL3' ) THEN
         iaddr_fit_work_xreal3 = mapped_address

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XREAL4' ) THEN
         iaddr_fit_work_xreal4 = mapped_address

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XDOUBLE2' ) THEN
         iaddr_fit_work_xdouble2 = mapped_address

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XDOUBLE' ) THEN
         iaddr_fit_work_xdouble = mapped_address

      ELSE IF ( ref_name .EQ. 'FIT_WORK_3XDOUBLE' ) THEN
         iaddr_fit_work_3xdouble = mapped_address

      ELSE IF ( ref_name .EQ. 'FLTFIT' ) THEN
         usr_fltfit = string

      ELSE IF ( ref_name .EQ. 'FITTED_SKY' ) THEN
         iaddr_fitted_sky = mapped_address

      ELSE IF ( ref_name .EQ. 'FSKY_ERRORS' ) THEN
         iaddr_fsky_errors = mapped_address

      ELSE IF ( ref_name .EQ. 'FITTED_SSKY' ) THEN
         iaddr_fitted_ssky = mapped_address

      ELSE IF ( ref_name .EQ. 'FSSKY_ERRORS' ) THEN
         iaddr_fssky_errors = mapped_address

      ELSE IF ( ref_name .EQ. 'FITTED_PFL' ) THEN
         iaddr_fitted_pfl = mapped_address

      ELSE IF ( ref_name .EQ. 'FIT_WAVES' ) THEN
         iaddr_fit_waves = mapped_address

      ELSE IF ( ref_name .EQ. 'FIT_WAVES2' ) THEN
         iaddr_fit_waves2 = mapped_address

      ELSE IF ( ref_name .EQ. 'FIT_WAVES_WORK' ) THEN
         iaddr_fit_waves_work = mapped_address
      END IF
      GO TO 999

*  'G'
    7 GO TO 999

*  'H'
    8 IF ( ref_name .EQ. 'HARD' ) THEN
         usr_hplot_device = string

      ELSE IF ( ref_name .EQ. 'HARDCOPY' ) THEN
         usr_hardcopy = boolean_value

      ELSE IF ( ref_name .EQ. 'HELIO_COR' ) THEN
         usr_helio_cor = value

      ELSE IF ( ref_name .EQ. 'HI_WAVE' ) THEN
         usr_hi_wave = value
      END IF
      GO TO 999

*  'I'
    9 IF ( ref_name .EQ. 'INPTIM' ) THEN
         iaddr_inptim = mapped_address
         iaddr_inptimq = mapped_qaddress
         iaddr_inptime = mapped_eaddress
         img_inptim = string
         IF ( status .NE. ECH__IS_ACCESSED ) THEN
            cstr_rdctn_inptim = string
         END IF

      ELSE IF ( ref_name .EQ. 'ID_COUNT' ) THEN
         iaddr_id_count = mapped_address

      ELSE IF ( ref_name .EQ. 'ID_WIDTHS' ) THEN
         iaddr_id_widths = mapped_address

      ELSE IF ( ref_name .EQ. 'ID_LINES' ) THEN
         iaddr_id_lines = mapped_address

      ELSE IF ( ref_name .EQ. 'ID_WAVES' ) THEN
         iaddr_id_waves = mapped_address

      ELSE IF ( ref_name .EQ. 'ID_STATUS' ) THEN
         iaddr_id_status = mapped_address
      END IF
      GO TO 999

*  'J'
   10 GO TO 999

*  'K'
   11 GO TO 999

*  'L'
   12 IF ( ref_name .EQ. 'LOC_BOX_SIZ' ) THEN
         usr_loc_box_siz = INT( value )

      ELSE IF ( ref_name .EQ. 'LOC_BOX_AVG' ) THEN
         usr_loc_box_avg = boolean_value

      ELSE IF ( ref_name .EQ. 'LOW_WAVE' ) THEN
         usr_low_wave = value
      END IF
      GO TO 999

*  'M'
   13 IF ( ref_name .EQ. 'MAX_DISPERSION' ) THEN
         usr_max_dispersion = value

      ELSE IF ( ref_name .EQ. 'MIN_DISPERSION' ) THEN
         usr_min_dispersion = value

      ELSE IF ( ref_name .EQ. 'MFILTER1' ) THEN
         iaddr_mfilter1 = mapped_address

      ELSE IF ( ref_name .EQ. 'MFILTER2' ) THEN
         iaddr_mfilter2 = mapped_address

      ELSE IF ( ref_name .EQ. 'MODEL_PROFILE' ) THEN
         iaddr_model_profile = mapped_address
      END IF
      GO TO 999

*  'N'
   14 IF ( ref_name .EQ. 'NX' .OR. ref_name .EQ. 'NY' ) THEN
         CONTINUE

      ELSE IF ( ref_name .EQ. 'NO_OF_ORDERS' ) THEN
         iaddr_no_of_orders = mapped_address
         CALL ECH_TRANSFER_INT(
     :        %VAL( iaddr_no_of_orders ), num_orders )
         usr_num_orders = num_orders

      ELSE IF ( ref_name .EQ. 'NX_PIXELS' ) THEN
         iaddr_nx_pixels = mapped_address
         IF ( nx .EQ. 0 )
     :      CALL ECH_TRANSFER_INT( %VAL ( iaddr_nx_pixels ), nx )

      ELSE IF ( ref_name .EQ. 'NY_PIXELS' ) THEN
         iaddr_ny_pixels = mapped_address
         IF ( ny .EQ. 0 )
     :      CALL ECH_TRANSFER_INT( %VAL ( iaddr_ny_pixels ), ny )

      ELSE IF ( ref_name .EQ. 'NUM_ORDERS' ) THEN
         usr_num_orders = INT( value )

      ELSE IF ( ref_name .EQ. 'NREF_FRAME' ) THEN
         iaddr_nref_frame = mapped_address

      ELSE IF ( ref_name .EQ. 'NO_OF_BINS' ) THEN
         iaddr_no_of_bins = mapped_address

      ELSE IF ( ref_name .EQ. 'NX_REBIN' ) THEN
         iaddr_nx_rebin = mapped_address
      END IF
      GO TO 999

*  'O'
   15 IF ( ref_name .EQ. 'OUTFLAT' ) THEN
         rdctn_file_outflat = string

      ELSE IF ( ref_name .EQ. 'ORDER_YPOS' ) THEN
         iaddr_order_ypos = mapped_address

      ELSE IF ( ref_name .EQ. 'OBJECT' ) THEN
         iaddr_object = mapped_address
         iaddr_objectq = mapped_qaddress
         iaddr_objecte = mapped_eaddress
         img_object = string
         IF ( status .NE. ECH__IS_ACCESSED )
     :      cstr_rdctn_object = string

      ELSE IF ( ref_name .EQ. 'ORDER_NUMBER' )   THEN
         usr_order_number = INT( value )

      ELSE IF ( ref_name .EQ. 'OBJ_BELOW' ) THEN
         iaddr_obj_below = mapped_address

      ELSE IF ( ref_name .EQ. 'OBJ_ABOVE' ) THEN
         iaddr_obj_above = mapped_address

      ELSE IF ( ref_name .EQ. 'OBJECT_PROFILE' ) THEN
         iaddr_object_profile = mapped_address

      ELSE IF ( ref_name .EQ. 'OPTEXT_MODE' ) THEN
         usr_optext_mode = string

      ELSE IF ( ref_name .EQ. 'OBJ_SKY_GAP' ) THEN
         usr_obj_sky_gap = INT( value )

      ELSE IF ( ref_name .EQ. 'OBS_LINES' ) THEN
         iaddr_obs_lines = mapped_address

      ELSE IF ( ref_name .EQ. 'OBS_INTEN' ) THEN
         iaddr_obs_inten = mapped_address

      ELSE IF ( ref_name .EQ. 'ORDER_IDNUM' ) THEN
         iaddr_order_idnum = mapped_address

      ELSE IF ( ref_name .EQ. 'ORDER_SLOPE' ) THEN
         iaddr_order_slope = mapped_address

      ELSE IF ( ref_name .EQ. 'OBJ_MASK' ) THEN
         iaddr_obj_mask = mapped_address

      ELSE IF ( ref_name .EQ. 'OUTPUT_IMAGE' ) THEN
         iaddr_output_image = mapped_address
           img_output_image = string

      ELSE IF ( ref_name .EQ. 'OBJFIT' ) THEN
         usr_objfit = string
      END IF
      GO TO 999

*  'P'
   16 IF ( ref_name .EQ. 'PHOTON_TO_ADU' ) THEN
         iaddr_photon_to_adu = mapped_address

      ELSE IF ( ref_name .EQ. 'PFL_MODE' ) THEN
         usr_pfl_mode =  string

      ELSE IF ( ref_name .EQ. 'PFL_INTERACT' ) THEN
         usr_pfl_interact = boolean_value

      ELSE IF ( ref_name .EQ. 'PLY_ORDERINCS' ) THEN
         iaddr_ply_orderincs = mapped_address

      ELSE IF ( ref_name .EQ. 'PERPIXEL_ERRORS' ) THEN
         iaddr_error_image = mapped_address
      END IF
      GO TO 999

*  'Q'
   17 IF ( ref_name .EQ. 'QUALITY' ) THEN
         iaddr_data_quality = mapped_address
      END IF
      GO TO 999

*  'R'
   18 IF ( ref_name( :6 ) .EQ. 'RDCTN_' ) THEN
         CONTINUE

      ELSE IF ( ref_name .EQ. 'READOUT_NOISE' ) THEN
         iaddr_readout_noise = mapped_address

      ELSE IF ( ref_name .EQ. 'REF_LINE_FWHM' ) THEN
         iaddr_ref_line_fwhm = mapped_address

      ELSE IF ( ref_name .EQ. 'REF_SPECTRUM' ) THEN
         iaddr_ref_spectrum = mapped_address

      ELSE IF ( ref_name .EQ. 'REF_CONTINUUM' ) THEN
         iaddr_ref_continuum = mapped_address

      ELSE IF ( ref_name .EQ. 'REF_LINES' ) THEN
         iaddr_ref_lines = mapped_address

      ELSE IF ( ref_name .EQ. 'REBIN_WORK' ) THEN
         iaddr_rebin_work = mapped_address

      ELSE IF ( ref_name .EQ. 'RBNOBJ' ) THEN
         iaddr_rebin_object = mapped_address
         iaddr_rebin_objecte = mapped_eaddress
         iaddr_rebin_objectq = mapped_qaddress
         img_rbnobj = string

      ELSE IF ( ref_name .EQ. 'RESULT_DATAX' .OR.
     :          ref_name .EQ. 'RESULT_WAVES' .OR.
     :          ref_name .EQ. 'RESULT_SCRWAVES' .OR.
     :          ref_name .EQ. 'RESULT_FITWAVE' .OR.
     :          ref_name .EQ. 'RESULT_FITWAVES' .OR.
     :          ref_name .EQ. 'RESULT_SCRWAVE' .OR.
     :          ref_name .EQ. 'RESULT_WAVESCALE' ) THEN
         iaddr_result_datax = mapped_address

      ELSE IF ( ref_name .EQ. 'RESULT_DATAY' .OR.
     :          ref_name .EQ. 'RESULT_ONUM' .OR.
     :          ref_name .EQ. 'RESULT_ONUMS' ) THEN
         iaddr_result_datay = mapped_address

      ELSE IF ( ref_name .EQ. 'RESULT_DATAZ'  .OR.
     :          ref_name .EQ. 'RESULT_ORDER' .OR.
     :          ref_name .EQ. 'RESULT_ORDERS' .OR.
     :          ref_name .EQ. 'RESULT_SPECTRUM' .OR.
     :          ref_name .EQ. 'RESULT_SCRORDER' .OR.
     :          ref_name .EQ. 'RESULT_SCRORDERS' ) THEN
         iaddr_result_dataz = mapped_address

      ELSE IF ( ref_name .EQ. 'RESULT_ERRORS'  .OR.
     :          ref_name .EQ. 'RESULT_ERROR' .OR.
     :          ref_name .EQ. 'RESULT_SCRERR' .OR.
     :          ref_name .EQ. 'RESULT_SPECTERR' .OR.
     :          ref_name .EQ. 'RESULT_SCRERROR' ) THEN
         iaddr_result_errors = mapped_address

      ELSE IF ( ref_name .EQ. 'RESULT_TYPE' ) THEN
         usr_result_type = string

      ELSE IF ( ref_name .EQ. 'RESULT_FORMAT' ) THEN
         usr_result_format = string

      ELSE IF ( ref_name .EQ. 'RESULT_LABELX' ) THEN
         cstr_result_labelx =  string

      ELSE IF ( ref_name .EQ. 'RESULT_LABELY' ) THEN
         cstr_result_labely =  string

      ELSE IF ( ref_name .EQ. 'RESULT_LABELZ' ) THEN
         cstr_result_labelz =  string

      ELSE IF ( ref_name .EQ. 'REBIN_ORDER' ) THEN
         iaddr_rebin_order = mapped_address

      ELSE IF ( ref_name .EQ. 'REBIN_QUALITY' ) THEN
         iaddr_rebin_quality = mapped_address

      ELSE IF ( ref_name .EQ. 'REBIN_EORDER' ) THEN
         iaddr_rebin_eorder = mapped_address

      ELSE IF ( ref_name .EQ. 'REBIN_ARC' ) THEN
         iaddr_rebin_arc = mapped_address

      ELSE IF ( ref_name .EQ. 'REBIN_EARC' ) THEN
         iaddr_rebin_earc = mapped_address
      END IF
      GO TO 999

*  'S'
   19 IF ( ref_name .EQ. 'SOFT' ) THEN
         usr_splot_device = string

      ELSE IF ( ref_name .EQ. 'SO_FAR' ) THEN
         iaddr_so_far = mapped_address

      ELSE IF ( ref_name .EQ. 'STACK' ) THEN
         out_stack = string

      ELSE IF ( ref_name .EQ. 'STAR' ) THEN
         iaddr_star = mapped_address
         iaddr_starq = mapped_qaddress
         iaddr_stare = mapped_eaddress
         img_star = string
         IF ( status .NE. ECH__IS_ACCESSED )
     :      cstr_rdctn_star = string

      ELSE IF ( ref_name .EQ. 'SLITIM' ) THEN
         iaddr_slitim = mapped_address
         iaddr_slitimq = mapped_qaddress
         iaddr_slitime = mapped_eaddress
         img_slitim = string
         IF ( status .NE. ECH__IS_ACCESSED )
     :      cstr_rdctn_slitim = string

      ELSE IF ( ref_name .EQ. 'SCRNCHD_WAVES' ) THEN
         iaddr_scrnchd_waves = mapped_address

      ELSE IF ( ref_name .EQ. 'SCRNCHD_ARC' ) THEN
         iaddr_scrnchd_arc = mapped_address

      ELSE IF ( ref_name .EQ. 'SCRNCHD_OBJ' ) THEN
         iaddr_scrnchd_obj = mapped_address

      ELSE IF ( ref_name .EQ. 'SCRNCHD_STAR' ) THEN
         iaddr_scrnchd_star = mapped_address

      ELSE IF ( ref_name .EQ. 'SCRNCHD_ARCV' ) THEN
         iaddr_scrnchd_arcv = mapped_address

      ELSE IF ( ref_name .EQ. 'SCRNCHD_OBJV' ) THEN
         iaddr_scrnchd_objv = mapped_address

      ELSE IF ( ref_name .EQ. 'SCRNCHD_STRV' ) THEN
         iaddr_scrnchd_strv = mapped_address

      ELSE IF ( ref_name .EQ. 'SET_WSCALE' ) THEN
         usr_set_wscale = boolean_value

      ELSE IF ( ref_name .EQ. 'SKY_SPECTRUM' ) THEN
         iaddr_sky_spectrum = mapped_address

      ELSE IF ( ref_name .EQ. 'SSKY_SPECTRUM' ) THEN
         iaddr_ssky_spectrum = mapped_address

      ELSE IF ( ref_name .EQ. 'SKY_LINES' ) THEN
         iaddr_sky_lines = mapped_address

      ELSE IF ( ref_name .EQ. 'SCRUNCH_TYPE' ) THEN
         usr_scrunch_type = string

      ELSE IF ( ref_name .EQ. 'STAR_BELOW' ) THEN
         iaddr_star_below = mapped_address

      ELSE IF ( ref_name .EQ. 'STAR_ABOVE' ) THEN
         iaddr_star_above = mapped_address

      ELSE IF ( ref_name .EQ. 'STAR_PROFILE' ) THEN
         iaddr_star_profile = mapped_address

      ELSE IF ( ref_name .EQ. 'START_WAVE' ) THEN
         usr_start_wave = value

      ELSE IF ( ref_name .EQ. 'SMOOTH_DIAG' ) THEN
         iaddr_smooth_diag = mapped_address

      ELSE IF ( ref_name .EQ. 'SKY_MASK' ) THEN
         iaddr_sky_mask = mapped_address

      ELSE IF ( ref_name .EQ. 'SKY_VARIANCE' ) THEN
         iaddr_sky_variance = mapped_address

      ELSE IF ( ref_name .EQ. 'SSKY_VARIANCE' ) THEN
         iaddr_ssky_variance = mapped_address

      ELSE IF ( ref_name .EQ. 'STD_SPECTRUM' ) THEN
         iaddr_fspectrum = mapped_address

      ELSE IF ( ref_name .EQ. 'SKYFIT' ) THEN
         usr_skyfit = string
      END IF
      GO TO 999

*  'T'
   20 IF ( ref_name .EQ. 'TRACE_PATH' ) THEN
         iaddr_trace_path = mapped_address

      ELSE IF ( REF_NAME .EQ. 'TRACIM' ) THEN
         IADDR_TRACIM = MAPPED_ADDRESS
         IADDR_TRACIMQ = MAPPED_QADDRESS
         IADDR_TRACIME = MAPPED_EADDRESS
         IMG_TRACIM = STRING
         IF ( STATUS .NE. ECH__IS_ACCESSED )
     :      CSTR_RDCTN_TRACIM = STRING

      ELSE IF ( ref_name .EQ. 'TRC_POLY' ) THEN
         iaddr_trc_poly = mapped_address

      ELSE IF ( ref_name .EQ. 'TRACE' ) THEN
         iaddr_trace = mapped_address

      ELSE IF ( ref_name .EQ. 'TRC_NPOLY' ) THEN
         usr_trc_npoly = INT( value )

      ELSE IF ( ref_name .EQ. 'TRC_IN_DEV' ) THEN
         iaddr_trc_in_dev = mapped_address

      ELSE IF ( ref_name .EQ. 'TRC_OUT_DEV' ) THEN
         iaddr_trc_out_dev = mapped_address

      ELSE IF ( ref_name .EQ. 'TRC_CLIPPED' ) THEN
         iaddr_trc_clipped = mapped_address

      ELSE IF ( ref_name .EQ. 'TRC_INTERACT' ) THEN
         usr_trc_interact = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_SKYINTER' ) THEN
         usr_tune_skyinter = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_FCHECK' ) THEN
         usr_tune_fcheck = boolean_value

      ELSE IF ( ref_name .EQ. 'TRACE_WIDTH' ) THEN
         iaddr_trace_width = mapped_address

      ELSE IF ( ref_name .EQ. 'TUNE_XBOX' ) THEN
         usr_tune_xbox = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_MAX2DPNTS' ) THEN
         usr_tune_max2dpnts = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_MAXLINES' ) THEN
         usr_tune_maxlines = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_MAX2DPLY' ) THEN
         usr_tune_max2dply = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_PFSAMP' ) THEN
         usr_tune_pfsamp = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_MAXWPLY' ) THEN
         usr_tune_maxwply = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_MAXPOLY' ) THEN
         usr_tune_maxpoly = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_TWTHR' ) THEN
         usr_tune_twthr = value

      ELSE IF ( ref_name .EQ. 'TUNE_AUTLOC' ) THEN
         usr_tune_autloc = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_MXSMP' ) THEN
         usr_tune_mxsmp = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_MXBADSMP' ) THEN
         usr_tune_mxbadsmp = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_CLPMXDEV' ) THEN
         usr_tune_clpmxdev = value

      ELSE IF ( ref_name .EQ. 'TUNE_CLPBY' ) THEN
         usr_tune_clpby = INT( value )

      ELSE IF ( ref_name .EQ. 'TWO_D_DLAMBDA' ) THEN
         iaddr_two_d_dlambda = mapped_address

      ELSE IF ( ref_name .EQ. 'TRACE_MODE' ) THEN
         usr_trace_mode = string

      ELSE IF ( ref_name .EQ. 'TRACE_RE_FITS' ) THEN
         iaddr_trace_re_fits = mapped_address

      ELSE IF ( ref_name .EQ. 'TUNE_TRCNS' ) THEN
         usr_tune_trcns = value

      ELSE IF ( ref_name .EQ. 'TUNE_CNSDEV' ) THEN
         usr_tune_cnsdev = value

      ELSE IF ( ref_name .EQ. 'TUNE_SKYPOLY' ) THEN
         usr_tune_skypoly = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_SKYREJ' ) THEN
         usr_tune_skyrej = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_SKYRTHR' ) THEN
         usr_tune_skyrthr = value

      ELSE IF ( ref_name .EQ. 'TUNE_OBJREJ' ) THEN
         usr_tune_objrej = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_OBJRTHR' ) THEN
         usr_tune_objrthr = value

      ELSE IF ( ref_name .EQ. 'TUNE_MXSKYPIX' ) THEN
         usr_tune_mxskypix = 2 * ( INT( value ) / 2 ) + 1

      ELSE IF ( ref_name .EQ. 'TUNE_OBJPOLY' ) THEN
         usr_tune_objpoly = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_USE_NXF' ) THEN
         usr_tune_use_nxf = value

      ELSE IF ( ref_name .EQ. 'TUNE_PFLSSAMP' ) THEN
         usr_tune_pflssamp = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_DEKTHR' ) THEN
         usr_tune_dekthr = value

      ELSE IF ( ref_name .EQ. 'TUNE_SKYHILIM' ) THEN
         usr_tune_skyhilim = value

      ELSE IF ( ref_name .EQ. 'TUNE_RFLNTHR' ) THEN
         usr_tune_rflnthr = value

      ELSE IF ( ref_name .EQ. 'TUNE_MAXRFLN' ) THEN
         usr_tune_maxrfln = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_SKVRCORR' ) THEN
         usr_tune_skvrcorr = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_CRCLEAN' ) THEN
         usr_tune_crclean = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_IDMXDIF' ) THEN
         usr_tune_idmxdif = value

      ELSE IF ( ref_name .EQ. 'TUNE_IDMDLT' ) THEN
         usr_tune_idmdlt = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_IDSDLT' ) THEN
         usr_tune_idsdlt = INT  ( value )

      ELSE IF ( ref_name .EQ. 'TUNE_IDSTRNG' ) THEN
         usr_tune_idstrng = value

      ELSE IF ( ref_name .EQ. 'TUNE_IDINMX' ) THEN
         usr_tune_idinmx = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_IDINMN' ) THEN
         usr_tune_idinmN = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_FINCPLY' ) THEN
         usr_tune_fincply = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_DSGMTHR' ) THEN
         usr_tune_dsgmthr = value

      ELSE IF ( ref_name .EQ. 'TUNE_DPRBTHR' ) THEN
         usr_tune_dprbthr = value

      ELSE IF ( ref_name .EQ. 'TUNE_FFNXPLY' ) THEN
         usr_tune_ffnxply = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_FFNYPLY' ) THEN
         usr_tune_ffnyply = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_FFNXREJ' ) THEN
         usr_tune_ffnxrej = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_FFNYREJ' ) THEN
         usr_tune_ffnyrej = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_FFTHRESH' ) THEN
         usr_tune_ffthresh = value

      ELSE IF ( ref_name .EQ. 'TUNE_SKEW' ) THEN
         usr_tune_skew = value

      ELSE IF ( ref_name .EQ. 'TUNE_LOG' ) THEN
         usr_tune_log = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_FLUX' ) THEN
         usr_tune_flux = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_INTR' ) THEN
         usr_tune_intr = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_QUAD' ) THEN
         usr_tune_quad = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_SCRMODE' ) THEN
         usr_tune_scrmode = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_SCRADD' ) THEN
         usr_tune_scradd = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_SCFRACT' ) THEN
         usr_tune_scfract = value

      ELSE IF ( ref_name .EQ. 'TUNE_CRXBOX' ) THEN
         usr_tune_crxbox = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_CRYBOX' ) THEN
         usr_tune_crybox = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_CRMAX' ) THEN
         usr_tune_crmax = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_CRINTER' ) THEN
         usr_tune_crinter = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_FFINTER' ) THEN
         usr_tune_ffinter = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_DIAGNOSE' ) THEN
         usr_tune_diagnose = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_REVCHK' ) THEN
         usr_tune_revchk = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_PREBAL' ) THEN
         usr_tune_prebal = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_DB_SCOPE' ) THEN
         usr_tune_db_scope = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_NOFLAT' ) THEN
         usr_tune_noflat = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_NOARC' ) THEN
         usr_tune_noarc = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_BATCH' ) THEN
         usr_tune_batch = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_QUICK' ) THEN
         usr_tune_quick = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_CRTRC' ) THEN
         usr_tune_crtrc = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_OBJBLW' ) THEN
         usr_tune_objblw = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_OBJABV' ) THEN
         usr_tune_objabv = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_DEKBLW' ) THEN
         usr_tune_dekblw = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_DEKABV' ) THEN
         usr_tune_dekabv = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_PAGE' ) THEN
         usr_tune_page = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_MINCR' ) THEN
         usr_tune_mincr = value

      ELSE IF ( ref_name .EQ. 'TUNE_SATRTN' ) THEN
         usr_tune_satrtn = value

      ELSE IF ( ref_name .EQ. 'TUNE_REPORT' ) THEN
         usr_tune_report = string

      ELSE IF ( ref_name .EQ. 'TUNE_CLONE' ) THEN
         usr_tune_clone = string

      ELSE IF ( ref_name .EQ. 'TUNE_SKYSIM' ) THEN
         usr_tune_skysim = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_FFLMED' ) THEN
         usr_tune_fflmed = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_FFSUBSMP' ) THEN
         usr_tune_ffsubsmp = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_SKYLTHR' ) THEN
         usr_tune_skylthr = value

      ELSE IF ( ref_name .EQ. 'TUNE_SKYXPLY' ) THEN
         usr_tune_skyxply = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_SKYLINW' ) THEN
         usr_tune_skylinw = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_FFLSMP' ) THEN
         usr_tune_fflsmp = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_ARCHIVE' ) THEN
         usr_tune_archive = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_PARTORD' ) THEN
         usr_tune_partord = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_USEAAA' ) THEN
         usr_tune_useaaa = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_AAACODE' ) THEN
         usr_tune_aaacode = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_FIBRES' ) THEN
         usr_tune_fibres = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_AIRTOVAC' ) THEN
         usr_tune_airtovac = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_IUE' ) THEN
         usr_tune_iue = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_MRGMINX' ) THEN
         usr_tune_mrgminx = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_MRGMAXX' ) THEN
         usr_tune_mrgmaxx = INT( value )

      ELSE IF ( ref_name .EQ. 'TUNE_BLZRSET' ) THEN
         usr_tune_blzrset = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_YBLAZE' ) THEN
         usr_tune_yblaze = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_MERGE' ) THEN
         usr_tune_merge = boolean_value

      ELSE IF ( ref_name .EQ. 'TUNE_AUTOMATE' ) THEN
         CONTINUE

      ELSE IF ( ref_name .EQ. 'TUNE_MRGWGHT' ) THEN
         usr_tune_mrgwght = string

      ELSE IF ( ref_name .EQ. 'TRCFIT' ) THEN
         usr_trcfit = string

      ELSE IF ( ref_name .EQ. 'TUNE_XZONE' ) THEN
         CONTINUE

      ELSE IF ( ref_name .EQ. 'TUNE_YZONE' ) THEN
         CONTINUE
      END IF
      GO TO 999

*  'U'
   21 IF ( ref_name .EQ. 'USE_MEDIAN' ) THEN
         usr_use_median = boolean_value
      END IF
      GO TO 999

*  'V'
   22 IF ( ref_name .EQ. 'VAR_MATRIX' ) THEN
         iaddr_var_matrix = mapped_address
      END IF
      GO TO 999

*  'W'
   23 IF ( ref_name .EQ. 'W_POLY' ) THEN
         iaddr_w_poly = mapped_address

      ELSE IF ( ref_name .EQ. 'WAVFIT' ) THEN
         usr_wavfit = string

      ELSE IF ( ref_name .EQ. 'WPFL_TOTAL' ) THEN
         iaddr_wpfl_total = mapped_address

      ELSE IF ( ref_name .EQ. 'WPFL_TCOUNT' ) THEN
         iaddr_wpfl_tcount = mapped_address

      ELSE IF ( ref_name .EQ. 'W_NPOLY' ) THEN
         usr_w_npoly = INT( value )

      ELSE IF ( ref_name .EQ. 'WID_POLY' ) THEN
         iaddr_wid_poly = mapped_address

      ELSE IF ( ref_name .EQ. 'WAVELENGTH' ) THEN
         iaddr_wavelength = mapped_address

      ELSE IF ( ref_name .EQ. 'W_POLY_2D' ) THEN
         iaddr_w_poly_2d = mapped_address

      ELSE IF ( ref_name .EQ. 'W2_NX_POLY' ) THEN
         usr_w2_nx_poly = INT( value )

      ELSE IF ( ref_name .EQ. 'W2_NY_POLY' ) THEN
         usr_w2_ny_poly = INT( value )

      ELSE IF ( ref_name .EQ. 'WLEFT_OFFSET' ) THEN
         iaddr_wleft_offset = mapped_address

      ELSE IF ( ref_name .EQ. 'WRIGHT_OFFSET' ) THEN
         iaddr_wright_offset = mapped_address

      ELSE IF ( ref_name .EQ. 'WNEXT_INDEX' ) THEN
         iaddr_wnext_index = mapped_address

      ELSE IF ( ref_name .EQ. 'WPREV_INDEX' ) THEN
         iaddr_wprev_index = mapped_address

      ELSE IF ( ref_name .EQ. 'WRATIOS' ) THEN
         iaddr_wratios = mapped_address

      ELSE IF ( ref_name .EQ. 'WSEAR_START' ) THEN
         iaddr_wsear_start = mapped_address

      ELSE IF ( ref_name .EQ. 'WSEAR_END' ) THEN
         iaddr_wsear_end = mapped_address

      ELSE IF ( ref_name .EQ. 'WVSCALE_INDEX' ) THEN
         iaddr_wvscale_index = mapped_address

      ELSE IF ( ref_name .EQ. 'WORDER_NUMS' ) THEN
         iaddr_worder_nums = mapped_address

      ELSE IF ( ref_name .EQ. 'WFDB_DATABASE' ) THEN
         iaddr_wfdb_database = mapped_address

      ELSE IF ( ref_name .EQ. 'WFDB_LEFT' ) THEN
         iaddr_wfdb_left = mapped_address

      ELSE IF ( ref_name .EQ. 'WFDB_RIGHT' ) THEN
         iaddr_wfdb_right = mapped_address
      END IF
      GO TO 999

*  'X'
   24 IF ( ref_name .EQ. 'X_TRACE_COORD' ) THEN
         iaddr_x_trace_coord = mapped_address
      END IF
      GO TO 999

*  'Y'
   25 IF ( ref_name .EQ. 'Y_TRACE_COORD' ) THEN
         iaddr_y_trace_coord = mapped_address
      END IF
      GO TO 999

*  Other...
   26 IF ( ref_name .EQ. '1D_SPECTRUM' ) THEN
         iaddr_spectrum = mapped_address

      ELSE IF ( ref_name .EQ. '2D_INTERACT' ) THEN
         usr_2d_interact = boolean_value

      ELSE
         REPORT_STRING = ' Setting up a foreign parameter for ' //
     :         ref_name
         CALL ECH_REPORT( 0, REPORT_STRING )
         found = .FALSE.
         DO i = 1, foreign_par_count
            IF ( ref_name .EQ. foreign_par( i ) ) THEN
               foreign_par_value( i ) = INT( value )
               found = .TRUE.
            END IF
         END DO
         IF ( .NOT. found ) THEN
            foreign_par_count = foreign_par_count + 1
            IF ( foreign_par_count .GT. max_foreign_pars ) THEN
               CALL ECH_REPORT ( 0,
     :              ' ERROR : Too many foreign index-parameters' )
               status = ECH__ABORT_OPTION

            ELSE
               foreign_par_value( i ) = INT( value )
               foreign_par( i ) = ref_name
            END IF
         END IF
      END IF

  999 CONTINUE

      IF ( usr_tune_client ) THEN
         iobj = ECH_OBJ_IND( ref_name )
         IF ( .NOT. is_workspace(iobj) ) THEN
            DATA_ADDRESS = MAPPED_ADDRESS
            DATA_EADDRESS = MAPPED_EADDRESS
            DATA_QADDRESS = MAPPED_QADDRESS
            IF ( DATA_ADDRESS .LE. 0 ) THEN
               VALARRY( 1 ) = VALUE
               BOOLVALARRY( 1 ) = BOOLEAN_VALUE
#if HAVE_PERCENTLOC
               DATA_EADDRESS = %LOC( VALARRY )
               DATA_QADDRESS = %LOC( BOOLVALARRY )
#elif HAVE_INTRINSIC_LOC
               DATA_EADDRESS = LOC( VALARRY )
               DATA_QADDRESS = LOC( BOOLVALARRY )
#else
          Do not have LOC or %LOC
#endif
            END IF
            CALL ECH_ACCESS_OBJECT( ref_name, 'SEND', ' ', 0,
     :           data_address, data_eaddress, dumdim, 7,
     :           data_qaddress, string, status )
         END IF
      END IF

      END


      SUBROUTINE ECH_TRANSFER_INT( FROM, TO )
*+
*-
      IMPLICIT NONE

      INTEGER FROM
      INTEGER TO
*.
      TO = FROM

      END
