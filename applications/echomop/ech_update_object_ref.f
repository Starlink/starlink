      SUBROUTINE ECH_UPDATE_OBJECT_REF(
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
*     ECHOMOP - ECH_UPDATE_OBJECT_REF

*  Purpose:
*     Update common variable-object mapping.

*  Description:
*     Update an object reference (for mapped objects), a value (always REAL)
*     or a string variable depending upon the object.

*  Invocation:
*     CALL ECH_UPDATE_OBJECT_REF(
*     :    REF_NAME,
*     :    MAPPED_ADDRESS,
*     :    MAPPED_QADDRESS,
*     :    MAPPED_EADDRESS,
*     :    STRING,
*     :    VALUE,
*     :    BOOLEAN_VALUE,
*     :    STATUS
*     :   )

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     01-JUL-1996 (MJC):
*       Added prologue.  Some dereferencing so that we can get
*       values for mapped objects.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables and Constants:
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_MAPPING.INC'
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
      INTEGER I
      INTEGER INDEX_NUMBER
      INTEGER ILOOP

      LOGICAL FOUND
      CHARACTER*33 COMP1
      CHARACTER*33 COMP2

*  Functions Called:
      REAL ECH_DEREF_REAL
      INTEGER ECH_DEREF_INT
*.

*  Potentially dynamic parameters (ie IDX_ array indices) are
*  checked first as they will be often used.
      IF ( REF_NAME ( :4 ) .EQ. 'IDX_' ) THEN
         INDEX_NUMBER = 0
         COMP1 = REF_NAME // ' '
         DO ILOOP = 1, DYNAMIC_INDICES_USED
            COMP2 = DYNAMIC_INDEX_NAME( ILOOP ) // ' '
            IF ( COMP1 .EQ. COMP2 ) THEN
               INDEX_NUMBER = ILOOP
               GO TO 100
            END IF
         END DO
  100    CONTINUE
         IF ( INDEX_NUMBER .EQ. 0 ) THEN
            REPORT_STRING = ' Uninitialised dynamic index:' //
     :            ' Mapping all ' // REF_NAME
            CALL ECH_REPORT( 0, REPORT_STRING )
            VALUE = 0

         ELSE
            VALUE = FLOAT( DYNAMIC_INDEX_VALUE( INDEX_NUMBER ) )
         END IF
         GO TO 999
      END IF

*  Each 'required_object' has its own piece of code.  The required
*  objects are defined at initialisation in ECH_DEFINE_MODULES.
      IF ( ICHAR( REF_NAME( :1 ) ) .LT. 65 ) THEN
         GO TO 26

      ELSE
         GO TO ( 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13,
     :          14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26 ),
     :         ICHAR( REF_NAME( :1 ) ) - 64
      END IF

*  'A'
    1 IF ( ref_name .EQ. 'ARC' .OR.
     :         ( ref_name( :3 ) .EQ. 'ARC' .AND.
     :          ref_name(4:4) .GE. '0'  .AND.
     :          ref_name(4:4) .LE. '9' ) )  THEN
         mapped_address = iaddr_arc
         mapped_qaddress = iaddr_arcq
         mapped_eaddress = iaddr_arce
         string = img_arc
         IF ( ref_name .EQ. 'ARC' ) string = cstr_rdctn_arc

      ELSE IF ( ref_name .EQ. 'ARC_TYPE' ) THEN
         string = usr_arc_type

      ELSE IF ( ref_name .EQ. 'ASCII_FILE' ) THEN
         string = out_file

      ELSE IF ( ref_name .EQ. 'AUTO_ID' ) THEN
         boolean_value = usr_id_interactive

      ELSE IF ( ref_name .EQ. 'AVG_SPECTRUM' ) THEN
         mapped_address = iaddr_avg_spectrum
      END IF
      GO TO 999

*  'B'
    2 IF ( ref_name .EQ. 'BAD_COLS' ) THEN
         mapped_address = iaddr_bad_cols

      ELSE IF ( ref_name .EQ. 'BAD_ORDER' ) THEN
         value = FLOAT( usr_bad_order )

      ELSE IF ( ref_name .EQ. 'BAD_ROWS' ) THEN
         mapped_address = iaddr_bad_rows

      ELSE IF ( ref_name .EQ. 'BIN_SIZE' ) THEN
         value = usr_bin_size

      ELSE IF ( ref_name .EQ. 'BLAZE_MEDIANS' ) THEN
         mapped_address = iaddr_blaze_medians

      ELSE IF ( ref_name .EQ. 'BLAZE_SPECT' ) THEN
         mapped_address = iaddr_blaze_spect

      ELSE IF ( ref_name .EQ. 'BLZ_INTERACT' ) THEN
         boolean_value = usr_blz_interact

      ELSE IF ( ref_name .EQ. 'BLZ_NPOLY' )  THEN
         value = FLOAT( usr_blz_npoly )

      ELSE IF ( ref_name .EQ. 'BLZFIT' ) THEN
         string = usr_blzfit
      END IF
      GO TO 999

*  'C'
    3 IF ( ref_name .EQ. 'CALOBJ_MASK' ) THEN
         mapped_address = iaddr_calobj_mask

      ELSE IF ( ref_name .EQ. 'CALSKY_MASK' ) THEN
         mapped_address = iaddr_calsky_mask

      ELSE IF ( ref_name .EQ. 'CENTRAL_ONUM' ) THEN
         mapped_address = iaddr_central_onum
         MAPPED_ADDRESS = IADDR_CENTRAL_ONUM
         IF ( IADDR_CENTRAL_ONUM .NE. 0 ) THEN
            VALUE = FLOAT( ECH_DEREF_INT( %VAL( IADDR_CENTRAL_ONUM ) ) )
         END IF

      ELSE IF ( ref_name .EQ. 'CENTRAL_WAVE' ) THEN
         MAPPED_ADDRESS = IADDR_CENTRAL_WAVE
         IF ( IADDR_CENTRAL_WAVE .NE. 0 ) THEN
            VALUE = ECH_DEREF_REAL( %VAL( IADDR_CENTRAL_WAVE ) )
         END IF

      ELSE IF ( ref_name .EQ. 'CONTIN_PROFILE' ) THEN
         mapped_address = iaddr_contin_profile

      ELSE IF ( ref_name .EQ. 'CONVOLVED' ) THEN
         mapped_address = iaddr_convolved

      ELSE IF ( ref_name .EQ. 'CONVOLVED2' ) THEN
         mapped_address = iaddr_convolved2

      ELSE IF ( ref_name .EQ. 'CORRELATE' ) THEN
         mapped_address = iaddr_correlate

      ELSE IF ( ref_name .EQ. 'CORREL_COUNT' ) THEN
         mapped_address = iaddr_correl_count

      ELSE IF ( ref_name .EQ. 'CRX_MEDIAN' ) THEN
         mapped_address = iaddr_xmedtmp

      ELSE IF ( ref_name .EQ. 'CRY_MEDIAN' ) THEN
         mapped_address = iaddr_ymedtmp
      END IF
      GO TO 999

*  'D'
    4 IF ( ref_name .EQ. 'DECOS_DATA' ) THEN
         mapped_address = iaddr_decos_data

      ELSE IF ( ref_name .EQ. 'DECOS_GAUSS' ) THEN
         mapped_address = iaddr_decos_gauss

      ELSE IF ( ref_name .EQ. 'DECOS_INDEX_X' ) THEN
         mapped_address = iaddr_decos_index_x

      ELSE IF ( ref_name .EQ. 'DECOS_INDEX_Y' ) THEN
         mapped_address = iaddr_decos_index_y

      ELSE IF ( ref_name .EQ. 'DECOS_SDATA' ) THEN
         mapped_address = iaddr_decos_sdata

      ELSE IF ( ref_name .EQ. 'DECOS_XAXIS' ) THEN
         mapped_address = iaddr_decos_xaxis

      ELSE IF ( ref_name .EQ. 'DECOS_YAXIS' ) THEN
         mapped_address = iaddr_decos_yaxis

      ELSE IF ( ref_name .EQ. 'DEK_ABOVE' ) THEN
         mapped_address = iaddr_dek_above

      ELSE IF ( ref_name .EQ. 'DEK_BELOW' ) THEN
         mapped_address = iaddr_dek_below

      ELSE IF ( ref_name .EQ. 'DIAGONAL' ) THEN
         mapped_address = iaddr_diagonal

      ELSE IF ( ref_name .EQ. 'DIAGONAL2' ) THEN
         mapped_address = iaddr_diagonal2

      ELSE IF ( ref_name .EQ. 'DISPLAY' ) THEN
         boolean_value = usr_display

      ELSE IF ( ref_name .EQ. 'DSAMPLE_XCOORD' ) THEN
         mapped_address = iaddr_dsample_xcoord

      ELSE IF ( ref_name .EQ. 'DSAMPLE_YCOORD' ) THEN
         mapped_address = iaddr_dsample_ycoord
      END IF
      GO TO 999

*  'E'
    5 IF ( ref_name .EQ. 'ECH_RDCTN' )  THEN
         string = rdctn_file_main

      ELSE IF ( ref_name .EQ. 'ECH_RDUCD' )  THEN
         string = rdctn_file_rducd

      ELSE IF ( ref_name .EQ. 'ECH_ECHAR' )  THEN
         string = rdctn_file_echarc

      ELSE IF ( ref_name .EQ. 'ECH_FTRDB' )  THEN
         string = rdctn_file_ftrdb

      ELSE IF ( ref_name .EQ. 'ECHARC_LABELX' ) THEN
         string = cstr_echarc_labelx

      ELSE IF ( ref_name .EQ. 'ECHARC_LABELY' ) THEN
         string = cstr_echarc_labely

      ELSE IF ( ref_name .EQ. 'ECHARC_LABELZ' ) THEN
         string = cstr_echarc_labelz

      ELSE IF ( ref_name .EQ. 'ECHARC_ORDNUM' ) THEN
         mapped_address = iaddr_echarc_datay

      ELSE IF ( ref_name .EQ. 'ECHARC_REFFTR' ) THEN
         mapped_address = iaddr_echarc_dataz

      ELSE IF ( ref_name .EQ. 'ECHARC_REFWAV' ) THEN
         mapped_address = iaddr_echarc_datax

      ELSE IF ( REF_NAME .EQ. 'EFTRDB_IND_SIZ' ) THEN
         mapped_address = iaddr_windex_size

      ELSE IF ( ref_name .EQ. 'EFTRDB_DATABASE' ) THEN
         mapped_address = iaddr_fdb_database

      ELSE IF ( ref_name .EQ. 'EFTRDB_INTENSITY' ) THEN
         mapped_address = iaddr_fdb_intensity

      ELSE IF ( ref_name .EQ. 'EFTRDB_LABELX' ) THEN
         string = cstr_eftrdb_labelx

      ELSE IF ( ref_name .EQ. 'EFTRDB_LABELZ' ) THEN
         string = cstr_eftrdb_labelz

      ELSE IF ( ref_name .EQ. 'EFTRDB_LEFT' ) THEN
         mapped_address = iaddr_fdb_left

      ELSE IF ( REF_NAME .EQ. 'EFTRDB_QIND_SIZ' ) THEN
         mapped_address = iaddr_qindex_size

      ELSE IF ( ref_name .EQ. 'EFTRDB_QUICK_INDEX' ) THEN
         mapped_address = iaddr_fdb_quick_index

      ELSE IF ( ref_name .EQ. 'EFTRDB_QUICK_VALUE' ) THEN
         mapped_address = iaddr_fdb_quick_value

      ELSE IF ( ref_name .EQ. 'EFTRDB_RIGHT' ) THEN
         mapped_address = iaddr_fdb_right

      ELSE IF ( ref_name .EQ. 'EFTRDB_SOURCE' ) THEN
         string = cstr_eftrdb_source

      ELSE IF ( ref_name .EQ. 'EFTRDB_WAVE_INDEX' ) THEN
         mapped_address = iaddr_fdb_wave_index

      ELSE IF ( ref_name .EQ. 'EFTRDB_WAVELENGTH' ) THEN
         mapped_address = iaddr_fdb_wavelength

      ELSE IF ( ref_name .EQ. 'EFTRDB_WSAMPLES' ) THEN
         mapped_address = iaddr_fdb_wsamples

      ELSE IF ( ref_name .EQ. 'EFTRDB_WSTATUS' ) THEN
         mapped_address = iaddr_fdb_wstatus

      ELSE IF ( ref_name .EQ. 'ERR_SPECTRUM' ) THEN
         mapped_address = iaddr_err_spectrum

      ELSE IF ( ref_name .EQ. 'ERR_STDSPECTRUM' ) THEN
         mapped_address = iaddr_err_fspectrum

      ELSE IF ( ref_name .EQ. 'EXTR_ARC_VAR' ) THEN
         mapped_address = iaddr_extr_arc_var

      ELSE IF ( ref_name .EQ. 'EXTR_OBJ_VAR' ) THEN
         mapped_address = iaddr_extr_obj_var

      ELSE IF ( ref_name .EQ. 'EXTR_STAR_VAR' ) THEN
         mapped_address = iaddr_extr_star_var

      ELSE IF ( ref_name .EQ. 'EXTRACT_MODE' ) THEN
         string = usr_extract_mode

      ELSE IF ( ref_name .EQ. 'EXTRACTED_ARC' ) THEN
         mapped_address = iaddr_extracted_arc

      ELSE IF ( ref_name .EQ. 'EXTRACTED_OBJ' ) THEN
         mapped_address = iaddr_extracted_obj

      ELSE IF ( ref_name .EQ. 'EXTRACTED_STAR' ) THEN
         mapped_address = iaddr_extracted_star
      END IF
      GO TO 999

*  'F'
    6 IF ( ref_name .EQ. 'F2D_DEVIATION' ) THEN
         mapped_address = iaddr_f2d_deviation

      ELSE IF ( ref_name .EQ. 'F2D_DX_COORD' ) THEN
         mapped_address = iaddr_f2d_dx_coord

      ELSE IF ( ref_name .EQ. 'F2D_DY_COORD' ) THEN
         mapped_address = iaddr_f2d_dy_coord

      ELSE IF ( ref_name .EQ. 'F2D_F_OF_XY' ) THEN
         mapped_address = iaddr_f2d_f_of_xy

      ELSE IF ( ref_name .EQ. 'F2D_FITTED_F' ) THEN
         mapped_address = iaddr_f2d_fitted_f

      ELSE IF ( ref_name .EQ. 'F2D_LINECENT' ) THEN
         mapped_address = iaddr_f2d_linecent

      ELSE IF ( ref_name .EQ. 'F2D_WEIGHTS' ) THEN
         mapped_address = iaddr_f2d_weights

      ELSE IF ( ref_name .EQ. 'F2D_WORK_NAG' ) THEN
         mapped_address = iaddr_f2d_work_nag

      ELSE IF ( ref_name .EQ. 'F2D_XV' ) THEN
         mapped_address = iaddr_f2d_xv

      ELSE IF ( ref_name .EQ. 'FFIELD' )  THEN
         mapped_address = iaddr_ffield
         mapped_qaddress = iaddr_ffieldq
         mapped_eaddress = iaddr_ffielde
         string = img_ffield

      ELSE IF ( ref_name .EQ. 'FIT_WAVES' ) THEN
         mapped_address = iaddr_fit_waves

      ELSE IF ( ref_name .EQ. 'FIT_WAVES2' ) THEN
         mapped_address = iaddr_fit_waves2

      ELSE IF ( ref_name .EQ. 'FIT_WAVES_WORK' ) THEN
         mapped_address = iaddr_fit_waves_work

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XDOUBLE' ) THEN
         mapped_address = iaddr_fit_work_xdouble

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XDOUBLE2' ) THEN
         mapped_address = iaddr_fit_work_xdouble2

      ELSE IF ( ref_name .EQ. 'FIT_WORK_3XDOUBLE' ) THEN
         mapped_address = iaddr_fit_work_3xdouble

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XINT' ) THEN
         mapped_address = iaddr_fit_work_xint

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XREAL' ) THEN
         mapped_address = iaddr_fit_work_xreal

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XREAL2' ) THEN
         mapped_address = iaddr_fit_work_xreal2

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XREAL3' ) THEN
         mapped_address = iaddr_fit_work_xreal3

      ELSE IF ( ref_name .EQ. 'FIT_WORK_XREAL4' ) THEN
         mapped_address = iaddr_fit_work_xreal4

      ELSE IF ( ref_name .EQ. 'FITTED_FLAT' ) THEN
         mapped_address = iaddr_fitted_flat

      ELSE IF ( ref_name .EQ. 'FITTED_PFL' ) THEN
         mapped_address = iaddr_fitted_pfl

      ELSE IF ( ref_name .EQ. 'FITTED_SKY' ) THEN
         mapped_address = iaddr_fitted_sky

      ELSE IF ( ref_name .EQ. 'FITTED_SSKY' ) THEN
         mapped_address = iaddr_fitted_ssky

      ELSE IF ( ref_name .EQ. 'FITTED_WAVES' ) THEN
         mapped_address = iaddr_fitted_waves

      ELSE IF ( ref_name .EQ. 'FLAT_ERRORS' ) THEN
         mapped_address = iaddr_flat_errors

      ELSE IF ( ref_name .EQ. 'FLTFIT' ) THEN
         string = usr_fltfit

      ELSE IF ( ref_name .EQ. 'FSKY_ERRORS' ) THEN
         mapped_address = iaddr_fsky_errors

      ELSE IF ( ref_name .EQ. 'FSSKY_ERRORS' ) THEN
         mapped_address = iaddr_fssky_errors
      END IF
      GO TO 999

*  'G'
    7 GO TO 999

*  'H'
    8 IF ( ref_name .EQ. 'HARD' ) THEN
         string = usr_hplot_device

      ELSE IF ( ref_name .EQ. 'HARDCOPY' ) THEN
         boolean_value = usr_hardcopy

      ELSE IF ( ref_name .EQ. 'HELIO_COR' ) THEN
         value = usr_helio_cor

      ELSE IF ( ref_name .EQ. 'HI_WAVE' ) THEN
         value = usr_hi_wave
      END IF
      GO TO 999

*  'I'
    9 IF ( ref_name .EQ. 'ID_COUNT' ) THEN
         mapped_address = iaddr_id_count

      ELSE IF ( ref_name .EQ. 'ID_LINES' ) THEN
         mapped_address = iaddr_id_lines

      ELSE IF ( ref_name .EQ. 'ID_STATUS' ) THEN
         mapped_address = iaddr_id_status

      ELSE IF ( ref_name .EQ. 'ID_WAVES' ) THEN
         mapped_address = iaddr_id_waves

      ELSE IF ( ref_name .EQ. 'ID_WIDTHS' ) THEN
         mapped_address = iaddr_id_widths

      ELSE IF ( ref_name .EQ. 'INPTIM' ) THEN
         mapped_address = iaddr_inptim
         mapped_qaddress = iaddr_inptimq
         mapped_eaddress = iaddr_inptime
         string = img_inptim
      END IF
      GO TO 999

*  'J'
   10 GO TO 999

*  'K'
   11 GO TO 999

*  'L'
   12 IF ( ref_name .EQ. 'LOC_BOX_AVG' ) THEN
         boolean_value = usr_loc_box_avg

      ELSE IF ( ref_name .EQ. 'LOC_BOX_SIZ' ) THEN
         value = FLOAT( usr_loc_box_siz )

      ELSE IF ( ref_name .EQ. 'LOW_WAVE' ) THEN
         value = usr_low_wave
      END IF
      GO TO 999

*  'M'
   13 IF ( ref_name .EQ. 'MAX_DISPERSION' ) THEN
         value = usr_max_dispersion

      ELSE IF ( ref_name .EQ. 'MFILTER1' ) THEN
         mapped_address = iaddr_mfilter1

      ELSE IF ( ref_name .EQ. 'MFILTER2' ) THEN
         mapped_address = iaddr_mfilter2

      ELSE IF ( ref_name .EQ. 'MIN_DISPERSION' ) THEN
         value = usr_min_dispersion

      ELSE IF ( ref_name .EQ. 'MODEL_PROFILE' ) THEN
         mapped_address = iaddr_model_profile
      END IF
      GO TO 999

*  'N'
   14 IF ( ref_name .EQ. 'NO_OF_BINS' ) THEN
         mapped_address = iaddr_no_of_bins

      ELSE IF ( ref_name .EQ. 'NO_OF_ORDERS' ) THEN
         mapped_address = iaddr_no_of_orders

      ELSE IF ( ref_name .EQ. 'NREF_FRAME' ) THEN
         mapped_address = iaddr_nref_frame

      ELSE IF ( ref_name .EQ. 'NUM_ORDERS' )  THEN
         value  = FLOAT( usr_num_orders )

      ELSE IF ( ref_name .EQ. 'NX_PIXELS' ) THEN
         mapped_address = iaddr_nx_pixels

      ELSE IF ( ref_name .EQ. 'NX_REBIN' ) THEN
         mapped_address = iaddr_nx_rebin

      ELSE IF ( ref_name .EQ. 'NY_PIXELS' ) THEN
         mapped_address = iaddr_ny_pixels
      END IF
      GO TO 999

*  'O'
   15 IF ( ref_name .EQ. 'OBJ_ABOVE' ) THEN
         mapped_address = iaddr_obj_above

      ELSE IF ( ref_name .EQ. 'OBJ_BELOW' ) THEN
         mapped_address = iaddr_obj_below

      ELSE IF ( ref_name .EQ. 'OBJ_MASK' ) THEN
         mapped_address = iaddr_obj_mask

      ELSE IF ( ref_name .EQ. 'OBJ_SKY_GAP' ) THEN
         value  = FLOAT( usr_obj_sky_gap )

      ELSE IF ( ref_name .EQ. 'OBJECT' ) THEN
         mapped_address = iaddr_object
         mapped_qaddress = iaddr_objectq
         mapped_eaddress = iaddr_objecte
         string = img_object

      ELSE IF ( ref_name .EQ. 'OBJECT_PROFILE' ) THEN
         mapped_address = iaddr_object_profile

      ELSE IF ( ref_name .EQ. 'OBJFIT' ) THEN
         string = usr_objfit

      ELSE IF ( ref_name .EQ. 'OBS_INTEN' ) THEN
         mapped_address = iaddr_obs_inten

      ELSE IF ( ref_name .EQ. 'OBS_LINES' ) THEN
         mapped_address = iaddr_obs_lines

      ELSE IF ( ref_name .EQ. 'OPTEXT_MODE' ) THEN
         string = usr_optext_mode

      ELSE IF ( ref_name .EQ. 'ORDER_IDNUM' ) THEN
         mapped_address = iaddr_order_idnum

      ELSE IF ( ref_name .EQ. 'ORDER_NUMBER' )   THEN
         value = FLOAT( usr_order_number )

      ELSE IF ( ref_name .EQ. 'ORDER_SLOPE' ) THEN
         mapped_address = iaddr_order_slope

      ELSE IF ( ref_name .EQ. 'ORDER_YPOS' )  THEN
         mapped_address = iaddr_order_ypos

      ELSE IF ( ref_name .EQ. 'OUTFLAT' )  THEN
         string = rdctn_file_outflat

      ELSE IF ( ref_name .EQ. 'OUTPUT_IMAGE' ) THEN
         mapped_address = iaddr_output_image
         string = img_output_image
      END IF
      GO TO 999

*  'P'
   16 IF ( ref_name .EQ. 'PERPIXEL_ERRORS' ) THEN
         mapped_address = iaddr_error_image

      ELSE IF ( ref_name .EQ. 'PFL_INTERACT' ) THEN
         boolean_value = usr_pfl_interact

      ELSE IF ( ref_name .EQ. 'PFL_MODE' ) THEN
         string = usr_pfl_mode

      ELSE IF ( ref_name .EQ. 'PHOTON_TO_ADU' ) THEN
         MAPPED_ADDRESS = IADDR_PHOTON_TO_ADU
         IF ( IADDR_PHOTON_TO_ADU .NE. 0 ) THEN
            VALUE = ECH_DEREF_REAL( %VAL( IADDR_PHOTON_TO_ADU ) )
         END IF

      ELSE IF ( ref_name .EQ. 'PLY_ORDERINCS' ) THEN
         mapped_address = iaddr_ply_orderincs
      END IF
      GO TO 999

*  'Q'
   17 IF ( ref_name .EQ. 'QUALITY' ) THEN
         mapped_address = iaddr_data_quality
      END IF
      GO TO 999

*  'R'
   18 IF ( ref_name .EQ. 'RBNOBJ' ) THEN
         mapped_address = iaddr_rebin_object
         mapped_eaddress = iaddr_rebin_objecte
         mapped_qaddress = iaddr_rebin_objectq
         string = img_rbnobj

      ELSE IF ( ref_name .EQ. 'RDCTN_ARC' ) THEN
         string = cstr_rdctn_arc

      ELSE IF ( ref_name .EQ. 'RDCTN_FFIELD' ) THEN
         string = cstr_rdctn_ffield

      ELSE IF ( ref_name .EQ. 'RDCTN_INPTIM' ) THEN
         string = cstr_rdctn_inptim

      ELSE IF ( ref_name .EQ. 'RDCTN_OBJECT' ) THEN
         string = cstr_rdctn_object

      ELSE IF ( ref_name .EQ. 'RDCTN_SLITIM' ) THEN
         string = cstr_rdctn_slitim

      ELSE IF ( ref_name .EQ. 'RDCTN_STAR' ) THEN
         string = cstr_rdctn_star

      ELSE IF ( ref_name .EQ. 'RDCTN_TRACIM' ) THEN
         string = cstr_rdctn_tracim

      ELSE IF ( ref_name .EQ. 'READOUT_NOISE' ) THEN
         MAPPED_ADDRESS = IADDR_READOUT_NOISE
         IF ( IADDR_READOUT_NOISE .NE. 0 ) THEN
            VALUE = ECH_DEREF_REAL( %VAL( IADDR_READOUT_NOISE ) )
         END IF

      ELSE IF ( ref_name .EQ. 'REBIN_ARC' ) THEN
         mapped_address = iaddr_rebin_arc

      ELSE IF ( ref_name .EQ. 'REBIN_EARC' ) THEN
         mapped_address = iaddr_rebin_earc

      ELSE IF ( ref_name .EQ. 'REBIN_EORDER' ) THEN
         mapped_address = iaddr_rebin_eorder

      ELSE IF ( ref_name .EQ. 'REBIN_ORDER' ) THEN
         mapped_address = iaddr_rebin_order

      ELSE IF ( ref_name .EQ. 'REBIN_QUALITY' ) THEN
         mapped_address = iaddr_rebin_quality

      ELSE IF ( ref_name .EQ. 'REBIN_WORK' ) THEN
         mapped_address = iaddr_rebin_work

      ELSE IF ( ref_name .EQ. 'REF_CONTINUUM' ) THEN
         mapped_address = iaddr_ref_continuum

      ELSE IF ( ref_name .EQ. 'REF_LINE_FWHM' ) THEN
         mapped_address = iaddr_ref_line_fwhm

      ELSE IF ( ref_name .EQ. 'REF_LINES' ) THEN
         mapped_address = iaddr_ref_lines

      ELSE IF ( ref_name .EQ. 'REF_SPECTRUM' ) THEN
         mapped_address = iaddr_ref_spectrum

      ELSE IF ( ref_name .EQ. 'RESULT_DATAX' .OR.
     :          ref_name .EQ. 'RESULT_WAVES' .OR.
     :          ref_name .EQ. 'RESULT_SCRWAVE' .OR.
     :          ref_name .EQ. 'RESULT_FITWAVE' .OR.
     :          ref_name .EQ. 'RESULT_FITWAVES' .OR.
     :          ref_name .EQ. 'RESULT_SCRWAVES' .OR.
     :          ref_name .EQ. 'RESULT_WAVESCALE' ) THEN
         mapped_address = iaddr_result_datax

      ELSE IF ( ref_name .EQ. 'RESULT_DATAY' .OR.
     :          ref_name .EQ. 'RESULT_ONUM' .OR.
     :          ref_name .EQ. 'RESULT_ONUMS' ) THEN
         mapped_address = iaddr_result_datay

      ELSE IF ( ref_name .EQ. 'RESULT_DATAZ' .OR.
     :          ref_name .EQ. 'RESULT_ORDER' .OR.
     :          ref_name .EQ. 'RESULT_SCRORDER' .OR.
     :          ref_name .EQ. 'RESULT_SCRORDERS' .OR.
     :          ref_name .EQ. 'RESULT_SPECTRUM' .OR.
     :          ref_name .EQ. 'RESULT_ORDERS' ) THEN
         mapped_address = iaddr_result_dataz

      ELSE IF ( ref_name .EQ. 'RESULT_ERRORS' .OR.
     :          ref_name .EQ. 'RESULT_ERROR' .OR.
     :          ref_name .EQ. 'RESULT_SCRERR' .OR.
     :          ref_name .EQ. 'RESULT_SCRERROR' .OR.
     :          ref_name .EQ. 'RESULT_SPECTERR' ) THEN
         mapped_address = iaddr_result_errors

      ELSE IF ( ref_name .EQ. 'RESULT_FORMAT' ) THEN
         string = usr_result_format

      ELSE IF ( ref_name .EQ. 'RESULT_LABELX' ) THEN
         string = cstr_result_labelx

      ELSE IF ( ref_name .EQ. 'RESULT_LABELY' ) THEN
         string = cstr_result_labely

      ELSE IF ( ref_name .EQ. 'RESULT_LABELZ' ) THEN
         string = cstr_result_labelz

      ELSE IF ( ref_name .EQ. 'RESULT_TYPE' ) THEN
         string = usr_result_type
      END IF
      GO TO 999

*  'S'
   19 IF ( ref_name .EQ. 'SCRNCHD_ARC' ) THEN
         mapped_address = iaddr_scrnchd_arc

      ELSE IF ( ref_name .EQ. 'SCRNCHD_ARCV' ) THEN
         mapped_address = iaddr_scrnchd_arcv

      ELSE IF ( ref_name .EQ. 'SCRNCHD_OBJ' ) THEN
         mapped_address = iaddr_scrnchd_obj

      ELSE IF ( ref_name .EQ. 'SCRNCHD_OBJV' ) THEN
         mapped_address = iaddr_scrnchd_objv

      ELSE IF ( ref_name .EQ. 'SCRNCHD_STAR' ) THEN
         mapped_address = iaddr_scrnchd_star

      ELSE IF ( ref_name .EQ. 'SCRNCHD_STRV' ) THEN
         mapped_address = iaddr_scrnchd_strv

      ELSE IF ( ref_name .EQ. 'SCRUNCH_TYPE' ) THEN
         string = usr_scrunch_type

      ELSE IF ( ref_name .EQ. 'SCRNCHD_WAVES' ) THEN
         mapped_address = iaddr_scrnchd_waves

      ELSE IF ( ref_name .EQ. 'SET_WSCALE' ) THEN
         boolean_value = usr_set_wscale

      ELSE IF ( ref_name .EQ. 'SKY_LINES' ) THEN
         mapped_address = iaddr_sky_lines

      ELSE IF ( ref_name .EQ. 'SKY_MASK' ) THEN
         mapped_address = iaddr_sky_mask

      ELSE IF ( ref_name .EQ. 'SKY_SPECTRUM' ) THEN
         mapped_address = iaddr_sky_spectrum

      ELSE IF ( ref_name .EQ. 'SKY_VARIANCE' ) THEN
         mapped_address = iaddr_sky_variance

      ELSE IF ( ref_name .EQ. 'SKYFIT' ) THEN
         string = usr_skyfit

      ELSE IF ( ref_name .EQ. 'SLITIM' ) THEN
         mapped_address = iaddr_slitim
         mapped_qaddress = iaddr_slitimq
         mapped_eaddress = iaddr_slitime
         string = img_slitim

      ELSE IF ( ref_name .EQ. 'SMOOTH_DIAG' ) THEN
         mapped_address = iaddr_smooth_diag

      ELSE IF ( ref_name .EQ. 'SO_FAR' ) THEN
         mapped_address = iaddr_so_far

      ELSE IF ( ref_name .EQ. 'SOFT' ) THEN
         string = usr_splot_device

      ELSE IF ( ref_name .EQ. 'SSKY_SPECTRUM' ) THEN
         mapped_address = iaddr_ssky_spectrum

      ELSE IF ( ref_name .EQ. 'SSKY_VARIANCE' ) THEN
         mapped_address = iaddr_ssky_variance

      ELSE IF ( ref_name .EQ. 'STACK' ) THEN
         string = out_stack

      ELSE IF ( ref_name .EQ. 'STAR' ) THEN
         mapped_address = iaddr_star
         mapped_qaddress = iaddr_starq
         mapped_eaddress = iaddr_stare
         string = img_star

      ELSE IF ( ref_name .EQ. 'STAR_ABOVE' ) THEN
         mapped_address = iaddr_star_above

      ELSE IF ( ref_name .EQ. 'STAR_BELOW' ) THEN
         mapped_address = iaddr_star_below

      ELSE IF ( ref_name .EQ. 'STAR_PROFILE' ) THEN
         mapped_address = iaddr_star_profile

      ELSE IF ( ref_name .EQ. 'START_WAVE' ) THEN
         value = usr_start_wave

      ELSE IF ( ref_name .EQ. 'STD_SPECTRUM' ) THEN
         mapped_address = iaddr_fspectrum
      END IF
      GO TO 999

*  'T'
   20 IF ( ref_name .EQ. 'TRACE' ) THEN
         mapped_address = iaddr_trace

      ELSE IF ( ref_name .EQ. 'TRACIM' ) THEN
         mapped_address = iaddr_tracim
         mapped_qaddress = iaddr_tracimq
         mapped_eaddress = iaddr_tracime
         string = img_tracim

      ELSE IF ( ref_name .EQ. 'TRC_CLIPPED' ) THEN
         mapped_address = iaddr_trc_clipped

      ELSE IF ( ref_name .EQ. 'TRC_IN_DEV' ) THEN
         mapped_address = iaddr_trc_in_dev

      ELSE IF ( ref_name .EQ. 'TRC_INTERACT' ) THEN
         boolean_value = usr_trc_interact

      ELSE IF ( ref_name .EQ. 'TRC_NPOLY' )  THEN
         value = FLOAT( usr_trc_npoly )

      ELSE IF ( ref_name .EQ. 'TRC_OUT_DEV' ) THEN
         mapped_address = iaddr_trc_out_dev

      ELSE IF ( ref_name .EQ. 'TRC_POLY' )  THEN
         mapped_address = iaddr_trc_poly

      ELSE IF ( ref_name .EQ. 'TUNE_SKYINTER' ) THEN
         boolean_value = usr_tune_skyinter

      ELSE IF ( ref_name .EQ. 'TUNE_FCHECK' ) THEN
         boolean_value = usr_tune_fcheck

      ELSE IF ( ref_name .EQ. 'TRACE_WIDTH' ) THEN
         mapped_address = iaddr_trace_width

      ELSE IF ( ref_name .EQ. 'TUNE_XBOX' ) THEN
         value = FLOAT( usr_tune_xbox )

      ELSE IF ( ref_name .EQ. 'TUNE_MAX2DPNTS' ) THEN
         value = FLOAT( usr_tune_max2dpnts )

      ELSE IF ( ref_name .EQ. 'TUNE_MAXLINES' ) THEN
         value = FLOAT( usr_tune_maxlines )

      ELSE IF ( ref_name .EQ. 'TUNE_MAX2DPLY' ) THEN
         value = FLOAT( usr_tune_max2dply )

      ELSE IF ( ref_name .EQ. 'TUNE_PFSAMP' ) THEN
         value = FLOAT( usr_tune_pfsamp )

      ELSE IF ( ref_name .EQ. 'TUNE_MAXWPLY' ) THEN
         value = FLOAT( usr_tune_maxwply )

      ELSE IF ( ref_name .EQ. 'TUNE_MAXPOLY' ) THEN
         value = FLOAT( usr_tune_maxpoly )

      ELSE IF ( ref_name .EQ. 'TUNE_TWTHR' ) THEN
         value = usr_tune_twthr

      ELSE IF ( ref_name .EQ. 'TUNE_AUTLOC' ) THEN
         boolean_value = usr_tune_autloc

      ELSE IF ( ref_name .EQ. 'TUNE_MXSMP' ) THEN
         value = FLOAT( usr_tune_mxsmp )

      ELSE IF ( ref_name .EQ. 'TUNE_MXBADSMP' ) THEN
         value = FLOAT( usr_tune_mxbadsmp )

      ELSE IF ( ref_name .EQ. 'TUNE_CLPMXDEV' ) THEN
         value = usr_tune_clpmxdev

      ELSE IF ( ref_name .EQ. 'TUNE_CLPBY' ) THEN
         value = FLOAT( usr_tune_clpby )

      ELSE IF ( ref_name .EQ. 'TWO_D_DLAMBDA' ) THEN
         mapped_address = iaddr_two_d_dlambda

      ELSE IF ( ref_name .EQ. 'TRACE_MODE' ) THEN
         string = usr_trace_mode

      ELSE IF ( ref_name .EQ. 'TRACE_RE_FITS' ) THEN
         mapped_address = iaddr_trace_re_fits

      ELSE IF ( ref_name .EQ. 'TUNE_TRCNS' ) THEN
         value = usr_tune_trcns

      ELSE IF ( ref_name .EQ. 'TUNE_CNSDEV' ) THEN
         value = usr_tune_cnsdev

      ELSE IF ( ref_name .EQ. 'TUNE_SKYPOLY' ) THEN
         value = FLOAT( usr_tune_skypoly )

      ELSE IF ( ref_name .EQ. 'TUNE_SKYREJ' ) THEN
         value = FLOAT( usr_tune_skyrej )

      ELSE IF ( ref_name .EQ. 'TUNE_SKYRTHR' ) THEN
         value = usr_tune_skyrthr

      ELSE IF ( ref_name .EQ. 'TUNE_OBJREJ' ) THEN
         value = FLOAT( usr_tune_objrej )

      ELSE IF ( ref_name .EQ. 'TUNE_OBJRTHR' ) THEN
         value = usr_tune_objrthr

      ELSE IF ( ref_name .EQ. 'TUNE_MXSKYPIX' ) THEN
         value = FLOAT( usr_tune_mxskypix )

      ELSE IF ( ref_name .EQ. 'TUNE_OBJPOLY' ) THEN
         value = FLOAT( usr_tune_objpoly )

      ELSE IF ( ref_name .EQ. 'TUNE_DEKTHR' ) THEN
         value = usr_tune_dekthr

      ELSE IF ( ref_name .EQ. 'TUNE_PFLSSAMP' ) THEN
         value = FLOAT( usr_tune_pflssamp )

      ELSE IF ( ref_name .EQ. 'TUNE_USE_NXF' ) THEN
         value = usr_tune_use_nxf

      ELSE IF ( ref_name .EQ. 'TUNE_SKYHILIM' ) THEN
         value = usr_tune_skyhilim

      ELSE IF ( ref_name .EQ. 'TUNE_MAXRFLN' ) THEN
         value = FLOAT( usr_tune_maxrfln )

      ELSE IF ( ref_name .EQ. 'TUNE_RFLNTHR' ) THEN
         value = usr_tune_rflnthr

      ELSE IF ( ref_name .EQ. 'TUNE_SKVRCORR' ) THEN
         boolean_value = usr_tune_skvrcorr

      ELSE IF ( ref_name .EQ. 'TUNE_CRCLEAN' ) THEN
         boolean_value = usr_tune_crclean

      ELSE IF ( ref_name .EQ. 'TUNE_IDMXDIF' ) THEN
         value = usr_tune_idmxdif

      ELSE IF ( ref_name .EQ. 'TUNE_IDMDLT' ) THEN
         value = FLOAT( usr_tune_idmdlt )

      ELSE IF ( ref_name .EQ. 'TUNE_IDSDLT' ) THEN
         value = FLOAT( usr_tune_idsdlt )

      ELSE IF ( ref_name .EQ. 'TUNE_IDSTRNG' ) THEN
         value = usr_tune_idstrng

      ELSE IF ( ref_name .EQ. 'TUNE_IDINMX' ) THEN
         value = FLOAT( usr_tune_idinmx )

      ELSE IF ( ref_name .EQ. 'TUNE_IDINMN' ) THEN
         value = FLOAT( usr_tune_idinmn )

      ELSE IF ( ref_name .EQ. 'TUNE_FINCPLY' ) THEN
         value = FLOAT( usr_tune_fincply )

      ELSE IF ( ref_name .EQ. 'TUNE_DSGMTHR' ) THEN
         value = usr_tune_dsgmthr

      ELSE IF ( ref_name .EQ. 'TUNE_DPRBTHR' ) THEN
         value = usr_tune_dprbthr

      ELSE IF ( ref_name .EQ. 'TUNE_FFNXPLY' ) THEN
         value = FLOAT( usr_tune_ffnxply )

      ELSE IF ( ref_name .EQ. 'TUNE_FFNYPLY' ) THEN
         value = FLOAT( usr_tune_ffnyply )

      ELSE IF ( ref_name .EQ. 'TUNE_FFNXREJ' ) THEN
         value = FLOAT( usr_tune_ffnxrej )

      ELSE IF ( ref_name .EQ. 'TUNE_FFNYREJ' ) THEN
         value = FLOAT( usr_tune_ffnyrej )

      ELSE IF ( ref_name .EQ. 'TUNE_FFTHRESH' ) THEN
         value = usr_tune_ffthresh

      ELSE IF ( ref_name .EQ. 'TUNE_SKEW' ) THEN
         value = usr_tune_skew

      ELSE IF ( ref_name .EQ. 'TUNE_LOG' ) THEN
         boolean_value = usr_tune_log

      ELSE IF ( ref_name .EQ. 'TUNE_FLUX' ) THEN
         boolean_value = usr_tune_flux

      ELSE IF ( ref_name .EQ. 'TUNE_INTR' ) THEN
         boolean_value = usr_tune_intr

      ELSE IF ( ref_name .EQ. 'TUNE_QUAD' ) THEN
         boolean_value = usr_tune_quad

      ELSE IF ( ref_name .EQ. 'TUNE_SCRMODE' ) THEN
         value = FLOAT( usr_tune_scrmode )

      ELSE IF ( ref_name .EQ. 'TUNE_SCRADD' ) THEN
         value = FLOAT( usr_tune_scradd )

      ELSE IF ( ref_name .EQ. 'TUNE_SCFRACT' ) THEN
         usr_tune_scfract = value

      ELSE IF ( ref_name .EQ. 'TUNE_CRXBOX' ) THEN
         value = FLOAT( usr_tune_crxbox )

      ELSE IF ( ref_name .EQ. 'TUNE_CRYBOX' ) THEN
         value = FLOAT( usr_tune_crybox )

      ELSE IF ( ref_name .EQ. 'TUNE_CRMAX' ) THEN
         value = FLOAT( usr_tune_crmax )

      ELSE IF ( ref_name .EQ. 'TUNE_CRINTER' ) THEN
         boolean_value = usr_tune_crinter

      ELSE IF ( ref_name .EQ. 'TUNE_FFINTER' ) THEN
         boolean_value = usr_tune_ffinter

      ELSE IF ( ref_name .EQ. 'TUNE_DIAGNOSE' ) THEN
         boolean_value = usr_tune_diagnose

      ELSE IF ( ref_name .EQ. 'TUNE_REVCHK' ) THEN
         boolean_value = usr_tune_revchk

      ELSE IF ( ref_name .EQ. 'TUNE_PREBAL' ) THEN
         boolean_value = usr_tune_prebal

      ELSE IF ( ref_name .EQ. 'TUNE_DB_SCOPE' ) THEN
         value = FLOAT( usr_tune_db_scope )

      ELSE IF ( ref_name .EQ. 'TUNE_NOFLAT' ) THEN
         boolean_value = usr_tune_noflat

      ELSE IF ( ref_name .EQ. 'TUNE_NOARC' ) THEN
         boolean_value = usr_tune_noarc

      ELSE IF ( ref_name .EQ. 'TUNE_BATCH' ) THEN
         boolean_value = usr_tune_batch

      ELSE IF ( ref_name .EQ. 'TUNE_QUICK' ) THEN
         boolean_value = usr_tune_quick

      ELSE IF ( ref_name .EQ. 'TUNE_CRTRC' ) THEN
         boolean_value = usr_tune_crtrc

      ELSE IF ( ref_name .EQ. 'TUNE_MINCR' ) THEN
         value = usr_tune_mincr

      ELSE IF ( ref_name .EQ. 'TUNE_SATRTN' ) THEN
         value = usr_tune_satrtn

      ELSE IF ( ref_name .EQ. 'TUNE_PAGE' ) THEN
         value = FLOAT( usr_tune_page )

      ELSE IF ( ref_name .EQ. 'TUNE_OBJBLW' ) THEN
         value = FLOAT( usr_tune_objblw )

      ELSE IF ( ref_name .EQ. 'TUNE_OBJABV' ) THEN
         value = FLOAT( usr_tune_objabv )

      ELSE IF ( ref_name .EQ. 'TUNE_DEKBLW' ) THEN
         value = FLOAT( usr_tune_dekblw )

      ELSE IF ( ref_name .EQ. 'TUNE_DEKABV' ) THEN
         value = FLOAT( usr_tune_dekabv )

      ELSE IF ( ref_name .EQ. 'TUNE_REPORT' ) THEN
         string = usr_tune_report

      ELSE IF ( ref_name .EQ. 'TUNE_CLONE' ) THEN
         string = usr_tune_clone

      ELSE IF ( ref_name .EQ. 'TUNE_SKYSIM' ) THEN
         boolean_value = usr_tune_skysim

      ELSE IF ( ref_name .EQ. 'TUNE_FFLMED' ) THEN
         boolean_value = usr_tune_fflmed

      ELSE IF ( ref_name .EQ. 'TUNE_FFSUBSMP' ) THEN
         boolean_value = usr_tune_ffsubsmp

      ELSE IF ( ref_name .EQ. 'TUNE_SKYXPLY' ) THEN
         value = FLOAT( usr_tune_skyxply )

      ELSE IF ( ref_name .EQ. 'TUNE_SKYLINW' ) THEN
         value = FLOAT( usr_tune_skylinw )

      ELSE IF ( ref_name .EQ. 'TUNE_FFLSMP' ) THEN
         value = FLOAT( usr_tune_fflsmp )

      ELSE IF ( ref_name .EQ. 'TUNE_SKYLTHR' ) THEN
         value = usr_tune_skylthr

      ELSE IF ( ref_name .EQ. 'TUNE_ARCHIVE' ) THEN
         boolean_value = usr_tune_archive

      ELSE IF ( ref_name .EQ. 'TUNE_PARTORD' ) THEN
         boolean_value = usr_tune_partord

      ELSE IF ( ref_name .EQ. 'TUNE_USEAAA' ) THEN
         boolean_value = usr_tune_useaaa

      ELSE IF ( ref_name .EQ. 'TUNE_AAACODE' ) THEN
         value = FLOAT( usr_tune_aaacode )

      ELSE IF ( ref_name .EQ. 'TUNE_MRGMINX' ) THEN
         value = FLOAT( usr_tune_mrgminx )

      ELSE IF ( ref_name .EQ. 'TUNE_MRGMAXX' ) THEN
         value = FLOAT( usr_tune_mrgmaxx )

      ELSE IF ( ref_name .EQ. 'TUNE_IUE' ) THEN
         value = FLOAT( usr_tune_iue )

      ELSE IF ( ref_name .EQ. 'TUNE_BLZRSET' ) THEN
         boolean_value = usr_tune_blzrset

      ELSE IF ( ref_name .EQ. 'TUNE_YBLAZE' ) THEN
         boolean_value = usr_tune_yblaze

      ELSE IF ( ref_name .EQ. 'TUNE_MERGE' ) THEN
         boolean_value = usr_tune_merge

      ELSE IF ( ref_name .EQ. 'TUNE_FIBRES' ) THEN
         boolean_value = usr_tune_fibres

      ELSE IF ( ref_name .EQ. 'TUNE_AIRTOVAC' ) THEN
         boolean_value = usr_tune_airtovac

      ELSE IF ( ref_name .EQ. 'TUNE_AUTOMATE' ) THEN
         CONTINUE

      ELSE IF ( ref_name .EQ. 'TRACE_PATH' ) THEN
         mapped_address = iaddr_trace_path

      ELSE IF ( ref_name .EQ. 'TUNE_MRGWGHT' ) THEN
         string = usr_tune_mrgwght

      ELSE IF ( ref_name .EQ. 'TRCFIT' ) THEN
         string = usr_trcfit

      ELSE IF ( ref_name .EQ. 'TUNE_XZONE' ) THEN
         CONTINUE

      ELSE IF ( ref_name .EQ. 'TUNE_YZONE' ) THEN
         CONTINUE
      END IF
      GO TO 999

*  'U'
   21 IF ( REF_NAME .EQ. 'USE_MEDIAN' ) THEN
         BOOLEAN_VALUE = USR_USE_MEDIAN
      END IF
      GO TO 999

*  'V'
   22 IF ( REF_NAME .EQ. 'VAR_MATRIX' ) THEN
         MAPPED_ADDRESS = IADDR_VAR_MATRIX
      END IF
      GO TO 999

*  'W'
   23 IF ( REF_NAME .EQ. 'W_NPOLY' )  THEN
         VALUE = FLOAT( USR_W_NPOLY )

      ELSE IF ( ref_name .EQ. 'W_POLY' ) THEN
         mapped_address = iaddr_w_poly

      ELSE IF ( ref_name .EQ. 'W_POLY_2D' ) THEN
         mapped_address = iaddr_w_poly_2d

      ELSE IF ( ref_name .EQ. 'W2_NX_POLY' ) THEN
         value = FLOAT( usr_w2_nx_poly )

      ELSE IF ( ref_name .EQ. 'W2_NY_POLY' ) THEN
         value = FLOAT( usr_w2_ny_poly )

      ELSE IF ( ref_name .EQ. 'WAVELENGTH' ) THEN
         mapped_address = iaddr_wavelength

      ELSE IF ( ref_name .EQ. 'WAVFIT' ) THEN
         string = usr_wavfit

      ELSE IF ( ref_name .EQ. 'WFDB_DATABASE' ) THEN
         mapped_address = iaddr_wfdb_database

      ELSE IF ( ref_name .EQ. 'WFDB_LEFT' ) THEN
         mapped_address = iaddr_wfdb_left

      ELSE IF ( ref_name .EQ. 'WFDB_RIGHT' ) THEN
         mapped_address = iaddr_wfdb_right

      ELSE IF ( ref_name .EQ. 'WID_POLY' ) THEN
         mapped_address = iaddr_wid_poly

      ELSE IF ( ref_name .EQ. 'WLEFT_OFFSET' ) THEN
         mapped_address = iaddr_wleft_offset

      ELSE IF ( ref_name .EQ. 'WNEXT_INDEX' ) THEN
         mapped_address = iaddr_wnext_index

      ELSE IF ( ref_name .EQ. 'WORDER_NUMS' ) THEN
         mapped_address = iaddr_worder_nums

      ELSE IF ( ref_name .EQ. 'WPFL_TCOUNT' ) THEN
         mapped_address = iaddr_wpfl_tcount

      ELSE IF ( ref_name .EQ. 'WPFL_TOTAL' ) THEN
         mapped_address = iaddr_wpfl_total

      ELSE IF ( ref_name .EQ. 'WPREV_INDEX' ) THEN
         mapped_address = iaddr_wprev_index

      ELSE IF ( ref_name .EQ. 'WRATIOS' ) THEN
         mapped_address = iaddr_wratios

      ELSE IF ( ref_name .EQ. 'WRIGHT_OFFSET' ) THEN
         mapped_address = iaddr_wright_offset

      ELSE IF ( ref_name .EQ. 'WSEAR_END' ) THEN
         mapped_address = iaddr_wsear_end

      ELSE IF ( ref_name .EQ. 'WSEAR_START' ) THEN
         mapped_address = iaddr_wsear_start

      ELSE IF ( ref_name .EQ. 'WVSCALE_INDEX' ) THEN
         mapped_address = iaddr_wvscale_index
      END IF
      GO TO 999

*  'X'
   24 IF ( ref_name .EQ. 'X_TRACE_COORD' ) THEN
         mapped_address = iaddr_x_trace_coord
      END IF
      GO TO 999

*  'Y'
   25 IF ( ref_name .EQ. 'Y_TRACE_COORD' ) THEN
         mapped_address = iaddr_y_trace_coord
      END IF
      GO TO 999

*  Others...
   26 IF ( ref_name .EQ. '1D_SPECTRUM' ) THEN
         mapped_address = iaddr_spectrum

      ELSE IF ( ref_name .EQ. '2D_INTERACT' ) THEN
         boolean_value = usr_2d_interact

      ELSE IF ( ref_name .EQ. '%%PLOT_SPECIAL' ) THEN

*     Special dummy variable for ECH_PLOTTER use in ADAM version
         CONTINUE

      ELSE
         FOUND = .FALSE.
         DO I = 1, FOREIGN_PAR_COUNT
            IF ( REF_NAME .EQ. FOREIGN_PAR( I ) ) THEN
               VALUE = FLOAT( FOREIGN_PAR_VALUE( I ) )
               FOUND = .TRUE.
               GO TO 200
            END IF
         END DO
  200    CONTINUE
         IF ( .NOT. FOUND ) THEN
            CALL ECH_REPORT( 0,
     :           '!   Error: unknown foreign index-parameter.' )
            STATUS = ECH__ABORT_OPTION
         END IF
      END IF

  999 CONTINUE

      END
