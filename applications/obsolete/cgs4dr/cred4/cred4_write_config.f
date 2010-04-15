*+  CRED4_WRITE_CONFIG - Write current configuration to file.
      SUBROUTINE CRED4_WRITE_CONFIG( STATUS )
*    Description :
*     This routine writes the current data reduction configuration
*     stored in variables in the common block to a file. The file
*     is assumed to be written to the CGS4_CONFIG directory by
*     default.
*    Invocation :
*     CALL CRED4_WRITE_CONFIG( STATUS )
*    Authors :
*     P N Daly (JACH::PND)
*    History :
*     06-Jan-1993: Original version (replaces previous file)       (PND)
*     14-Jul-1993: Make PoSiX error non-fatal                      (PND)
*     21-Dec-1993: Put in task aliasing                            (PND)
*      3-Jan-1994: Add contouring etc                              (PND)
*     22-Mar-1994: Add automated extract spc                       (PND,KLK)
*     24-May-1994: Add bright and faint source algorithm           (PND)
*     19-Jul-1994: Replace IO_ calls with FIO_ calls               (PND)
*     30-Aug-1994: Port to Unix                                    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS             ! Global status
*    External references :
      INTEGER CHR_LEN            ! Length of string function
*    Global variables :
      INCLUDE 'CRED4COM.INC'     ! CRED4 common block
*    Local variables :
      CHARACTER*100
     :  CFILE,                   ! Name of configuration file
     :  COMMENT                  ! An optional comment
      CHARACTER*32
     :  USER,                    ! Calling username
     :  NODENAME,                ! The system nodename
     :  RELEASE,                 ! Software release
     :  VERSION,                 ! Software version id
     :  MACHINE,                 ! Machine type
     :  CALTIME,                 ! Calendar time
     :  TMPSTR                   ! Temporary string
      INTEGER
     :  CLEN,                    ! Length in character COMMENT
     :  CPOS,                    ! Position in string
     :  LUN,                     ! Fortran logical unit number
     :  NTICKS                   ! Time ticks
      LOGICAL
     :  PRINTER,                 ! Flag indicating if the file is to be printed.
     :  NBS                      ! TRUE for ICL files else KEYWORD files
*    Local data :
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain the name of the configuration file to be created.
      CALL CHR_FILL( ' ', CFILE )
      CALL PAR_GET0C( 'CONFIG_FILE', CFILE, STATUS )
      CALL PAR_GET0L( 'PRINTER', PRINTER, STATUS )
      CALL PAR_GET0L( 'NBS', NBS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CRED4_WRITE_CONFIG: '/
     :      /'Unable to obtain configuration parameters', STATUS )
         RETURN
      ENDIF

*    Get the full file spec
      CPOS = CHR_LEN( CFILE )
      CALL CHR_FIND( CFILE, SEPARATOR, .FALSE., CPOS )
      CFILE = CGS4_CONFIG(1:CHR_LEN(CGS4_CONFIG)) // CFILE(CPOS+1:CHR_LEN(CFILE))
      IF ( INDEX(CFILE,'.').EQ.0 ) CFILE = CFILE(1:CHR_LEN(CFILE)) // '.cred4'

*   Create a new configuration file
      CALL FIO_OPEN( CFILE(1:CHR_LEN(CFILE)),
     :   'WRITE', 'LIST', 0, LUN, STATUS )

*   Do some PoSiX calls to get some information
      CALL PSX_CUSERID( USER, STATUS )
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, CALTIME, STATUS )
      CALL PSX_UNAME( SYSNAME, NODENAME,
     :   RELEASE, VERSION, MACHINE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_OUT( ' ',
     :     'Unable to obtain PoSiX information '/
     :     /'(error ignored)', STATUS )
         CALL ERR_ANNUL( STATUS )
      ENDIF

*   Write out header information
      COMMENT = '{ +'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      COMMENT = '{ '//CFILE(1:CHR_LEN(CFILE))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      COMMENT = '{ Written by ' // NODENAME(1:CHR_LEN(NODENAME)) /
     :   / '::' // USER(1:CHR_LEN(USER)) // ' on ' /
     :   / CALTIME(1:CHR_LEN(CALTIME))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      COMMENT = '{ Written under ' // SYSNAME(1:CHR_LEN(SYSNAME)) // ' / '
     :   // RELEASE(1:CHR_LEN(RELEASE)) // ' / '
     :   // VERSION(1:CHR_LEN(VERSION)) // ' / '
     :   // MACHINE(1:CHR_LEN(MACHINE)) // ' '
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      COMMENT = '{ -'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write a header for the set_reduction_sequence menu
      COMMENT = '{ MENU: set_reduction_sequence '
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write subtract_bias_frame
      COMMENT = '{ Subtract_bias_frame? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
         COMMENT =
     :     'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.SUBTRACT_BIAS.EXECUTE'') '
      ELSE
         COMMENT = 'SUBTRACT_BIAS = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//SUBTRACT_BIAS(1:CHR_LEN(SUBTRACT_BIAS))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write subtract_dark_frame
      COMMENT = '{ Subtract_dark_frame? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.SUBTRACT_DARK.EXECUTE'') '
      ELSE
         COMMENT = 'SUBTRACT_DARK = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//SUBTRACT_DARK(1:CHR_LEN(SUBTRACT_DARK))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

*  Write coadd_each_integration
      COMMENT = '{ Coadd_each_integration? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.ADD_INT.EXECUTE'') '
      ELSE
         COMMENT = 'ADD_INT = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//ADD_INT(1:CHR_LEN(ADD_INT))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write archive_each_observation
      COMMENT = '{ Archive_each_observation? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.ARCHIVE_OBS.EXECUTE'') '
      ELSE
         COMMENT = 'ARCHIVE_OBS = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//ARCHIVE_OBS(1:CHR_LEN(ARCHIVE_OBS))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write file_each_observation
      COMMENT = '{ File_each_observation? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.FILE_OBS.EXECUTE'') '
      ELSE
         COMMENT = 'FILE_OBS = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//FILE_OBS(1:CHR_LEN(FILE_OBS))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write normalise_each_flat_field
      COMMENT = '{ Normalise_each_flat_field? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.NORMALISE_FF.EXECUTE'') '
      ELSE
         COMMENT = 'NORMALISE_FF = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//NORMALISE_FF(1:CHR_LEN(NORMALISE_FF))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write divide_by_flat_field
      COMMENT = '{ Divide_by_flat_field? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.DIVIDE_BY_FF.EXECUTE'') '
      ELSE
        COMMENT = 'DIVIDE_BY_FF = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DIVIDE_BY_FF(1:CHR_LEN(DIVIDE_BY_FF))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write wavelength_calibrate
      COMMENT = '{ Wavelength_calibrate? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.TO_WAVELENGTH.EXECUTE'') '
      ELSE
        COMMENT = 'TO_WAVELENGTH = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//TO_WAVELENGTH(1:CHR_LEN(TO_WAVELENGTH))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write add_observations_into_groups
      COMMENT = '{ Add_observations_into_groups? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.ADD_OBS.EXECUTE'') '
      ELSE
        COMMENT = 'ADD_OBS = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//ADD_OBS(1:CHR_LEN(ADD_OBS))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write divide_by_a_standard_source
      COMMENT = '{ Divide_by_a_standard_source? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.DIVIDE_BY_STD.EXECUTE'') '
      ELSE
        COMMENT = 'DIVIDE_BY_STD = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :/' "'//DIVIDE_BY_STD(1:CHR_LEN(DIVIDE_BY_STD))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write extract_spectrum
      COMMENT = '{ Extract_spectrum? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.EXTRACT_SPC.EXECUTE'') '
      ELSE
        COMMENT = 'EXTRACT_SPC = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//EXTRACT_SPC(1:CHR_LEN(EXTRACT_SPC))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write automatic_line_fit
      COMMENT = '{ Automatic_line_fit? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.AUTOFIT.EXECUTE'') '
      ELSE
        COMMENT = 'AUTOFIT = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//AUTOFIT(1:CHR_LEN(AUTOFIT))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write a header for the file_selection menu
      COMMENT = '{ MENU: file_selection '
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write bias_search_mode
      COMMENT = '{ Bias_search_mode? '/
     :   /'FORWARDS/BACKWARDS/BOTH/SPECIFIED'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.BIAS_MODE'') '
      ELSE
        COMMENT = 'BIAS_MODE = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//BIAS_MODE(1:CHR_LEN(BIAS_MODE))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write specified_bias
      COMMENT = '{ Specified_bias? ROyymmdd_oooo'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.SPECIFIED_BIAS'') '
      ELSE
        COMMENT = 'SPECIFIED_BIAS = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//SPECIFIED_BIAS(1:CHR_LEN(SPECIFIED_BIAS))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write dark_search_mode
      COMMENT = '{ Dark_search_mode? '/
     :   /'FORWARDS/BACKWARDS/BOTH/SPECIFIED'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.DARK_MODE'') '
      ELSE
        COMMENT = 'DARK_MODE = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DARK_MODE(1:CHR_LEN(DARK_MODE))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write specified_dark
      COMMENT = '{ Specified_dark? ROyymmdd_oooo'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.SPECIFIED_DARK'') '
      ELSE
        COMMENT = 'SPECIFIED_DARK = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//SPECIFIED_DARK(1:CHR_LEN(SPECIFIED_DARK))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write flat_search_mode
      COMMENT = '{ Flat_search_mode? '/
     :   /'FORWARDS/BACKWARDS/BOTH/SPECIFIED'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.FLAT_MODE'') '
      ELSE
        COMMENT = 'FLAT_MODE = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//FLAT_MODE(1:CHR_LEN(FLAT_MODE))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write specified_flat
      COMMENT = '{ Specified_flat? ROyymmdd_oooo'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.SPECIFIED_FLAT'') '
      ELSE
        COMMENT = 'SPECIFIED_FLAT = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//SPECIFIED_FLAT(1:CHR_LEN(SPECIFIED_FLAT))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write calibration_search_mode
      COMMENT = '{ Calibration_search_mode? '/
     :   /'FORWARDS/BACKWARDS/BOTH/SPECIFIED'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.CALIB_MODE'') '
      ELSE
        COMMENT = 'CALIB_MODE = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//CALIB_MODE(1:CHR_LEN(CALIB_MODE))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write specified_calibration
      COMMENT = '{ Specified_calibration? CAyymmdd_oooo'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
         COMMENT =
     :     'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.SPECIFIED_CALIB'') '
      ELSE
        COMMENT = 'SPECIFIED_CALIB = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//SPECIFIED_CALIB(1:CHR_LEN(SPECIFIED_CALIB))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write standard_search_mode
      COMMENT = '{ Standard_search_mode? '/
     :   /'FORWARDS/BACKWARDS/BOTH/SPECIFIED'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.STANDARD_MODE'') '
      ELSE
        COMMENT = 'STANDARD_MODE = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//STANDARD_MODE(1:CHR_LEN(STANDARD_MODE))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write specified_standard
      COMMENT = '{ Specified_standard?  STyymmdd_gggg'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.SPECIFIED_STD'') '
      ELSE
        COMMENT = 'SPECIFIED_STD = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//SPECIFIED_STD(1:CHR_LEN(SPECIFIED_STD))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write a header for the ff_normalisation menu
      COMMENT = '{ MENU: ff_normalisation'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write normalisation method
      COMMENT = '{ Normalisation method? POLYFIT/SMOOTH'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.NORMALISE_FF.METHOD'') '
      ELSE
        COMMENT = 'NORM_METHOD = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//NORM_METHOD(1:CHR_LEN(NORM_METHOD))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write polynomial_order
      COMMENT = '{ Polynomial_order? 1/2/3/4/5/6/7'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.NORMALISE_FF.ORDER'') '
      ELSE
        COMMENT = 'ORDER = '
      END IF
      CALL CHR_ITOC( ORDER, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write smooth_box_size
      COMMENT = '{ Smooth_box_size? '/
     :   /'1/3/5/7/9/11/13/15/17/19/21/23/25/27'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.NORMALISE_FF.BOXSIZE'') '
      ELSE
        COMMENT = 'BOXSIZE = '
      END IF
      CALL CHR_ITOC( BOXSIZE, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write a header for the wavelength_calibration menu
      COMMENT = '{ MENU: wavelength_calibration'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write calibration_method
      COMMENT = '{ Calibration_method? ESTIMATED/CALIBRATED'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.TO_WAVELENGTH.METHOD'') '
      ELSE
        COMMENT = 'LAMBDA_METHOD = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//LAMBDA_METHOD(1:CHR_LEN(LAMBDA_METHOD))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write a header for the normal_sky_subtraction_parameters menu
      COMMENT = '{ MENU: normal_sky_subtraction_parameters'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write add_in_pairs
      COMMENT = '{ Add_in_pairs? TRUE/FALSE'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.ADD_IN_PAIRS'') '
      ELSE
        COMMENT = 'ADD_IN_PAIRS = '
      END IF
      CALL CHR_LTOC( ADD_IN_PAIRS, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write variance_wt
      COMMENT = '{ Variance_wt? TRUE/FALSE'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.VARIANCE_WT'') '
      ELSE
        COMMENT = 'VARIANCE_WT = '
      END IF
      CALL CHR_LTOC( VARIANCE_WT, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write error_propagation
      COMMENT = '{ Error_propagation? FROM_INT/FROM_OBS'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.ERRORS'') '
      ELSE
        COMMENT = 'ERRORS = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//ERRORS(1:CHR_LEN(ERRORS))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write sky_wt
      COMMENT = '{ Sky_wt? Any real value'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.SKY_WT'') '
      ELSE
        COMMENT = 'SKY_WT = '
      END IF
      CALL CHR_RTOC( SKY_WT, TMPSTR, CLEN )
      IF ( INDEX( TMPSTR, '.' ) .EQ. 0 )
     :   TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write a header for the point_source_options menu
      COMMENT = '{ MENU: point_source_options'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write polyfit
      COMMENT = '{ Polyfit? NONE/OBJ-SKY/REDUCED_GRP/OBJECT'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_POLYFIT'') '
      ELSE
        COMMENT = 'PF_POLYFIT = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//PF_POLYFIT(1:CHR_LEN(PF_POLYFIT))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write degree
      COMMENT = '{ Degree? 1/2/3/4/5'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_DEGREE'') '
      ELSE
        COMMENT = 'PF_DEGREE = '
      END IF
      CALL CHR_ITOC( PF_DEGREE, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write nreject
      COMMENT = '{ Nreject? 0/1/2/3/4/5 etc'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_NREJECT'') '
      ELSE
        COMMENT = 'PF_NREJECT = '
      END IF
      CALL CHR_ITOC( PF_NREJECT, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write weight
      COMMENT = '{ Weight? TRUE/FALSE'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_WEIGHT'') '
      ELSE
        COMMENT = 'PF_WEIGHT = '
      END IF
      CALL CHR_LTOC( PF_WEIGHT, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write first_sky_area_ystart
      COMMENT = '{ First_sky_area_ystart? '/
     :   /'Any integer value (-1 means ignore)'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_SAYS1'') '
      ELSE
        COMMENT = 'PF_SAYS1 = '
      END IF
      CALL CHR_ITOC( PF_SAYS1, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write first_sky_area_yend
      COMMENT = '{ First_sky_area_yend? '/
     :   /'Any integer value (-1 means ignore)'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_SAYE1'') '
      ELSE
        COMMENT = 'PF_SAYE1 = '
      END IF
      CALL CHR_ITOC( PF_SAYE1, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write second_sky_area_ystart
      COMMENT = '{ Second_sky_area_ystart? '/
     :   /'Any integer value (-1 means ignore)'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_SAYS2'') '
      ELSE
        COMMENT = 'PF_SAYS2 = '
      END IF
      CALL CHR_ITOC( PF_SAYS2, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write second_sky_area_yend
      COMMENT = '{ Second_sky_area_yend? '/
     :   /'Any integer value (-1 means ignore)'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_SAYE2'') '
      ELSE
        COMMENT = 'PF_SAYE2 = '
      END IF
      CALL CHR_ITOC( PF_SAYE2, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write third_sky_area_ystart
      COMMENT = '{ Third_sky_area_ystart? '/
     :   /'Any integer value (-1 means ignore)'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_SAYS3'') '
      ELSE
        COMMENT = 'PF_SAYS3 = '
      END IF
      CALL CHR_ITOC( PF_SAYS3, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write third_sky_area_yend
      COMMENT = '{ Third_sky_area_yend? '/
     :   /'Any integer value (-1 means ignore)'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_SAYE3'') '
      ELSE
        COMMENT = 'PF_SAYE3 = '
      END IF
      CALL CHR_ITOC( PF_SAYE3, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write fourth_sky_area_ystart
      COMMENT = '{ Fourth_sky_area_ystart? '/
     :   /'Any integer value (-1 means ignore)'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_SAYS4'') '
      ELSE
        COMMENT = 'PF_SAYS4 = '
      END IF
      CALL CHR_ITOC( PF_SAYS4, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write fourth_sky_area_yend
      COMMENT = '{ Fourth_sky_area_yend? '/
     :   /'Any integer value (-1 means ignore)'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.PF_SAYE4'') '
      ELSE
        COMMENT = 'PF_SAYE4 = '
      END IF
      CALL CHR_ITOC( PF_SAYE4, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write a header for the  set_extract_spc menu
      COMMENT = '{ MENU: set_extract_spc'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write 'Start_row1'
      COMMENT = '{ Start_row1? Any real value'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.EXTRACT_SPC.ROW1S'') '
      ELSE
        COMMENT = 'SPC_ROW1S = '
      END IF
      CALL CHR_RTOC( SPC_ROW1S, TMPSTR, CLEN )
      IF ( INDEX( TMPSTR, '.' ) .EQ. 0 )
     :   TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write 'End_row1'
      COMMENT = '{ End_row1? Any real value'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.EXTRACT_SPC.ROW1E'') '
      ELSE
        COMMENT = 'SPC_ROW1E = '
      END IF
      CALL CHR_RTOC( SPC_ROW1E, TMPSTR, CLEN )
      IF ( INDEX( TMPSTR, '.' ) .EQ. 0 )
     :   TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write 'Start_row2'
      COMMENT = '{ Start_row2? Any real value'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.EXTRACT_SPC.ROW2S'') '
      ELSE
        COMMENT = 'SPC_ROW2S = '
      END IF
      CALL CHR_RTOC( SPC_ROW2S, TMPSTR, CLEN )
      IF ( INDEX( TMPSTR, '.' ) .EQ. 0 )
     :   TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write 'End_row2'
      COMMENT = '{ End_row2? Any real value'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.EXTRACT_SPC.ROW2E'') '
      ELSE
        COMMENT = 'SPC_ROW2E = '
      END IF
      CALL CHR_RTOC( SPC_ROW2E, TMPSTR, CLEN )
      IF ( INDEX( TMPSTR, '.' ) .EQ. 0 )
     :   TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write 'Start_row3'
      COMMENT = '{ Start_row3? Any real value'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.EXTRACT_SPC.ROW3S'') '
      ELSE
        COMMENT = 'SPC_ROW3S = '
      END IF
      CALL CHR_RTOC( SPC_ROW3S, TMPSTR, CLEN )
      IF ( INDEX( TMPSTR, '.' ) .EQ. 0 )
     :   TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write 'End_row3'
      COMMENT = '{ End_row3? Any real value'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.EXTRACT_SPC.ROW3E'') '
      ELSE
        COMMENT = 'SPC_ROW3E = '
      END IF
      CALL CHR_RTOC( SPC_ROW3E, TMPSTR, CLEN )
      IF ( INDEX( TMPSTR, '.' ) .EQ. 0 )
     :   TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write 'Invert_spec'
      COMMENT = '{ Invert_spec? TRUE/FALSE'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.EXTRACT_SPC.INVERT'') '
      ELSE
        COMMENT = 'SPC_INVERT = '
      END IF
      CALL CHR_LTOC( SPC_INVERT, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write 'Algorithm'
      COMMENT = '{ Algorithm? BRIGHT/FAINT'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT =
     :    'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.EXTRACT_SPC.ALGORITHM'') '
      ELSE
        COMMENT = 'SPC_ALGORITHM = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//SPC_ALGORITHM(1:CHR_LEN(SPC_ALGORITHM))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write a header for the automatic_line_fit menu
      COMMENT = '{ MENU: automatic_line_fit'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write rows_to_average
      COMMENT = '{ Rows_to_average? '/
     :   /'1/3/5/7/9/11/13/15/17/19/21/23/25'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.AUTOFIT.NROWS'') '
      ELSE
        COMMENT = 'AFIT_NROWS = '
      END IF
      CALL CHR_ITOC( AFIT_NROWS, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write central_row1
      COMMENT = '{ Central_row1? Any integer value'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.AUTOFIT.ROW1'') '
      ELSE
        COMMENT = 'AFIT_ROW1 = '
      END IF
      CALL CHR_ITOC( AFIT_ROW1, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write central_row1
      COMMENT = '{ Central_row2? Any integer value'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.AUTOFIT.ROW2'') '
      ELSE
        COMMENT = 'AFIT_ROW2 = '
      END IF
      CALL CHR_ITOC( AFIT_ROW2, TMPSTR, CLEN )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write xstart
      COMMENT = '{ Xstart? Any real value'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.AUTOFIT.XSTART'') '
      ELSE
        COMMENT = 'AFIT_XSTART = '
      END IF
      CALL CHR_RTOC( AFIT_XSTART, TMPSTR, CLEN )
      IF ( INDEX( TMPSTR, '.' ) .EQ. 0 )
     :   TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write xend
      COMMENT = '{ Xend? Any real value'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.REDUCTION.AUTOFIT.XEND'') '
      ELSE
        COMMENT = 'AFIT_XEND = '
      END IF
      CALL CHR_RTOC( AFIT_XEND, TMPSTR, CLEN )
      IF ( INDEX( TMPSTR, '.' ) .EQ. 0 )
     :   TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))//' '//TMPSTR(1:CHR_LEN(TMPSTR))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write a header for the define_linearisation_file menu
      COMMENT = '{ MENU: define_linearisation_file'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write name_of_lin_coeffs_file
      COMMENT = '{ Name_of_lin_coeffs_file? '/
     :   /'Any character value (# for none)'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.LINCOEFFS'') '
      ELSE
        COMMENT = 'LINCOEFFS = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//LINCOEFFS(1:CHR_LEN(LINCOEFFS))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write a header for the define_bad_pixel_mask menu
      COMMENT = '{ MENU: define_bad_pixel_mask'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write name_of_existing_mask
      COMMENT = '{ Name_of_existing_mask? '/
     :   /'Any character value (# for none)'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.MISCELLANEOUS.MASK'') '
      ELSE
        COMMENT = 'MASK = '
      END IF
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//MASK(1:CHR_LEN(MASK))//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_integration in P0
      COMMENT = '{ MENU: Display options'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
      COMMENT = '{ Display_reduced_integration (P0)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.INT_P0'') '
      ELSE
        COMMENT = 'INT_P0 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_INT(0) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_INT(0)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_integration in P1
      COMMENT = '{ Display_reduced_integration (P1)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.INT_P1'') '
      ELSE
        COMMENT = 'INT_P1 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_INT(1) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_INT(1)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_integration in P2
      COMMENT = '{ Display_reduced_integration (P2)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.INT_P2'') '
      ELSE
        COMMENT = 'INT_P2 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_INT(2) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_INT(2)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_integration in P3
      COMMENT = '{ Display_reduced_integration (P3)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.INT_P3'') '
      ELSE
        COMMENT = 'INT_P3 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_INT(3) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_INT(3)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_integration in P4
      COMMENT = '{ Display_reduced_integration (P4)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.INT_P4'') '
      ELSE
        COMMENT = 'INT_P4 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_INT(4) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_INT(4)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_integration in P5
      COMMENT = '{ Display_reduced_integration (P5)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.INT_P5'') '
      ELSE
        COMMENT = 'INT_P5 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_INT(5) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_INT(5)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_integration in P6
      COMMENT = '{ Display_reduced_integration (P6)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.INT_P6'') '
      ELSE
        COMMENT = 'INT_P6 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_INT(6) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_INT(6)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_integration in P7
      COMMENT = '{ Display_reduced_integration (P7)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.INT_P7'') '
      ELSE
        COMMENT = 'INT_P7 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_INT(7) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_INT(7)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_integration in P8
      COMMENT = '{ Display_reduced_integration (P8)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.INT_P8'') '
      ELSE
        COMMENT = 'INT_P8 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_INT(8) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_INT(8)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_observation in P0
      COMMENT = '{ Display_reduced_observation (P0)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.OBS_P0'') '
      ELSE
        COMMENT = 'OBS_P0 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_OBS(0) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_OBS(0)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_observation in P1
      COMMENT = '{ Display_reduced_observation (P1)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.OBS_P1'') '
      ELSE
        COMMENT = 'OBS_P1 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_OBS(1) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_OBS(1)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_observation in P2
      COMMENT = '{ Display_reduced_observation (P2)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.OBS_P2'') '
      ELSE
        COMMENT = 'OBS_P2 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_OBS(2) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_OBS(2)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_observation in P3
      COMMENT = '{ Display_reduced_observation (P3)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.OBS_P3'') '
      ELSE
        COMMENT = 'OBS_P3 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_OBS(3) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_OBS(3)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_observation in P4
      COMMENT = '{ Display_reduced_observation (P4)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.OBS_P4'') '
      ELSE
        COMMENT = 'OBS_P4 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_OBS(4) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_OBS(4)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_observation in P5
      COMMENT = '{ Display_reduced_observation (P5)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.OBS_P5'') '
      ELSE
        COMMENT = 'OBS_P5 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_OBS(5) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_OBS(5)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_observation in P6
      COMMENT = '{ Display_reduced_observation (P6)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.OBS_P6'') '
      ELSE
        COMMENT = 'OBS_P6 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_OBS(6) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_OBS(6)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_observation in P7
      COMMENT = '{ Display_reduced_observation (P7)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.OBS_P7'') '
      ELSE
        COMMENT = 'OBS_P7 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_OBS(7) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_OBS(7)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_observation in P8
      COMMENT = '{ Display_reduced_observation (P8)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.OBS_P8'') '
      ELSE
        COMMENT = 'OBS_P8 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_OBS(8) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_OBS(8)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_group in P0
      COMMENT = '{ Display_reduced_group (P0)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.GRP_P0'') '
      ELSE
        COMMENT = 'GRP_P0 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_GRP(0) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_GRP(0)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_group in P1
      COMMENT = '{ Display_reduced_group (P1)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.GRP_P1'') '
      ELSE
        COMMENT = 'GRP_P1 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_GRP(1) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_GRP(1)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_group in P2
      COMMENT = '{ Display_reduced_group (P2)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.GRP_P2'') '
      ELSE
        COMMENT = 'GRP_P2 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_GRP(2) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_GRP(2)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_group in P3
      COMMENT = '{ Display_reduced_group (P3)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.GRP_P3'') '
      ELSE
        COMMENT = 'GRP_P3 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_GRP(3) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_GRP(3)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_group in P4
      COMMENT = '{ Display_reduced_group (P4)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.GRP_P4'') '
      ELSE
        COMMENT = 'GRP_P4 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_GRP(4) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_GRP(4)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_group in P5
      COMMENT = '{ Display_reduced_group (P5)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.GRP_P5'') '
      ELSE
        COMMENT = 'GRP_P5 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_GRP(5) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_GRP(5)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_group in P6
      COMMENT = '{ Display_reduced_group (P6)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.GRP_P6'') '
      ELSE
        COMMENT = 'GRP_P6 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_GRP(6) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_GRP(6)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_group in P7
      COMMENT = '{ Display_reduced_group (P7)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.GRP_P7'') '
      ELSE
        COMMENT = 'GRP_P7 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_GRP(7) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_GRP(7)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_group in P8
      COMMENT = '{ Display_reduced_group (P8)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.GRP_P8'') '
      ELSE
        COMMENT = 'GRP_P8 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_GRP(8) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_GRP(8)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_spectrum in P0
      COMMENT = '{ Display_reduced_spectrum (P0)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.SPC_P0'') '
      ELSE
        COMMENT = 'SPC_P0 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_SPC(0) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_SPC(0)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_spectrum in P1
      COMMENT = '{ Display_reduced_spectrum (P1)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.SPC_P1'') '
      ELSE
        COMMENT = 'SPC_P1 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_SPC(1) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_SPC(1)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_spectrum in P2
      COMMENT = '{ Display_reduced_spectrum (P2)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.SPC_P2'') '
      ELSE
        COMMENT = 'SPC_P2 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_SPC(2) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_SPC(2)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_spectrum in P3
      COMMENT = '{ Display_reduced_spectrum (P3)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.SPC_P3'') '
      ELSE
        COMMENT = 'SPC_P3 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_SPC(3) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_SPC(3)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_spectrum in P4
      COMMENT = '{ Display_reduced_spectrum (P4)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.SPC_P4'') '
      ELSE
        COMMENT = 'SPC_P4 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_SPC(4) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_SPC(4)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_spectrum in P5
      COMMENT = '{ Display_reduced_spectrum (P5)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.SPC_P5'') '
      ELSE
        COMMENT = 'SPC_P5 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_SPC(5) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_SPC(5)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_spectrum in P6
      COMMENT = '{ Display_reduced_spectrum (P6)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.SPC_P6'') '
      ELSE
        COMMENT = 'SPC_P6 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_SPC(6) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_SPC(6)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_spectrum in P7
      COMMENT = '{ Display_reduced_spectrum (P7)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.SPC_P7'') '
      ELSE
        COMMENT = 'SPC_P7 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_SPC(7) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_SPC(7)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Write display_reduced_spectrum in P8
      COMMENT = '{ Display_reduced_spectrum (P8)? YES/NO/ASK'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

      IF ( NBS ) THEN
        COMMENT = 'PUTNBS ((CRED4_NB_ALIAS)&''.DISPLAY.SPC_P8'') '
      ELSE
        COMMENT = 'SPC_P8 = '
      END IF
      CLEN = CHR_LEN( DISPLAY_SPC(8) )
      COMMENT = COMMENT(1:CHR_LEN(COMMENT))/
     :   /' "'//DISPLAY_SPC(8)(1:CLEN)//'"'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Close file
      CALL FIO_CLOSE( LUN, STATUS )

      END
