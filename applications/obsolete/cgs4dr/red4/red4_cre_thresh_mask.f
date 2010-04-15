*+  RED4_CRE_THRESH_MASK - Create a bad pixel mask by thresholding
      SUBROUTINE RED4_CRE_THRESH_MASK( STATUS )
*    Description :
*     This routine creates a bad pixel mask by examining a data
*     array and flagging all the locations where the data lies
*     outside a specified threshold range as bad.
*    Invocation :
*     CALL RED4_CRE_THRESH_MASK( STATUS )
*    Parameters :
*     STATUS      = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     15-Feb-1990: Original version.                          (SMB)
*     16-Feb-1990: Modified to use DSA_GET_RANGE. Also to
*                  use 'Bad pixel mask' as object name.       (SMB)
*     16-Feb-1990: DSA_GET_RANGE called GEN_FRANGEF and
*                  GEN_QRANGEF routines, which do not exist,
*                  resulting in a crash with an access
*                  violation!! Old code reinstated.           (SMB)
*     16-Feb-1990: Made to create a new data structure, using
*                  mask template file as a basis, rather than
*                  the input structure. (The latter lead to
*                  inconsistencies in the mask structure and
*                  the storage of unwanted junk).             (SMB)
*     16-Feb-1990: The status value returned by DSA routines
*                  is not a valid ADAM status. Status
*                  reporting commented out.                   (SMB)
*     16-Feb-1990: DTA_WRVARC refused to work. Commented out
*                  until I can find some DTA documentation or
*                  some help from a DTA expert.               (SMB)
*     20-Feb-1990: Bug fix. The arguments of RED4_QTHRESH
*                  were inconsistent. Check included to make
*                  sure the input and mask arrays are exactly
*                  the same size.                             (SMB)
*     28-Feb-1990: DSA_GET_RANGE restored as an experiment.   (SMB)
*      7-Mar-1990: DSA_GET_RANGE worked, but it took quality
*                  into account. GEN_RANGEF used to ensure
*                  that in this case quality is ignored.
*                  DTA_WRVARC restored.                       (SMB)
*     24-Apr-1990: Because of memory corruption problems, the
*                  code needs to be compiled with array bounds
*                  checking switched on. The Figaro dynamic
*                  memory functions (DYN_ELEMENT, DYNAMIC_MEM,
*                  DYN_INCREMENT) would not allow this. Code
*                  modified to use %val() instead.            (SMB)
*      2-May-1990: The routines DSA_GET_DATA_INFO and
*                  DSA_SET_DATA_INFO require a double
*                  precision array for the NUM_ARRAY argument.
*                  This routine was giving an integer (0),
*                  which could cause problems. Fixed.         (SMB)
*      4-May-1990: All masks now written to CGS4_MASKS
*                  directory.                                 (SMB)
*     23-May-1990: Renamed to RED4_CRE_THRESH_MASK, because
*                  another way of creating a bad pixel mask
*                  now exists.                                (SMB)
*     23-May-1990: TLOW and THIGH written to output strcuture.(SMB)
*      3-Sep-1990: Modified to use .FITS structure.           (SMB)
*     25-Sep-1990: The specification for handling bad pixel
*                  masks has now changed. Masks will now NOT
*                  all be the same size as the array, as in
*                  some circumstances only part of the array
*                  may be read out. Code modified so the data
*                  array in the mask is forced to be the
*                  required size.                             (SMB)
*     12-Nov-1990: Modified to write axis info to mask.       (SMB)
*     28-Nov-1990: Silly typing mistake fixed.                (SMB)
*     18-Feb-1993: Conform to error strategy                  (PND)
*     11-Jan-1994: Allow NDFs with RESHAPE_DATA               (PND)
*     17-Jan-1994: Parameterise template file                 (PND)
*      9-Nov-1994: Attempt to make vaguely portable           (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
      INCLUDE 'RED4_COMMON.INC'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN            ! Character length determining function
*    Global variables :
*    Local Constants :
      INTEGER NINFO              ! Number of information items
      PARAMETER ( NINFO = 2 )
*    Local variables :
      INTEGER
     :  NDIM,                    ! Number of dimensions in input data array
     :  DIMS( MAXDIM ),          ! Dimensions of input data array
     :  NELM,                    ! Total number of elements in input data array
     :  DET_NINCR,               ! Number of detector increments
     :  INPUT_SLOT,              ! Reference slot for mapped input data
     :  INPUT_PTR,               ! Address of input data.
     :  MASK_SLOT,               ! Reference slot for mapped mask data
     :  MASK_PTR,                ! Address of mask data.
     :  GOOD,                    ! Data quality value for good data
     :  BAD,                     ! Data quality value for bad data
     :  NGOOD,                   ! Number of good pixels
     :  NBAD,                    ! Number of bad pixels
     :  CPOS,                    ! Position in a string
     :  CLEN                     ! Non-blank length of character string
      REAL
     :  VMIN,                    ! Minimum value found in data
     :  VMAX,                    ! Maximum value found in data
     :  TLOW,                    ! Lowest acceptable data value
     :  THIGH                    ! Highest acceptable data value
      CHARACTER*80
     :  INPUT,                   ! Name of input data structure
     :  MASK                     ! Name of output data structure for mask
      CHARACTER*32
     :  CHAR_ARRAY( NINFO ),     ! Array to hold axes info
     :  DATA_INFO( NINFO )       ! Labels and units for the data.
      CHARACTER*10
     :  DETECTOR                 ! The name of the detector
      CHARACTER*20  LPREFIX      ! Prefix to apply
      CHARACTER*4
     :  COMMENT                  ! Dummy comment
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the input and output structures.
      CALL PAR_GET0C( 'INPUT', INPUT, STATUS )
      CALL CHR_RMBLK( INPUT )
      CALL PAR_GET0C( 'MASK', MASK, STATUS )
      CLEN = CHR_LEN( MASK )
      CPOS = INDEX( MASK, 'CGS4_MASKS:')
      IF (CPOS .EQ. 0) CPOS = INDEX( MASK, 'CGS4_MASKS/')
      IF ( CPOS .EQ. 0 ) THEN
         CALL RED4_GET_PREFIX ('MASK', LPREFIX, STATUS)
         MASK = LPREFIX(:CHR_LEN(LPREFIX)) // MASK(1:CLEN)
      END IF

*   Obtain the values to be used for good and bad data quality.
      CALL PAR_GET0I( 'GOOD', GOOD, STATUS )
      CALL PAR_GET0I( 'BAD', BAD, STATUS )

*   Check these parameters have been obtained succesfully
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA and attempt to open the input data structure
         CALL DSA_OPEN( STATUS )
         CALL RED4_CHECK_INPUT( INPUT, STATUS )
         CALL DSA_NAMED_INPUT( 'INPUT', INPUT, STATUS )

*      Obtain the size of the data array in the input structure
         CALL DSA_DATA_SIZE( 'INPUT', MAXDIM, NDIM, DIMS, NELM, STATUS )

*      Obtain the number of detector increments used for input data structure.
         CALL DSA_GET_FITS_I( 'INPUT', 'DETNINCR', 0, DET_NINCR,
     :     COMMENT, STATUS )

*      Obtain the name of the detector used to generate input data structure.
         CALL DSA_GET_FITS_C( 'INPUT', 'DETECTOR', 0, DETECTOR,
     :     COMMENT, STATUS )

*      Check this has worked.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Ensure the input data structure has not been oversampled.
            IF ( DET_NINCR .EQ. 1 ) THEN

*            Open the mask template file CGS4_TEMPLATES:MASK_TEMPLATE
*            and use this as a basis for creating the new mask structure
               CALL DSA_NAMED_INPUT( 'MASK_TEMP',
     :            MASK_TEMPLATE, STATUS )
               CALL DSA_NAMED_OUTPUT( 'MASK', MASK,
     :           'MASK_TEMP', 0, 1, STATUS )
               CALL DSA_USE_QUALITY( 'MASK', STATUS )

*            Force the data array in the mask structure to be the same size.
               CALL DSA_RESHAPE_DATA( 'MASK', 'MASK', NDIM,
     :           DIMS, STATUS )

*            Map the data arrays in both structures.
               CALL DSA_MAP_DATA( 'INPUT', 'READ', 'FLOAT',
     :           INPUT_PTR, INPUT_SLOT, STATUS )
               CALL DSA_MAP_DATA( 'MASK', 'WRITE', 'BYTE',
     :           MASK_PTR, MASK_SLOT, STATUS )

*            Check this has worked
               IF ( STATUS .EQ. ADAM__OK ) THEN

*               Determine the range of the input array, and use the
*               maximum and minimum as default values for the TLOW
*               and THIGH parameters. (GEN_RANGEF is used, rather
*               than DSA_GET_RANGE to ensure that data quality is
*               ignored.)
                  CALL GEN_RANGEF( %val(INPUT_PTR), 1, NELM,
     :              VMAX, VMIN )
                  CALL PAR_DEF0R( 'TLOW', VMIN, STATUS )
                  CALL PAR_DEF0R( 'THIGH', VMAX, STATUS )

*               Obtain the acceptable range of the data.
                  CALL PAR_GET0R( 'TLOW', TLOW, STATUS )
                  CALL PAR_GET0R( 'THIGH', THIGH, STATUS )

*               Check these parameters have been obtained successfully
                  IF ( STATUS .EQ. ADAM__OK ) THEN

*                  Set the mapped quality array to "bad" wherever the
*                  data falls outside the specified range. (The arrays
*                  are treated as 1-D, since this makes no difference).
                     CALL RED4_QTHRESH( NELM,
     :                 %val(INPUT_PTR),
     :                 TLOW, THIGH, GOOD, BAD,
     :                 %val(MASK_PTR),
     :                 NGOOD, NBAD, STATUS )

*                  Write the object name 'Bad pixel mask', into
*                  the output data structure.
                     CALL DSA_SET_OBJECT( 'MASK',
     :                 'Bad pixel mask', STATUS )

*                  Write the data label 'Data Quality' into the
*                  output data structure.
                     DATA_INFO(1) = ' '
                     DATA_INFO(2) = 'Data Quality'
                     CALL DSA_SET_DATA_INFO( 'MASK', NINFO,
     :                 DATA_INFO, 0, 0.0D0, STATUS )

*                  Set up the label and units for the axes:
*                  X axis
                     CHAR_ARRAY(1) = ' '
                     CHAR_ARRAY(2) = 'Detector columns'
                     CALL DSA_SET_AXIS_INFO( 'MASK', 1, NINFO,
     :                 CHAR_ARRAY, 0, 0.0D0, STATUS )

*                  Y axis
                     CHAR_ARRAY(1) = ' '
                     CHAR_ARRAY(2) = 'Detector rows'
                     CALL DSA_SET_AXIS_INFO( 'MASK', 2, NINFO,
     :                 CHAR_ARRAY, 0, 0.0D0, STATUS )

*                  Record how this mask was created.
                     CALL DSA_PUT_FITS_C( 'MASK', 'COMMENT',
     :                 'Created by thresholding existing data.',
     :                 ' ', STATUS )

*                  Record the name of the file from which this mask
*                  was generated in the FITS structure.
                     CLEN = MAX( 1, CHR_LEN( INPUT ) )
                     CALL DSA_PUT_FITS_C( 'MASK', 'FROMFILE',
     :                 INPUT(1:CLEN), ' ', STATUS )

*                  Record the name of the detector in the FITS structure.
                     CALL DSA_PUT_FITS_C( 'MASK', 'DETECTOR',
     :                 DETECTOR, ' ', STATUS )

*                  Record the threshold limits in the FITS structure.
                     CALL DSA_PUT_FITS_F( 'MASK', 'TLOW', TLOW,
     :                 ' ', STATUS )
                     CALL DSA_PUT_FITS_F( 'MASK', 'THIGH', THIGH,
     :                 ' ', STATUS )

*                  Record the number of good and bad pixels.
                     CALL DSA_PUT_FITS_I( 'MASK', 'NGOOD', NGOOD,
     :                 ' ', STATUS )
                     CALL DSA_PUT_FITS_I( 'MASK', 'NBAD', NBAD,
     :                 ' ', STATUS )

*                  If everything has worked, report the numbers of
*                  good and bad pixels. (Note that the following will
*                  only produce output if STATUS is ok).
                     CALL MSG_SETI( 'NGOOD', NGOOD )
                     CALL MSG_SETI( 'NBAD', NBAD )
                     CALL MSG_OUT( ' ', 'Bad pixel mask '/
     :                 /'created. ^NGOOD good pixels, ^NBAD '/
     :                 /'bad pixels.', STATUS )
                  ELSE

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_CRE_THRESH_MASK: '/
     :                 /'Failed to get %TLOW '/
     :                 /'and %THIGH parameters', STATUS )
                  END IF
               ELSE

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_CRE_THRESH_MASK: '/
     :              /'Failed to create mask or '/
     :              /'map data arrays', STATUS )
               END IF
            ELSE

               STATUS = SAI__ERROR
               CALL MSG_SETI( 'DETNINCR', DET_NINCR )
               CALL ERR_REP( ' ', 'RED4_CRE_THRESH_MASK: '/
     :           /'Input data are oversampled '/
     :           /'by a factor of ^DETNINCR and not 1', STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_CRE_THRESH_MASK: '/
     :        /'Failed to access data structures', STATUS )
         END IF

*      Close DSA. It will create the new mask structure and will
*      also unmap all the arrays mapped above.
*      (The routine will attempt to work even if the status on entry
*      is bad).
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_CRE_THRESH_MASK: '/
     :     /'Failed to get %INPUT, %MASK, %GOOD '/
     :     /'and %BAD parameters', STATUS )
      END IF

      END
