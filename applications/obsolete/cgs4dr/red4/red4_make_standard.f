*+  RED4_MAKE_STANDARD - Convert a group into a STANDARD
      SUBROUTINE RED4_MAKE_STANDARD( STATUS )
*    Description :
*     This routine converts a given group into a STANDARD frame.
*    Invocation :
*     CALL RED4_MAKE_STANDARD( OBSRED, GRPRED, STATUS )
*    Parameters :
*     STATUS   = INTEGER( UPDATE )
*           Global ADAM status. This must be ADAM__OK on entry, or the
*           routine will not execute. It will be returned ADAM__OK if
*           the routine is successful. Otherwise it will contain an
*           error status.
*    Method :
*    Deficiencies :
*     Better description needed (when there's time).
*     DSA status values no not follow the usual ADAM scheme.
*     None of the GEN routines have any status arguments, so there
*     is no way of knowing if anything has gone wrong.
*
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*    Bugs :
*    Authors :
*     Steven Beard  (REVAD::SMB)
*     Phil Daly     (JACH::PND)
*    History :
*      6-Dec-1990: Original version.                              (SMB)
*      7-Dec-1990: More error traps included to try and prevent
*                  arithmetic errors.                             (SMB)
*     10-Dec-1990: Bug fix. The routine was corrupting memory, and
*                  it was discovered the array size passed to
*                  GEN_FILL was too large.                        (SMB)
*     19-Dec-1990: More typing mistakes fixed.                    (SMB)
*      3-Jan-1991: It was discovered that when a standard spectrum
*                  is extracted from only a few rows, the result
*                  can be unreliable when one or more of the rows
*                  contains a bad pixel. OPER and MEND parameters
*                  added, so that if necessary a bad pixel in any
*                  row can make that pixel bad in the standard,
*                  and if required these bad pixels can be patched
*                  over by interpolation. (This procedure is only
*                  recommended when a small number of rows are used.
*                  The best solution would be either to trail the
*                  standard along the slit or use an optimal
*                  extraction).                                   (SMB)
*     25-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.          (SMB)
*     01-Oct-1991: Convert GEN_*AFE to GEN_AFV*.                  (PND)
*     23-Jul-1992: Allow XSTART XEND to vary  outside range       (PND)
*     31-Aug-1992: Cancel the above change (mis-specification)
*                  but allow the BB to be normalised anywhere
*                  between 0.8 - 10.0 microns.                    (PND)
*     22-Feb-1993: Conform to error strategy                      (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'      ! Contains ADAM__OK.
      INCLUDE 'SAI_ERR'       ! Contains SAI__ERROR.
*    Import :
*    Status :
      INTEGER
     :  STATUS                ! Global ADAM status.
*    External references :
      INTEGER DSA_TYPESIZE    ! DSA type size enquiry function
      INTEGER CHR_LEN         ! Character length determining function
      INTEGER GEN_BSEARCH     ! Figaro binary search function
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'   ! RED4 common block
*    Local Constants :
      REAL WIEN               ! Constant for Wien's displacement law
      PARAMETER ( WIEN = 2897.6 )
      INTEGER NINFO           ! Number of "data info" items which DSA uses
      PARAMETER ( NINFO = 2 )
*    Local variables :
      LOGICAL
     :  WHOLE,                ! TRUE if the whole group is to be extracted.
     :  MEND                  ! TRUE if bad pixels are to be mended
      REAL
     :  TEFF,                 ! Effective temperature of BB
     :  RFLAMBDA,             ! Reference wavelength
     :  XSTART,               ! Lowest X axis wavelength
     :  XEND,                 ! Highest X axis wavelength
     :  YSTART,               ! Lowest Y axis co-ordinate for extraction
     :  YEND                  ! Highest Y axis co-ordinate for extraction
      DOUBLE PRECISION
     :  DIGNORE               ! Ignored argument
      INTEGER
     :  RFELEMENT,            ! Element number at reference wavelength
     :  ISTART,               ! First element of X axis
     :  IEND,                 ! Last element of X axis
     :  JSTART,               ! First row to be extracted
     :  JEND,                 ! Last row to be extracted
     :  FLOATSIZE,            ! Number of bytes in FLOAT data item.
     :  DSA_STATUS,           ! DSA status value
     :  NDIM,                 ! Number of dimensions.
     :  DIMS( MAXDIM ),       ! Dimensions of data arrays.
     :  NELM,                 ! Number of elements in data arrays.
     :  AXIS1_SLOT,           ! Slot number for X axis array
     :  AXIS1_PTR,            ! Pointer to X axis array
     :  GDATA_SLOT,           ! Slot number for data array mapped by DSA
     :  GDATA_PTR,            ! Pointer to data array mapped by DSA
     :  GVAR_SLOT,            ! Slot number for variance array mapped by DSA
     :  GVAR_PTR,             ! Pointer to variance array mapped by DSA
     :  GQUAL_SLOT,           ! Slot number for quality array mapped by DSA
     :  GQUAL_PTR             ! Pointer to quality array mapped by DSA
      INTEGER
     :  SDATA_SLOT,           ! Slot number for data array mapped by DSA
     :  SDATA_PTR,            ! Pointer to data array mapped by DSA
     :  SVAR_SLOT,            ! Slot number for variance array mapped by DSA
     :  SVAR_PTR,             ! Pointer to variance array mapped by DSA
     :  SQUAL_SLOT,           ! Slot number for quality array mapped by DSA
     :  SQUAL_PTR,            ! Pointer to quality array mapped by DSA
     :  D1DATA_SLOT,          ! Slot number for 1-D spectrum data
     :  D1DATA_PTR,           ! Pointer      "   "    "       "
     :  D1VAR_SLOT,           ! Slot number for 1-D spectrum variance
     :  D1VAR_PTR,            ! Pointer      "   "    "        "
     :  D1QUAL_SLOT,          ! Slot number for 1-D spectrum quality
     :  D1QUAL_PTR,           ! Pointer      "   "    "        "
     :  BBDATA_SLOT,          ! Slot number for black-body data
     :  BBDATA_PTR,           ! Pointer for black-body data
     :  BBVAR_SLOT,           ! Slot number for black-body variance
     :  BBVAR_PTR,            ! Pointer for black-body variance
     :  WORK_SLOT,            ! Slot for work array
     :  WORK_PTR,             ! Pointer for work array
     :  CLEN                  ! Non-blank Length of character string.
      CHARACTER*80
     :  GRPFILE,              ! Name of GROUP structure
     :  STDFILE               ! Name of STANDARD structure
      CHARACTER*40
     :  OBJECT                ! Object name
      CHARACTER*32
     :  UNITS,                ! Upper case units.
     :  DATA_INFO( NINFO ),   ! Data info
     :  AXIS_INFO( NINFO )    ! Axis info
      CHARACTER*4
     :  OPER,                 ! Logical operator for bad pixels during extract
     :  COMMENT               ! Dummy comment
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the input group file.
      CALL PAR_GET0C( 'GRPFILE', GRPFILE, STATUS )
      CALL RED4_CHECK_INPUT( GRPFILE, STATUS )

*   Obtain the logical operation to be used when combining bad pixels.
      CALL PAR_GET0C( 'OPER', OPER, STATUS )

*   Determine whether bad pixels are to be mended.
      CALL PAR_GET0L( 'MEND', MEND, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Convert the group file name into the name of a STANDARD.
         CALL RED4_GRPTOSTD( GRPFILE, STDFILE, STATUS )

*      Open DSA and attempt to open the group and standard files.
*      Note that the group file is used as a template for the
*      standard file.
         DSA_STATUS = STATUS
         CALL DSA_OPEN( DSA_STATUS )
         CALL DSA_NAMED_INPUT( 'GROUP', GRPFILE, DSA_STATUS )
         CALL DSA_USE_QUALITY( 'GROUP', DSA_STATUS )
         CALL DSA_NAMED_OUTPUT( 'STANDARD', STDFILE,
     :     'GROUP', 0, 0, DSA_STATUS )
         CALL DSA_USE_QUALITY( 'STANDARD', DSA_STATUS )

*      Determine the size of the data array.
         CALL DSA_DATA_SIZE( 'GROUP', MAXDIM, NDIM, DIMS,
     :     NELM, DSA_STATUS )

*      Obtain the number of bytes in a floating point element.
         FLOATSIZE = DSA_TYPESIZE( 'FLOAT', DSA_STATUS )

*      Obtain the X axis information from the group file.
         CALL DSA_GET_AXIS_INFO( 'GROUP', 1, NINFO, AXIS_INFO,
     :     0, DIGNORE, DSA_STATUS )

*      Give a warning if the X axis units are not microns.
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
             CALL ERR_REP( ' ', 'RED4_MAKE_STANDARD: '/
     :          /'Unable to get X axis info ', STATUS )
         END IF

         UNITS = AXIS_INFO( 1 )
         CALL CHR_UCASE( UNITS )
         IF ( UNITS .NE. 'MICRONS' ) THEN

            CALL MSG_SETC( 'UNITS', AXIS_INFO(1) )
            CALL MSG_OUT( ' ', 'WARNING - X axis units are '/
     :        /'^UNITS and not microns', STATUS )
         END IF

*      Obtain the range of the X axis, specifying that the whole
*      axis is to be used.
         DSA_STATUS = STATUS
         CALL DSA_AXIS_RANGE( 'GROUP', 1, ' ', .TRUE., XSTART, XEND,
     :     ISTART, IEND, DSA_STATUS )

*      Using the central wavelength on the X axis as a default,
*      obtain the reference wavelength.
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
             CALL ERR_REP( ' ', 'RED4_MAKE_STANDARD: '/
     :          /'Unable to get X axis range ', STATUS )
         END IF

         RFLAMBDA = (XSTART + XEND) / 2.0
         CALL PAR_DEF0R( 'RFLAMBDA', RFLAMBDA, STATUS )
         CALL PAR_GET0R( 'RFLAMBDA', RFLAMBDA, STATUS )

*      Ensure this wavelength is within the allowed range.
         IF ( (RFLAMBDA.LT.0.8) .OR. (RFLAMBDA.GT.10.0) ) THEN
            CALL MSG_OUT( ' ', 'RED4_MAKE_STANDARD: '/
     :         /'Reference wavelength is not in range', STATUS )
         END IF

*      Use the Wien displacement law to convert the reference
*      wavelength into a default black-body temperature, being
*      careful to avoid a divide-by-zero.. (This is the temperature
*      at which a black-body would peak at the reference wavelength).
*      Then obtain the required effective temperature.
         IF ( ABS( RFLAMBDA ) .GT. 1.0E-30 ) THEN

            TEFF = WIEN / RFLAMBDA
            CALL PAR_DEF0R( 'TEFF', TEFF, STATUS )
         END IF
         CALL PAR_GET0R( 'TEFF', TEFF, STATUS )

*      Obtain the range of Y axis elements over which the extraction
*      is to take place. The WHOLE parameter is used to specify
*      whether the whole data set is to be extracted. If WHOLE=FALSE,
*      the parameters YSTART and YEND are used to specify the range
*      of rows required.
         CALL PAR_GET0L( 'WHOLE', WHOLE, STATUS )
         DSA_STATUS = STATUS
         CALL DSA_AXIS_RANGE( 'GROUP', 2, ' ', WHOLE, YSTART, YEND,
     :     JSTART, JEND, DSA_STATUS )

*      Check this has worked.
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_MAKE_STANDARD: '/
     :          /'Unable to get Y axis range ', STATUS )
         END IF

         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Map the X axis array from the group structure.
            CALL DSA_MAP_AXIS_DATA( 'GROUP', 1, 'READ', 'FLOAT',
     :        AXIS1_PTR, AXIS1_SLOT, DSA_STATUS )

*         Map the data, variance and quality arrays from both
*         structures.
            CALL DSA_MAP_DATA( 'GROUP', 'READ', 'FLOAT', GDATA_PTR,
     :        GDATA_SLOT, DSA_STATUS )
            CALL DSA_MAP_VARIANCE( 'GROUP', 'READ', 'FLOAT',
     :        GVAR_PTR, GVAR_SLOT, DSA_STATUS )
            CALL DSA_MAP_QUALITY( 'GROUP', 'READ', 'BYTE', GQUAL_PTR,
     :        GQUAL_SLOT, DSA_STATUS )
            CALL DSA_MAP_DATA( 'STANDARD', 'WRITE', 'FLOAT',
     :        SDATA_PTR, SDATA_SLOT, DSA_STATUS )
            CALL DSA_MAP_VARIANCE( 'STANDARD', 'WRITE', 'FLOAT',
     :        SVAR_PTR, SVAR_SLOT, DSA_STATUS )
            CALL DSA_MAP_QUALITY( 'STANDARD', 'WRITE', 'BYTE',
     :        SQUAL_PTR, SQUAL_SLOT, DSA_STATUS )

*         Obtain the workspace to hold the data, variance and quality
*         for the 1-D spectrum and the black-body spectrum.
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT', D1DATA_PTR,
     :        D1DATA_SLOT, DSA_STATUS )
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT', D1VAR_PTR,
     :        D1VAR_SLOT, DSA_STATUS )
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'BYTE', D1QUAL_PTR,
     :        D1QUAL_SLOT, DSA_STATUS )
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT', BBDATA_PTR,
     :        BBDATA_SLOT, DSA_STATUS )
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'FLOAT', BBVAR_PTR,
     :        BBVAR_SLOT, DSA_STATUS )

*         Obtain some workspace for the EXTRACT function.
            CALL DSA_GET_WORK_ARRAY( DIMS(1), 'INT', WORK_PTR,
     :        WORK_SLOT, DSA_STATUS )

*         Check everything has been mapped successfully.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
                CALL ERR_REP( ' ', 'RED4_MAKE_STANDARD: '/
     :             /'Unable to map work array ', STATUS )
            END IF

            IF ( STATUS .EQ. ADAM__OK ) THEN

*            Determine the element number in the X axis array
*            at which the reference wavelength occurs.
               RFELEMENT = GEN_BSEARCH( %val(AXIS1_PTR), DIMS(1),
     :                       RFLAMBDA )

*            Extract a 1-D array from the specified rows of the
*            group data array, combining the variance and quality
*            together. BBDATA_PTR and BBVAR_PTR are used as workspace.
*            (Note that FIGE_XTRACT4 is a private copy of the Figaro
*            extract routine which handles errors and quality).
               IF ( VERBOSE ) THEN

                  CALL MSG_SETR( 'YSTART', YSTART )
                  CALL MSG_SETR( 'YEND', YEND )
                  CALL MSG_OUT( ' ', 'Extracting data from '/
     :              /'rows ^YSTART to ^YEND into 1-D spectrum', STATUS )
               END IF

               CALL FIGE_XTRACT4( %val(GDATA_PTR), %val(GVAR_PTR),
     :           %val(GQUAL_PTR), DIMS(1), DIMS(2), JSTART, JEND,
     :           .TRUE., .TRUE., .FALSE., 0.0, %val(D1DATA_PTR),
     :           %val(D1VAR_PTR), %val(D1QUAL_PTR),
     :           %val(WORK_PTR), %val(BBDATA_PTR),
     :           %val(BBVAR_PTR) )

*            The above extraction routine will have combined the bad
*            pixels using a logical AND function. If an OR is
*            required instead then redo the bad pixel mask.
               IF ( OPER .EQ. 'OR' ) THEN

                  CALL RED4_XTRACTMASK( DIMS(1), DIMS(2),
     :              %val(GQUAL_PTR), JSTART, JEND, 'OR',
     :              %val(D1QUAL_PTR), STATUS )
               END IF

*            If required, mend an bad pixels in the spectrum by
*            interpolation.
               IF ( MEND ) THEN
                  IF ( VERBOSE ) THEN

                     CALL MSG_OUT( ' ', 'Mending bad pixels by '/
     :                 /'interpolation', STATUS )
                  END IF

                  CALL GEN_MEND1F( DIMS(1), %val(D1DATA_PTR),
     :              %val(D1VAR_PTR), %val(D1QUAL_PTR), .TRUE.,
     :              .FALSE., 0.0, .TRUE., %val(D1DATA_PTR),
     :              %val(D1VAR_PTR), %val(D1QUAL_PTR) )
               END IF

*            Generate a Black-Body spectrum with the specified
*            effective temperature, normalised at the given
*            wavelength. (GEN_BBSPC is a private routine).
               IF ( VERBOSE ) THEN

                  CALL MSG_SETR( 'TEFF', TEFF )
                  CALL MSG_SETR( 'RFLAMBDA', RFLAMBDA )
                  CALL MSG_OUT( ' ', 'Generating BB spectrum. '/
     :              /'Temperature ^TEFF K. Normalised at '/
     :              /'^RFLAMBDA microns', STATUS )
               END IF

               CALL GEN_BBSPC( RFLAMBDA, TEFF, %val(AXIS1_PTR),
     :           DIMS(1), %val(BBDATA_PTR) )

*            It is assumed this model black-body spectrum has no
*            error associated with it, so fill its variance array
*            with zeros.
               CALL GEN_FILL( FLOATSIZE*DIMS(1), 0, %val(BBVAR_PTR),
     :           STATUS )

*            Divide the extracted spectrum by this model black-body
*            spectrum, propagating the variance and quality.
               IF ( VERBOSE ) THEN

                  CALL MSG_OUT( ' ', 'Dividing spectrum by '/
     :              /'black body and normalising', STATUS )
               END IF

               CALL GEN_DIVAFV( DIMS(1), %val(D1DATA_PTR),
     :           %val(BBDATA_PTR), %val(D1DATA_PTR),
     :           %val(D1QUAL_PTR), %val(D1QUAL_PTR),
     :           %val(D1QUAL_PTR), %val(D1VAR_PTR),
     :           %val(BBVAR_PTR), %val(D1VAR_PTR), .TRUE.,
     :           .FALSE., 0.0, .TRUE. )

*            Finally, grow the 1-D data, variance and quality arrays
*            along the slit to form the 2-D flux standard.
               IF ( VERBOSE ) THEN

                  CALL MSG_OUT( ' ', 'Growing standard spectrum '/
     :              /'into 2-D along the slit', STATUS )
               END IF

               CALL GEN_GROWX( %val(D1DATA_PTR), DIMS(1), DIMS(2),
     :           1, DIMS(2), %val(SDATA_PTR) )
               CALL GEN_GROWX( %val(D1VAR_PTR), DIMS(1), DIMS(2),
     :           1, DIMS(2), %val(SVAR_PTR) )
               CALL GEN_GROWXB( %val(D1QUAL_PTR), DIMS(1),
     :           DIMS(2), 1, DIMS(2), %val(SQUAL_PTR) )

*            At this point there is no way of telling if all the
*            above has worked, but check the status anyway in case
*            the above routines are given status arguments at some
*            stage.
               IF ( STATUS .EQ. ADAM__OK ) THEN

                  DATA_INFO(1) = 'DN/exp'
                  DATA_INFO(2) = 'STANDARD (un-normalised)'

                  DSA_STATUS = STATUS
                  CALL DSA_SET_DATA_INFO( 'STANDARD', NINFO,
     :              DATA_INFO, 0, 0.0D0, DSA_STATUS )

*               Change the object name to 'Standard from <old name>'
                  CALL DSA_GET_FITS_C( 'STANDARD', 'OBJECT', 0,
     :              OBJECT, COMMENT, DSA_STATUS )

                  OBJECT = 'STANDARD from ' // OBJECT
                  CLEN = MAX( 1, CHR_LEN( OBJECT ) )
                  CALL DSA_PUT_FITS_C( 'STANDARD', 'OBJECT',
     :              OBJECT(1:CLEN), ' ', DSA_STATUS )
                  CALL DSA_SET_OBJECT( 'STANDARD', OBJECT(1:CLEN),
     :              DSA_STATUS )

*               Write a new observation type of 'STANDARD' to the
*               .FITS structure.
                  CALL DSA_PUT_FITS_C( 'STANDARD', 'OBSTYPE',
     :              'STANDARD', ' ', DSA_STATUS )

*               Record the reference wavelength and effective
*               temperature used in the FITS structure.
                  CALL DSA_PUT_FITS_F( 'STANDARD', 'RFLAMBDA',
     :              RFLAMBDA, ' ', DSA_STATUS )
                  CALL DSA_PUT_FITS_F( 'STANDARD', 'TEFF',
     :              TEFF, ' ', DSA_STATUS )

*               Record the name of the group used in the .FITS
*               structure.
                  CLEN = MAX( 1, CHR_LEN( GRPFILE ) )
                  CALL DSA_PUT_FITS_C( 'STANDARD', 'GRPFILE',
     :              GRPFILE(1:CLEN), ' ', DSA_STATUS )

*               If this has all worked, issue a message.
                  IF ( DSA_STATUS .NE. ADAM__OK ) THEN

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_MAKE_STANDARD: '/
     :                  /'Unable to write FITS info ', STATUS )
                  END IF

                  CALL MSG_SETC( 'GRPFILE', GRPFILE )
                  CALL MSG_SETC( 'STDFILE', STDFILE )
                  CALL MSG_OUT( ' ', '^GRPFILE converted to a '/
     :              /'STANDARD, ^STDFILE', STATUS )
               END IF
            ELSE

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_MAKE_STANDARD: '/
     :           /'Error mapping arrays and '/
     :           /'obtaining workspace', STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_MAKE_STANDARD: '/
     :        /'Error accessing data structures '/
     :        /'and obtaining parameters', STATUS )
         END IF

*      Close DSA.
         DSA_STATUS = STATUS
         CALL DSA_CLOSE( DSA_STATUS )

         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_MAKE_STANDARD: '/
     :        /'Unable to proceed with making a standard', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_STANDARD: '/
     :    /'Error obtaining %GRPFILE, %OPER, '/
     :    /'%MEND and %STANDARD_TYPE parameters', STATUS )
      END IF

      END
