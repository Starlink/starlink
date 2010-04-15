*+  RED4_DIVIDE_BY_STD,
      SUBROUTINE RED4_DIVIDE_BY_STD( STATUS )
*    Description :
*     This routine divides a specified GROUP by a suitable STANDARD
*     frame and writes the result to the given output file.
*    Invocation :
*     CALL RED4_DIVIDE_BY_STD( STATUS )
*    Parameters :
*     STATUS    = INTEGER( UPDATE )
*           Global ADAM status.
*    Method :
*    Deficiencies :
*     The output data label is modified by adding "/STANDARD" to be
*     consistent with the rest of the software. Unfortunately, in some
*     circumstances this label may end up being truncated, as only 32
*     characters are available. It might be more sensible simply to
*     replace the label with "Relative flux" and the units with
*     "arbitrary units".
*    Bugs :
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     16-Dec-1990: Original version.                              (SMB)
*     18-Dec-1990: Silly typing mistakes fixed. (More haste less
*                  speed).                                        (SMB)
*     19-Dec-1990: Made to change the data label and units.       (SMB)
*      1-Jan-1991: There were problems driving this action from
*                  CRED4, because it had already added RGDIR to
*                  the names of the group files. Modified to check
*                  for the presence of directory spec before adding
*                  RGDIR.                                         (SMB)
*      3-Jan-1991: Bug in character handling fixed.               (SMB)
*      1-Feb-1991: Modified so data cannot be divided by a
*                  standard more than once.                       (SMB)
*      7-Feb-1991: DSA does not initialise the output variance
*                  array, and this can end up full of junk, which
*                  can crash DSA_CLOSE. Output variance array
*                  initialised to zero to solve this problem.     (SMB)
*     23-Feb-1991: Made to write the name of the STANDARD used
*                  to the FITS header of the output file.         (UKIRT::SMB)
*      1-Oct-1991: Change to GEN_DIVAFV call.                     (PND)
*      8-Oct-1991: Call RED4_GET_OBSERVATION instead of
*                  RED4_SEEK_OBSERVATION, so that calibration
*                  observations can be specified explicitly when
*                  required (DRIMP/5.1 and 5.2).                  (SMB)
*     24-Jun-1992: Allow RO files to be divided by std (but they
*                  are still referred to by DSA as GRPFILE in
*                  this code - an unfortunate misnomer)           (PND)
*     18-Feb-1993: Conform to error strategy                      (PND)
*     30-Jun-1993: Remove output spec                             (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'             ! Contains SAI__ERROR
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN               ! Character length determining function
      INTEGER DSA_TYPESIZE          ! DSA type size inquiry function
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'     ! RED4 common block
*    Local Constants :
      INTEGER NINFO                 ! Number of information items
      PARAMETER ( NINFO = 2 )
*    Local variables :
      INTEGER
     :  FLOATSIZE,                  ! Bytes per element of 'FLOAT' array
     :  CLEN,                       ! Non-blank length of character string
     :  CPOS,                       ! Position in character string
     :  NDIM,                       ! No. of. dimensions in data arrays
     :  DIMS( RMAXDIM ),            ! Dimensions of group data arrays
     :  NELM,                       ! Number of elements in data arrays
     :  GDATA_SLOT,                 ! Input group data array slot
     :  GDATA_PTR,                  ! Input group data array pointer
     :  GVAR_SLOT,                  ! Input group variance array slot
     :  GVAR_PTR                    ! Input group variance array pointer
      INTEGER
     :  GQUAL_SLOT,                 ! Input group quality array slot
     :  GQUAL_PTR,                  ! Input group quality array pointer
     :  ODATA_SLOT,                 ! Output group data array slot
     :  ODATA_PTR,                  ! Output group data array pointer
     :  OVAR_SLOT,                  ! Output group variance array slot
     :  OVAR_PTR,                   ! Output group variance array pointer
     :  OQUAL_SLOT,                 ! Output group quality array slot
     :  OQUAL_PTR,                  ! Output group quality array pointer
     :  NLOW,                       ! Number of low values in GEN_CLIPF
     :  NHIGH                       ! Number of high values in GEN_CLIPF
      DOUBLE PRECISION
     :  DIGNORE                     ! Ignored parameter
      CHARACTER*32
     :  LABEL,                      ! Character buffer for data label.
     :  UNITS,                      ! Character buffer for data units.
     :  DATA_INFO( NINFO )          ! Data label and units.
      CHARACTER*80
     :  GROUP,                      ! The name of the input group file.
     :  OUTPUT,                     ! The name of the output group file.
     :  INDEX_FILE,                 ! The name of the observation index file.
     :  STANDARD_NAME               ! The name of the suitable STANDARD
*    Internal References :
*    Local data :
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain the names of the input and output group files, using
*   the input file as a default for the output.
      CALL PAR_GET0C( 'GROUP', GROUP, STATUS )
      CALL RED4_CHECK_INPUT( GROUP, STATUS )

*   Obtain an output file spec
      CALL PAR_GET0C( 'OUTPUT', OUTPUT, STATUS )

      CALL MSG_SETC( 'GROUP', GROUP )
      CALL MSG_SETC( 'OUTPUT', OUTPUT )
      CALL MSG_OUT( ' ',
     :  'Dividing ^GROUP by a standard to create ^OUTPUT', STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Open DSA and then open the input group, telling DSA that a
*      quality array will be used to store bad pixel information.
*      The group files are assumed to reside in the RGDIR: directory.
         CALL DSA_OPEN( STATUS )
         CALL DSA_NAMED_INPUT( 'GRPFILE', GROUP, STATUS )
         CALL DSA_USE_QUALITY( 'GRPFILE', STATUS )

*      Obtain the number of bytes in an array element of type 'FLOAT'
         FLOATSIZE = DSA_TYPESIZE( 'FLOAT', STATUS )

*      Obtain the data label and units in the input group.
         CALL DSA_GET_DATA_INFO( 'GRPFILE', NINFO, DATA_INFO,
     :     0, DIGNORE, STATUS )

*      Check this has worked.
         IF ( STATUS .EQ. SAI__OK ) THEN

*         Check that the data units do not contain the string 'FLUX',
*         indicating that the input group has already been divided by
*         a STANDARD.
            UNITS = DATA_INFO(1)
            CALL CHR_UCASE( UNITS )

            IF ( INDEX( UNITS, 'FLUX' ) .EQ. 0 ) THEN

*            Open the output group, using the input one as a template and
*            instructing DSA not to force the creation of a new file.
*            If GROUP and OUTPUT happen to be the same, the data will be
*            altered "in situ".
*            Again tell DSA that a quality array will be used.
               CALL DSA_NAMED_OUTPUT( 'OUTFILE', OUTPUT,
     :           'GRPFILE', 0, 0, STATUS )
               CALL DSA_USE_QUALITY( 'OUTFILE', STATUS )

*            Obtain the size of the data array in the input group (which
*            will also be the same size as the one in the output group).
               CALL DSA_DATA_SIZE( 'GRPFILE', RMAXDIM, NDIM, DIMS,
     :           NELM, STATUS )

*            Map the data, variance and quality arrays in the input structure.
               CALL DSA_MAP_DATA( 'GRPFILE', 'READ', 'FLOAT',
     :           GDATA_PTR, GDATA_SLOT, STATUS )
               CALL DSA_MAP_VARIANCE( 'GRPFILE', 'READ', 'FLOAT',
     :           GVAR_PTR, GVAR_SLOT, STATUS )
               CALL DSA_MAP_QUALITY( 'GRPFILE', 'READ', 'BYTE',
     :           GQUAL_PTR, GQUAL_SLOT, STATUS )

*            Map the data, variance and quality arrays in the output structure.
               CALL DSA_MAP_DATA( 'OUTFILE', 'WRITE', 'FLOAT',
     :           ODATA_PTR, ODATA_SLOT, STATUS )
               CALL DSA_MAP_VARIANCE( 'OUTFILE', 'WRITE', 'FLOAT',
     :           OVAR_PTR, OVAR_SLOT, STATUS )

*            Initialise the variance array to zero. DSA occasionally
*            maps an array full of junk, and this can crash DSA_CLOSE
*            if the array contains negative values.
               IF ( STATUS .EQ. SAI__OK ) THEN

                  CALL GEN_FILL( FLOATSIZE*NELM, 0, %val(OVAR_PTR) )
               END IF

               CALL DSA_MAP_QUALITY( 'OUTFILE', 'WRITE', 'BYTE',
     :           OQUAL_PTR, OQUAL_SLOT, STATUS )

*            Check everything has worked so far.
               IF ( STATUS .EQ. SAI__OK ) THEN

*               Obtain the name of the index file from the group name.
                  CALL RED4_GRPTOINDEX( GROUP, INDEX_FILE, STATUS )

*               Obtain a suitable STANDARD, either by searching the
*               index file, or by using one specified explicitly.
                  CALL RED4_GET_OBSERVATION( INDEX_FILE, 'GRPFILE',
     :              'STANDARD', STANDARD_NAME, STATUS )

*               Check that a suitable STANDARD has been found successfully.
                  IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :                 ( STANDARD_NAME .NE. ' ' ) ) THEN

*                  A suitable standard has been found - issue a message.
                     CALL MSG_SETC( 'STANDARD', STANDARD_NAME )
                     CALL MSG_OUT( ' ', 'Using the reduced '/
     :                 /'STANDARD in ^STANDARD', STATUS )

*                  The STANDARD will have already been mapped, and the
*                  pointers stored in the RED4 common block. Using these
*                  pointers, together with the ones obtained above, divide
*                  the input group by the standard and write the result to
*                  the output group, propagating veriance and quality.
                     CALL GEN_DIVAFV( NELM, %val(GDATA_PTR),
     :                 %val(STANDARD_DATA), %val(ODATA_PTR),
     :                 %val(GQUAL_PTR), %val(STANDARD_QUAL),
     :                 %val(OQUAL_PTR), %val(GVAR_PTR),
     :                 %val(STANDARD_VAR),
     :                 %val(OVAR_PTR), .TRUE., .FALSE., 0.0, .TRUE. )

*                  Clip the variance array to ensure there are no negative
*                  values produced by rounding errors. (These will crash
*                  DSA_CLOSE when an attempt is made to square root the
*                  variance array).
                     CALL GEN_CLIPF( %val(OVAR_PTR), NELM, 0.0,
     :                  VAL__MAXR, NLOW, NHIGH, %val(OVAR_PTR) )

*                  Convert the data label and units in the output file
*                  to <Current-label>/STANDARD and "Relative flux".
                     DATA_INFO(1) = 'Relative flux'
                     LABEL = DATA_INFO(2)
                     CLEN = MAX( 1, CHR_LEN( LABEL ) )
                     DATA_INFO(2) = ' '
                     CPOS = 0
                     CALL CHR_PUTC( '(', DATA_INFO(2), CPOS )
                     CALL CHR_PUTC( LABEL(1:CLEN), DATA_INFO(2), CPOS )
                     CALL CHR_PUTC( ')/STANDARD', DATA_INFO(2), CPOS )
                     CALL DSA_SET_DATA_INFO( 'OUTFILE', NINFO,
     :                 DATA_INFO, 0, 0.0D0, STATUS )

*                  Write the name of the STANDARD used to the STDUSED FITS item.
                     CALL DSA_PUT_FITS_C( 'OUTFILE', 'STDUSED',
     :                 STANDARD_NAME, ' ', STATUS )

                  ELSE IF ( STATUS .EQ. SAI__OK ) THEN

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_DIVIDE_BY_STD: '/
     :                  /'No suitable STANDARD found', STATUS )
                  ELSE

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_DIVIDE_BY_STD: '/
     :                 /'Error searching for STANDARD', STATUS )
                  END IF
               ELSE

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_DIVIDE_BY_STD: '/
     :              /'Error opening output and '/
     :              /'mapping data structures', STATUS )
               END IF
            ELSE

               STATUS = SAI__ERROR
               CALL MSG_SETC( 'GROUP', GROUP )
               CALL ERR_REP( ' ', 'RED4_DIVIDE_BY_STD: '/
     :           /'^GROUP has already been divided '/
     :           /'by a STANDARD', STATUS )
               CALL MSG_SETC( 'UNITS', DATA_INFO(1) )
               CALL ERR_REP( ' ', 'RED4_DIVIDE_BY_STD: '/
     :           /'Its units are "^UNITS".', STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_DIVIDE_BY_STD: '/
     :        /'Error opening DSA and input structure', STATUS )
         END IF

*      Close DSA. This routine will attempt to tidy up and unmap any
*      workspace mapped above. It should be called even if an error
*      has occurred.
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_DIVIDE_BY_STD: '/
     :     /'Error obtaining %GROUP and %STANDARD parameters', STATUS )
      END IF

      END
