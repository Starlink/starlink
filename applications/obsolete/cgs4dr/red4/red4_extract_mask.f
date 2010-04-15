*+  RED4_EXTRACT_MASK - Extract a bad pixel mask from a data set
      SUBROUTINE RED4_EXTRACT_MASK( STATUS )
*    Description :
*     This routine creates a new bad pixel mask using the data quality
*     information in a data set.
*    Invocation :
*     CALL RED4_EXTRACT_MASK( OPERATION, STATUS )
*    Parameters :
*     STATUS    = INTEGER( UPDATE )
*           Global status. This must be ADAM__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be ADAM__OK on exit. Any other value indicates
*           an error.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly (JACH.HAWAII.EDU::PND)
*    History :
*     18-May-1990: Original version.                               (SMB)
*      3-Sep-1990: Modified to make use of FITS structure.         (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed, which would
*                  have made this routine fail under ADAM V1.9.    (SMB)
*     18-Feb-1993: Conform to error strategy                       (PND)
*     17-Jan-1994: Add template files                              (PND)
*      9-Nov-1994: Attempt to make vaguely portable                (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
      INCLUDE 'RED4_COMMON.INC'
*    Status :
      INTEGER
     :  STATUS               ! Global status
*    Global variables :
*    External references :
      INTEGER CHR_LEN        ! Character length determining function
*    Local constants :
*    Local variables :
      CHARACTER*80
     :  DATA,                ! Name of data from which mask is to be extracted
     :  MASK                 ! Name of bad pixel mask to be created
      CHARACTER*32
     :  DATA_INFO(2)         ! Labels and units for the data.
      CHARACTER*20 LPREFIX   ! Prefix to apply to name
      LOGICAL
     :  QUALITY              ! TRUE if a data quality array exists
      INTEGER
     :  CLEN,                ! Non-blank length of character string
     :  CPOS,                ! Position in a string
     :  NDIM,                ! The number of dimensions in the mask
     :  DIMS( MAXDIM ),      ! The dimensions of the bad pixel mask
     :  NELM,                ! The number of elements in the bad pixel mask
     :  ADDRESS,             ! Address of virtual memory
     :  MASK_PTR,            ! Address of mapped data array in MASK
     :  QUALITY_PTR,         ! Address of mapped data quality array in DATA
     :  MASK_SLOT,           ! Slot for mapped data array in MASK
     :  QUALITY_SLOT         ! Slot for mapped data quality array in DATA
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the names of data structure and the bad pixel mask to be created.
      CALL PAR_GET0C( 'DATA', DATA, STATUS )
!      CALL CHR_UCASE( DATA )
      CALL PAR_GET0C( 'MASK', MASK, STATUS )
!      CALL CHR_UCASE( MASK )
      CLEN = CHR_LEN( MASK )
      CPOS = INDEX( MASK, 'CGS4_MASKS:')
      IF (CPOS .EQ. 0) CPOS = INDEX( MASK, 'CGS4_MASKS/')
      IF ( CPOS .EQ. 0 ) THEN
         CALL RED4_GET_PREFIX ('MASK', LPREFIX, STATUS)
         MASK = LPREFIX(:CHR_LEN(LPREFIX)) //  MASK(1:CLEN)
      END IF

*   Check this has worked
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA
         CALL DSA_OPEN( STATUS )

*      Open the data structure for input
         CALL RED4_CHECK_INPUT( DATA, STATUS )
         CALL DSA_NAMED_INPUT( 'DATA', DATA, STATUS )

*      Tell DSA that a data quality array is will be used to
*      hold bad pixel information.
         CALL DSA_USE_QUALITY( 'DATA', STATUS )

*      Obtain the dimensions of the data array in the data structure
         CALL DSA_DATA_SIZE( 'DATA', MAXDIM, NDIM, DIMS, NELM, STATUS )

*      Check that a data quality array exists in the data structure
         CALL DSA_SEEK_QUALITY( 'DATA', QUALITY, STATUS )

*      Check everything is ok so far
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Check that there is a quality array to be transferred to
*         the bad pixel mask.
            IF ( QUALITY ) THEN

*            Open the mask template file CGS4_TEMPLATES:MASK_TEMPLATE.DST
*            and use this as a basis for creating the new mask structure
               CALL DSA_NAMED_INPUT( 'MASK_TEMP',
     :            MASK_TEMPLATE, STATUS )
               CALL DSA_NAMED_OUTPUT( 'MASK', MASK,
     :           'MASK_TEMP', 0, 0, STATUS )

*            Check that the sizes of the data arrays contained in these
*            structures match up. (If any do not, a bad status will
*            be returned).
*            (Note that the actual dimensions of the data arrays are not
*            checked to be the same, so for example a 62x58 mask can be
*            applied to a 58x62 data structure. Perhaps this check should
*            be included).
               CALL DSA_MATCH_SIZES( 'DATA', 'MASK', STATUS )

*            Map the data quality array of the data structure
*            and the data array of the mask.
               CALL DSA_MAP_QUALITY( 'DATA', 'READ', 'BYTE',
     :            ADDRESS, QUALITY_SLOT, STATUS )
               QUALITY_PTR = ADDRESS
               CALL DSA_MAP_DATA( 'MASK', 'WRITE', 'BYTE', ADDRESS,
     :            MASK_SLOT, STATUS )
               MASK_PTR = ADDRESS

*            Check the arrays have been mapped successfully
               IF ( STATUS .EQ. ADAM__OK ) THEN

*               Copy the quality array to the bad pixel mask
                  CALL GEN_MOVE( NELM, %val(QUALITY_PTR),
     :              %val(MASK_PTR) )

*               Write the object name 'Bad pixel mask', into
*               the output data structure.
                  CALL DSA_SET_OBJECT( 'MASK',
     :              'Bad pixel mask', STATUS )

*               Write the data label 'Data Quality' into the
*               output data structure.
                  DATA_INFO(1) = ' '
                  DATA_INFO(2) = 'Data Quality'
                  CALL DSA_SET_DATA_INFO( 'MASK', 2, DATA_INFO,
     :              0, 0.0D0, STATUS )

*               Record how this mask was generated.
                  CALL DSA_PUT_FITS_C( 'MASK', 'COMMENT',
     :              'Extracted from the QUALITY array of existing '/
     :              /'data.', ' ', STATUS )

*               Record the name of the file from which this mask
*               was generated in the output structure.
                  CLEN = MAX( 1, CHR_LEN( DATA ) )
                  CALL DSA_PUT_FITS_C( 'MASK', 'FROMFILE',
     :              DATA(1:CLEN), ' ', STATUS )
               ELSE

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_EXTRACT_MASK: Error '/
     :              /'creating mask and mapping data', STATUS )
               END IF
            ELSE

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_EXTRACT_MASK: Data '/
     :           /'structure does not contain a quality array ',
     :           STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_EXTRACT_MASK: Error opening '/
     :        /'DSA and data structures', STATUS )
         END IF

*      Close DSA. Note that this will unmap any data and workspace
*      mapped above. It will attempt to execute regardless of the
*      status.
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_EXTRACT_MASK: Error obtaining '/
     :     /'input parameters', STATUS )
      END IF

      END
