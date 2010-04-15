*+  RED4_MEND -  Mend a spectrum by interpolating across bad pixels.
      SUBROUTINE RED4_MEND( STATUS )
*    Description :
*     This routine mends a spectrum by interpolating across bad pixel
*     regions. A linear interpolation is used, and suitable variances
*     are calculated.
*    Invocation :
*     CALL RED4_MEND( STATUS )
*    Parameters :
*     STATUS    = INTEGER( UPDATE )
*           Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*      2-Jan-1991: Original version.             (SMB)
*     22-Feb-1993: Conform to error strategy     (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'             ! Contains SAI__ERROR
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'     ! RED4 common block
*    Local Constants :
*    Local variables :
      INTEGER
     :  NDIM,                       ! No. of. dimensions in data arrays
     :  DIMS( RMAXDIM ),            ! Dimensions of data arrays
     :  NELM,                       ! Number of elements in data arrays
     :  IDATA_PTR,                  ! Pointer to input data array
     :  IDATA_SLOT,                 ! Slot for input data array
     :  IVAR_PTR,                   ! Pointer to input variance array
     :  IVAR_SLOT,                  ! Slot for input variance array
     :  IQUAL_PTR,                  ! Pointer to input quality array
     :  IQUAL_SLOT,                 ! Slot for input quality array
     :  ODATA_PTR,                  ! Pointer to output data array
     :  ODATA_SLOT,                 ! Slot for output data array
     :  OVAR_PTR,                   ! Pointer to output variance array
     :  OVAR_SLOT,                  ! Slot for output variance array
     :  OQUAL_PTR,                  ! Pointer to output quality array
     :  OQUAL_SLOT                  ! Slot for output quality array

      CHARACTER*80
     :  INPUT,                      ! The name of the input structure.
     :  OUTPUT                      ! The name of the output structure.
*    Internal References :
*    Local data :
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the names of the input and output structures, using
*   the input structure as a default for the output.
      CALL PAR_GET0C( 'INPUT', INPUT, STATUS )
      CALL PAR_DEF0C( 'OUTPUT', INPUT, STATUS )
      CALL PAR_GET0C( 'OUTPUT', OUTPUT, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA and then open the input structure, telling DSA that a
*      quality array will be used to store bad pixel information.
         CALL DSA_OPEN( STATUS )
         CALL RED4_CHECK_INPUT(  INPUT, STATUS )
         CALL DSA_NAMED_INPUT( 'INFILE', INPUT, STATUS )
         CALL DSA_USE_QUALITY( 'INFILE', STATUS )

*      Open the output structure, using the input one as a template and
*      instructing DSA not to force the creation of a new file.
*      If INPUT and OUTPUT happen to be the same, the data will be
*      altered "in situ".
*      Again tell DSA that a quality array will be used.
         CALL DSA_NAMED_OUTPUT( 'OUTFILE', OUTPUT,
     :     'INFILE', 0, 0, STATUS )
         CALL DSA_USE_QUALITY( 'OUTFILE', STATUS )

*      Obtain the size of the data array in the input structure (which
*      will also be the same size as the one in the output structure).
         CALL DSA_DATA_SIZE( 'INFILE', RMAXDIM, NDIM, DIMS,
     :     NELM, STATUS )

*      Map the data, variance and quality arrays in the input structure.
         CALL DSA_MAP_DATA( 'INFILE', 'READ', 'FLOAT', IDATA_PTR,
     :     IDATA_SLOT, STATUS )
         CALL DSA_MAP_VARIANCE( 'INFILE', 'READ', 'FLOAT',
     :     IVAR_PTR, IVAR_SLOT, STATUS )
         CALL DSA_MAP_QUALITY( 'INFILE', 'READ', 'BYTE',
     :     IQUAL_PTR, IQUAL_SLOT, STATUS )

*      Map the data, variance and quality arrays in the output structure.
         CALL DSA_MAP_DATA( 'OUTFILE', 'WRITE', 'FLOAT', ODATA_PTR,
     :     ODATA_SLOT, STATUS )
         CALL DSA_MAP_VARIANCE( 'OUTFILE', 'WRITE', 'FLOAT',
     :     OVAR_PTR, OVAR_SLOT, STATUS )
         CALL DSA_MAP_QUALITY( 'OUTFILE', 'WRITE', 'BYTE',
     :     OQUAL_PTR, OQUAL_SLOT, STATUS )

*      Check everything has worked so far.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Give a warning if the input array is not 1-D. GEN_MEND1F is
*         designed to work with 1-D arrays. It will work with 2-D
*         bu may do strange things at the ends of rows.
            IF ( NDIM .GT. 1 ) THEN

               CALL MSG_SETI( 'NDIM', NDIM )
               CALL MSG_OUT( ' ', 'NOTE - The result on a '/
     :           /'^NDIM-D array may be unreliable at the '/
     :           /'ends of each row', STATUS )
            END IF

*         Mend the data arrays.
            CALL GEN_MEND1F( NELM, %val(IDATA_PTR), %val(IVAR_PTR),
     :        %val(IQUAL_PTR), .TRUE., .FALSE., 0.0, .TRUE.,
     :        %val(ODATA_PTR), %val(OVAR_PTR), %val(OQUAL_PTR) )

*         Add a comment to the FITS structure.
            CALL DSA_PUT_FITS_C( 'OUTFILE', 'COMMENT',
     :        'Bad pixels removed by interpolation', ' ', STATUS )
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_MEND: '/
     :        /'Error opening DSA, accessing and '/
     :        /'mapping data structures', STATUS )
         END IF

*      Close DSA. This routine will attempt to tidy up and unmap any
*      workspace mapped above. It should be called even if an error
*      has occurred.
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MEND: '/
     :    /'Error obtaining %INPUT and %OUTPUT '/
     :    /'parameters', STATUS )
      END IF

      END
