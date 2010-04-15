*+  RED4_CRE_ERROR_MASK - Create a bad pixel mask from error array
      SUBROUTINE RED4_CRE_ERROR_MASK( STATUS )
*    Description :
*     This routine creates a bad pixel mask by replacing the data array
*     with the error array. It writes the data to a temporary named
*     output file of the form CGS4_MASKS:MASK_yymmdd_TEMPLATE.DST.
*    Invocation :
*     CALL RED4_CRE_ERROR_MASK( STATUS )
*    Parameters :
*     STATUS      = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Daly (JACH::PND)
*    History :
*     30-Jan-1992: Original version.                          (PND)
*     18-Feb-1993: Conform to error strategy                  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'
*    Local Constants :
      INTEGER NINFO              ! Number of information items
      PARAMETER ( NINFO = 2 )
*    Local variables :
      INTEGER
     :  NDIM,                    ! Number of dimensions in input data array
     :  DIMS( MAXDIM ),          ! Dimensions of input data array
     :  NELM,                    ! Total number of elements in input data array
     :  NX,                      ! Dimensions in X
     :  NY,                      ! Dimensions in Y
     :  FITS_DETNINCR,           ! Number of detector increments
     :  ADDRESS,                 ! Address of mapped arrays
     :  INPUT_SLOT,              ! Reference slot for mapped input data
     :  INPUT_PTR,               ! Address of input data.
     :  MASK_SLOT,               ! Reference slot for mapped mask data
     :  MASK_PTR                 ! Address of mask data.
      CHARACTER
     :  INPUT*80,                ! Name of input data structure
     :  MASK*80,                 ! Name of output data structure for mask
     :  DATA_INFO( NINFO )*32,   ! Labels and units for the data.
     :  COMMENT*4                ! Dummy comment
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    Obtain the name of the input and output structures
      CALL PAR_GET0C( 'INPUT', INPUT, STATUS )
      CALL PAR_GET0C( 'MASK', MASK, STATUS )

*    Check these parameters have been obtained succesfully
      IF ( STATUS .EQ. ADAM__OK ) THEN

*       Open DSA and attempt to open the input data structure
         CALL DSA_OPEN( STATUS )
         CALL RED4_CHECK_INPUT( INPUT, STATUS )
         CALL DSA_NAMED_INPUT( 'INPUT', INPUT, STATUS )

*       Obtain the size of the data array in the input structure
         CALL DSA_DATA_SIZE( 'INPUT', MAXDIM, NDIM, DIMS, NELM, STATUS )
         NX = DIMS( 1 )
         NY = DIMS( 2 )

*       Get FITS parameter `DETNINCR - Detector increments' for input
         CALL DSA_GET_FITS_I( 'INPUT', 'DETNINCR', 0, FITS_DETNINCR,
     :      COMMENT, STATUS )

*       Check this has worked
         IF ( STATUS .EQ. ADAM__OK ) THEN

*          Ensure the input data structure has not been oversampled
            IF ( FITS_DETNINCR .EQ. 1 ) THEN

*             Open the mask temporry file CGS4_MASKS:MASK_yymmdd_TEMPLATE.DST
               CALL DSA_NAMED_OUTPUT( 'MASK', MASK, 'INPUT',
     :            0, 0, STATUS )

*             Map the variance array of input and the data array of output
               CALL DSA_MAP_VARIANCE( 'INPUT', 'READ', 'FLOAT',
     :            ADDRESS, INPUT_SLOT, STATUS )
               INPUT_PTR = ADDRESS
               CALL DSA_MAP_DATA( 'MASK', 'WRITE', 'FLOAT',
     :            ADDRESS, MASK_SLOT, STATUS )
               MASK_PTR = ADDRESS

*             Write the error array from input to data array of output
               CALL RED4_CRE_ERROR_MASK2 ( NX, NY, %val(INPUT_PTR),
     :            %val(MASK_PTR), STATUS )

*             Check this has worked
               IF ( STATUS .EQ. ADAM__OK ) THEN

*                Write the data labels for output structure Z-array
                  DATA_INFO(1) = 'A/D numbers per exposure'
                  DATA_INFO(2) = 'Intermediate Bad Pixel Mask'
                  CALL DSA_SET_DATA_INFO( 'MASK', NINFO,
     :               DATA_INFO, 0, 0.0D0, STATUS )
               ELSE

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_CRE_ERROR_MASK: '/
     :               /'Failed to create mask or '/
     :               /'map data arrays', STATUS )
               END IF
            ELSE

               STATUS = SAI__ERROR
               CALL MSG_SETI( 'DETNINCR', FITS_DETNINCR )
               CALL ERR_REP( ' ', 'RED4_CRE_ERROR_MASK: '/
     :            /'Input data are oversampled '/
     :            /'by a factor of ^DETNINCR and not 1', STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_CRE_ERROR_MASK: '/
     :        /'Failed to access data structures', STATUS )
         END IF

*       Close DSA
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_CRE_ERROR_MASK: '/
     :     /'Failed to get %INPUT or %MASK', STATUS )
      END IF

      END
