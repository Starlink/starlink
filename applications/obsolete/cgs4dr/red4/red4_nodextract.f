      SUBROUTINE RED4_NODEXTRACT( STATUS )
*
*     R E D 4 _ N O D E X T R A C T
*
*     Allows for the extraction of 1, 2, or 3 nod-beam data
*
*     Command parameters -
*     'IMAGE'        The name of the image from which the rows are to be taken.
*     'INVERT_SPEC'  If beams were backwards, turn spectrum upside down?
*     'ALGORITHM'    Use a bright source or faint source algorithm?
*     'ROW1S'        The start Y-value of the first nod position to be used.
*     'ROW1E'        The end   Y-value of the first nod position to be used.
*     'ROW2S'        The start Y-value of the 2nd   nod position to be used.
*     'ROW2E'        The start Y-value of the 2nd   nod position to be used.
*     'ROW3S'        The start Y-value of the 3rd   nod position to be used.
*     'ROW3E'        The start Y-value of the 3rd   nod position to be used.
*     'SPECT'        The name of the resulting 1-D data.
*     'ISPECT'       The name of the resulting 2-D data.
*     Output data -
*     1-D SPECT plus an IMAGE version grown along the slit.
*
*     Modified:
*     26-Oct-1993 :  Original (cloned from red4_extract)       KLK ( JAC )
*     08-Nov-1993 :  Changed to red4_nodextract                KLK ( JAC )
*     07-Dec-1993 :  Allow 2-D output (also significantly
*                    re-worked to avoid other problems)        PND ( JAC )
*     24-May-1994 :  Add bright and faint source algorithm     PND ( JAC )
*+
      IMPLICIT NONE
*
*     Functions
      INTEGER GEN_BSEARCH
      INTEGER CHR_LEN
*
*     ADAM include files
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*
*     ADAM status
      INTEGER STATUS
      INTEGER DSA_TYPESIZE
*
*     Local variables
      LOGICAL   VARIANCE           ! TRUE if image has error array
      LOGICAL   QUALITY            ! TRUE if image has quality array
      LOGICAL   INVERT_SPEC        ! TRUE to invert ( near ) final spectrum
      INTEGER   FLOATSIZE          ! Number of bytes in a float
      INTEGER   BYTESIZE           ! Number of bytes in a byte
      INTEGER   INTSIZE            ! Number of bytes in an integer
      INTEGER   NLOW               ! Number of good points
      INTEGER   NHIGH              ! Number of bad points
      INTEGER   CLEN               ! The length of a string
      INTEGER   DIMS( 2 )          ! Image dimensions
      INTEGER   IPTR               ! VM address for image data
      INTEGER   ISLT               ! Slot number for mapped data - ignored
      INTEGER   IVPTR              !           "          variance
      INTEGER   IVSLT              !           "          variance
      INTEGER   IQSLT              !           "          quality
      INTEGER   IQPTR              !           "          quality
      INTEGER   A1PTR              !           "          Y-axis row 1
      INTEGER   A1SLT              ! Slot number for mapped data - ignored
      INTEGER   IYEN1              ! Last image row to extract row 1
      INTEGER   IYST1              ! First image row to extract row 1
      INTEGER   IYEN2              ! Last image row to extract row 2
      INTEGER   IYST2              ! First image row to extract row 2
      INTEGER   IYEN3              ! Last image row to extract row 3
      INTEGER   IYST3              ! First image row to extract row 3
      INTEGER   ITEMP              ! Temporary variable
      INTEGER   NBEAMS             ! Number of beams specified
      INTEGER   NDIM               ! Number of image dimensions
      INTEGER   NELM               ! Number of elements in image - ignored
      INTEGER   NX                 ! First dimension of image
      INTEGER   NY                 ! Second dimension of image
      INTEGER   TSP1PTR            ! VM address for spectrum data
      INTEGER   TSP1SLT            ! VM address for spectrum data
      INTEGER   TSPV1PTR           !           "            variance
      INTEGER   TSPV1SLT           !           "            variance
      INTEGER   TSPQ1PTR           !           "            quality
      INTEGER   TSPQ1SLT           !           "            quality
      INTEGER   TSP2PTR            ! VM address for spectrum data
      INTEGER   TSP2SLT            ! VM address for spectrum data
      INTEGER   TSPV2PTR           !           "            variance
      INTEGER   TSPV2SLT           !           "            variance
      INTEGER   TSPQ2PTR           !           "            quality
      INTEGER   TSPQ2SLT           !           "            quality
      INTEGER   TSP3PTR            ! VM address for spectrum data
      INTEGER   TSP3SLT            ! VM address for spectrum data
      INTEGER   TSPV3PTR           !           "            variance
      INTEGER   TSPV3SLT           !           "            variance
      INTEGER   TSPQ3PTR           !           "            quality
      INTEGER   TSPQ3SLT           !           "            quality
      INTEGER   TSP4PTR            ! VM address for spectrum data
      INTEGER   TSP4SLT            ! VM address for spectrum data
      INTEGER   TSPV4PTR           !           "            variance
      INTEGER   TSPV4SLT           !           "            variance
      INTEGER   TSPQ4PTR           !           "            quality
      INTEGER   TSPQ4SLT           !           "            quality
      INTEGER   TSP5PTR            ! VM address for spectrum data
      INTEGER   TSP5SLT            ! VM address for spectrum data
      INTEGER   TSPV5PTR           !           "            variance
      INTEGER   TSPV5SLT           !           "            variance
      INTEGER   TSPQ5PTR           !           "            quality
      INTEGER   TSPQ5SLT           !           "            quality
      INTEGER   TSP6PTR            ! VM address for spectrum data
      INTEGER   TSP6SLT            ! VM address for spectrum data
      INTEGER   TSPV6PTR           !           "            variance
      INTEGER   TSPV6SLT           !           "            variance
      INTEGER   TSPQ6PTR           !           "            quality
      INTEGER   TSPQ6SLT           !           "            quality
      INTEGER   SPTR               ! VM address for spectrum data
      INTEGER   SSLT               ! VM address for spectrum data
      INTEGER   SVPTR              !           "            variance
      INTEGER   SVSLT              !           "            variance
      INTEGER   SQPTR              !           "            quality
      INTEGER   SQSLT              !           "            quality
      INTEGER   ISPTR              ! VM address for image spectrum data
      INTEGER   ISSLT              ! VM address for image spectrum data
      INTEGER   ISVPTR             !           "                   variance
      INTEGER   ISVSLT             !           "                   variance
      INTEGER   ISQPTR             !           "                   quality
      INTEGER   ISQSLT             !           "                   quality
      INTEGER   NUM1PTR            !           "            work array
      INTEGER   NUM1SLT            !           "            work array
      INTEGER   NUM2PTR            !           "            work array
      INTEGER   NUM2SLT            !           "            work array
      INTEGER   NUM3PTR            !           "            work array
      INTEGER   NUM3SLT            !           "            work array
      INTEGER   SUM1PTR            !           "            work array
      INTEGER   SUM1SLT            !           "            work array
      INTEGER   SUM2PTR            !           "            work array
      INTEGER   SUM2SLT            !           "            work array
      INTEGER   SUM3PTR            !           "            work array
      INTEGER   SUM3SLT            !           "            work array
      INTEGER   SUMSQ1PTR          !           "            work array
      INTEGER   SUMSQ1SLT          !           "            work array
      INTEGER   SUMSQ2PTR          !           "            work array
      INTEGER   SUMSQ2SLT          !           "            work array
      INTEGER   SUMSQ3PTR          !           "            work array
      INTEGER   SUMSQ3SLT          !           "            work array
      REAL      ROW1S 		   ! Nod position 1 start
      REAL      ROW1E              ! Nod position 1 end
      REAL      ROW2S 		   ! Nod position 2 start
      REAL      ROW2E              ! Nod position 2 end
      REAL      ROW3S 		   ! Nod position 3 start
      REAL      ROW3E              ! Nod position 3 end
      REAL      FBAD               ! Value of bad pixel flag
      CHARACTER*15 ALGORITHM       ! Bright or faint source algorithm
      CHARACTER*80 IMAGE           ! Name of image file
      CHARACTER*80 SPECT           ! Name of file to contain extracted spectrum
      CHARACTER*80 ISPECT          ! Name of file to extracted image spectrum
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Open DSA routines
      CALL DSA_OPEN( STATUS )

*    Get data sizes
      FLOATSIZE = DSA_TYPESIZE( 'FLOAT', STATUS )
      BYTESIZE  = DSA_TYPESIZE( 'BYTE', STATUS )
      INTSIZE   = DSA_TYPESIZE( 'INT', STATUS )

*    Open IMAGE file
      CALL PAR_GET0C( 'IMAGE', IMAGE, STATUS )
      CALL RED4_CHECK_INPUT( IMAGE, STATUS )
      CALL DSA_NAMED_INPUT( 'IMAGE', IMAGE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IMAGE', IMAGE )
         CALL ERR_REP( ' ', 'RED4_NODEXTRACT4: '/
     :     /'Failed to get or open input image ^IMAGE', STATUS )
         GOTO 500
      ENDIF

*    Invert the final spectrum?
      INVERT_SPEC = .FALSE.
      CALL PAR_GET0L( 'INVERT_SPEC', INVERT_SPEC, STATUS )

*    Which algorithm should we use?
      CALL PAR_GET0C( 'ALGORITHM', ALGORITHM, STATUS )
      CALL CHR_UCASE( ALGORITHM )

*    Get size of data in IMAGE
      CALL DSA_DATA_SIZE( 'IMAGE', 2, NDIM, DIMS, NELM, STATUS )
      NX=DIMS( 1 )
      IF ( NDIM.EQ.1 ) THEN
        NY=1
      ELSE
        NY=DIMS( 2 )
      ENDIF

*    Get ranges of Y values for all beams
      NBEAMS = 0
      CALL PAR_GET0R( 'ROW1S', ROW1S, STATUS )
      CALL PAR_GET0R( 'ROW1E', ROW1E, STATUS )
      CALL PAR_GET0R( 'ROW2S', ROW2S, STATUS )
      CALL PAR_GET0R( 'ROW2E', ROW2E, STATUS )
      CALL PAR_GET0R( 'ROW3S', ROW3S, STATUS )
      CALL PAR_GET0R( 'ROW3E', ROW3E, STATUS )
      CALL DSA_MAP_AXIS_DATA( 'IMAGE', 2, 'READ', 'FLOAT',
     :  A1PTR, A1SLT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        IYST1 = GEN_BSEARCH( %val( A1PTR ), NY, ROW1S )
        IYEN1 = GEN_BSEARCH( %val( A1PTR ), NY, ROW1E )
        IF ( IYEN1 .LT. IYST1 ) THEN
          ITEMP = IYST1
          IYST1 = IYEN1
          IYEN1 = ITEMP
        ENDIF

        IYST2 = GEN_BSEARCH( %val( A1PTR ), NY, ROW2S )
        IYEN2 = GEN_BSEARCH( %val( A1PTR ), NY, ROW2E )
        IF ( IYEN2 .LT. IYST2 ) THEN
          ITEMP = IYST2
          IYST2 = IYEN2
          IYEN2 = ITEMP
        ENDIF

        IYST3 = GEN_BSEARCH( %val( A1PTR ), NY, ROW3S )
        IYEN3 = GEN_BSEARCH( %val( A1PTR ), NY, ROW3E )
        IF ( IYEN3 .LT. IYST3 ) THEN
          ITEMP = IYST3
          IYST3 = IYEN3
          IYEN3 = ITEMP
        ENDIF
      ENDIF

*    Set number of beams
      IF ( ( IYST1 .GE. 1 ) .AND. ( IYEN1 .GE. 1 ) .AND.
     :     ( IYST2 .LT. 1 ) .AND. ( IYEN2 .LT. 1 ) .AND.
     :     ( IYST3 .LT. 1 ) .AND. ( IYEN3 .LT. 1 ) ) THEN
        NBEAMS = 1
      ELSE IF ( ( IYST1 .GE. 1 ) .AND. ( IYEN1 .GE. 1 ) .AND.
     :     ( IYST2 .GE. 1 ) .AND. ( IYEN2 .GE. 1 ) .AND.
     :     ( IYST3 .LT. 1 ) .AND. ( IYEN3 .LT. 1 ) ) THEN
        NBEAMS = 2
      ELSEIF ( ( IYST1 .GE. 1 ) .AND. ( IYEN1 .GE. 1 ) .AND.
     :     ( IYST2 .GE. 1 ) .AND. ( IYEN2 .GE. 1 ) .AND.
     :     ( IYST3 .GE. 1 ) .AND. ( IYEN3 .GE. 1 ) ) THEN
        NBEAMS = 3
      ENDIF

*    Check that we have found at least one beam
      IF ( NBEAMS .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'RED4_NODEXTRACT4: '/
     :    /'Failed to detect any signal beams', STATUS )
        GOTO 500
      ENDIF

*    Search for error, quality arrays, magic values
      CALL DSA_GET_FLAG_VALUE ( 'FLOAT', FBAD, STATUS )
      CALL DSA_SEEK_ERRORS( 'IMAGE', VARIANCE, STATUS )
      CALL DSA_SEEK_QUALITY( 'IMAGE', QUALITY, STATUS )
*     CALL DSA_SEEK_FLAGGED_VALUES( 'IMAGE', FLAGGED, STATUS )

*    Create new spectrum file in 1 and 2-D
      CLEN = CHR_LEN(  IMAGE  )
      CALL PAR_DEF0C( 'SPECT', IMAGE(1:CLEN)//'_spc', STATUS )
      CALL PAR_GET0C( 'SPECT', SPECT, STATUS )
      CALL PAR_DEF0C( 'ISPECT', IMAGE(1:CLEN)//'_imspc', STATUS )
      CALL PAR_GET0C( 'ISPECT', ISPECT, STATUS )
      CALL DSA_NAMED_OUTPUT( 'ISPECT', ISPECT, 'IMAGE', 0, 1, STATUS )
      CALL DSA_NAMED_OUTPUT( 'SPECT', SPECT, 'IMAGE', 1, 1, STATUS )
      CALL DSA_RESHAPE_DATA( 'SPECT', 'IMAGE', 1, NX, STATUS )
      CALL DSA_RESHAPE_AXIS( 'SPECT', 1, 'IMAGE', 1, 1, NX, STATUS )

*    Tell DSA to use quality or FLAGGED values
      IF ( QUALITY ) THEN
        CALL DSA_USE_QUALITY( 'ISPECT', STATUS )
        CALL DSA_USE_QUALITY( 'SPECT', STATUS )
        CALL DSA_USE_QUALITY( 'IMAGE', STATUS )
*     ELSE IF ( FLAGGED ) THEN
*       CALL DSA_USE_FLAGGED_VALUES( 'ISPECT', STATUS )
*       CALL DSA_USE_FLAGGED_VALUES( 'SPECT', STATUS )
*       CALL DSA_USE_FLAGGED_VALUES( 'IMAGE', STATUS )
      ENDIF

*    Map the data, quality and variance arrays
      CALL DSA_MAP_DATA( 'IMAGE', 'READ', 'FLOAT',
     :  IPTR, ISLT, STATUS )
      IF ( VARIANCE ) CALL DSA_MAP_VARIANCE( 'IMAGE', 'READ', 'FLOAT',
     :  IVPTR, IVSLT, STATUS )
      IF ( QUALITY ) CALL DSA_MAP_QUALITY( 'IMAGE', 'READ', 'BYTE',
     :  IQPTR, IQSLT, STATUS )

*    Map the output spectrum and initialize it
      CALL DSA_MAP_DATA( 'SPECT', 'UPDATE', 'FLOAT',
     :  SPTR, SSLT, STATUS )
      CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SPTR) )
      CALL DSA_MAP_VARIANCE( 'SPECT', 'UPDATE', 'FLOAT',
     :  SVPTR, SVSLT, STATUS )
      CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SVPTR) )
      IF ( QUALITY ) THEN
        CALL DSA_MAP_QUALITY( 'SPECT', 'UPDATE', 'BYTE',
     :    SQPTR, SQSLT, STATUS )
        CALL GEN_FILL( NX*BYTESIZE, 0, %val(SQPTR) )
      END IF

*    Map the output spectral image and initialize it
      CALL DSA_MAP_DATA( 'ISPECT', 'UPDATE', 'FLOAT',
     :  ISPTR, ISSLT, STATUS )
      CALL GEN_FILL( NX*NY*FLOATSIZE, 0.0, %val(ISPTR) )
      CALL DSA_MAP_VARIANCE( 'ISPECT', 'UPDATE', 'FLOAT',
     :  ISVPTR, ISVSLT, STATUS )
      CALL GEN_FILL( NX*NY*FLOATSIZE, 0.0, %val(ISVPTR) )
      IF ( QUALITY ) THEN
        CALL DSA_MAP_QUALITY( 'ISPECT', 'UPDATE', 'BYTE',
     :    ISQPTR,ISQSLT, STATUS )
        CALL GEN_FILL( NX*NY*BYTESIZE, 0.0, %val(ISQPTR) )
      END IF

*    DEAL WITH EACH CASE INDIVIDUALLY TO AVOID RESERVING UNNECCESARY MEMORY

*    Case 1: Single beam extraction
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NBEAMS .EQ. 1 ) ) THEN

*       Get some work arrays and initialize them
         CALL DSA_GET_WORK_ARRAY( NX, 'INT',
     :     NUM1PTR, NUM1SLT, STATUS )
         CALL GEN_FILL( NX*INTSIZE, 0, %val(NUM1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUM1PTR, SUM1SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUM1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUMSQ1PTR, SUMSQ1SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUMSQ1PTR) )

*       Do the one beam extraction into SPTR
         CALL FIGE_XTRACT4( %val( IPTR ), %val( IVPTR ), %val( IQPTR ),
     :     NX, NY, IYST1, IYEN1, VARIANCE, QUALITY, .FALSE., FBAD,
     :     %val( SPTR ), %val( SVPTR ), %val( SQPTR ),
     :     %val( NUM1PTR ), %val( SUM1PTR ), %val( SUMSQ1PTR ) )

*      Release work arrays
         CALL DSA_FREE_WORKSPACE( NUM1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUM1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUMSQ1SLT, STATUS )

*    Case 2: Two beam extraction. Result = ( FIRST - SECOND ) / 2
      ELSE IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NBEAMS .EQ. 2 ) ) THEN

*       Get work arrays and initialize them
         CALL DSA_GET_WORK_ARRAY( NX, 'INT',
     :     NUM1PTR, NUM1SLT, STATUS )
         CALL GEN_FILL( NX*INTSIZE, 0, %val(NUM1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUM1PTR, SUM1SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUM1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUMSQ1PTR, SUMSQ1SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUMSQ1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'INT',
     :     NUM2PTR, NUM2SLT, STATUS )
         CALL GEN_FILL( NX*INTSIZE, 0, %val(NUM2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUM2PTR, SUM2SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUM2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUMSQ2PTR, SUMSQ2SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUMSQ2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSP1PTR, TSP1SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSP1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSP2PTR, TSP2SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSP2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSP3PTR, TSP3SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSP3PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSPV1PTR, TSPV1SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSPV1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSPV2PTR, TSPV2SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSPV2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSPV3PTR, TSPV3SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSPV3PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'BYTE',
     :     TSPQ1PTR, TSPQ1SLT, STATUS )
         CALL GEN_FILL( NX*BYTESIZE, 0, %val(TSPQ1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'BYTE',
     :     TSPQ2PTR, TSPQ2SLT, STATUS )
         CALL GEN_FILL( NX*BYTESIZE, 0, %val(TSPQ2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'BYTE',
     :     TSPQ3PTR, TSPQ3SLT, STATUS )
         CALL GEN_FILL( NX*BYTESIZE, 0, %val(TSPQ3PTR) )

*       Extract first beam into TSP1
         CALL FIGE_XTRACT4( %val( IPTR ), %val( IVPTR ), %val( IQPTR ),
     :     NX, NY, IYST1, IYEN1, VARIANCE, QUALITY, .FALSE., FBAD,
     :     %val( TSP1PTR ), %val( TSPV1PTR ), %val( TSPQ1PTR ),
     :     %val( NUM1PTR ), %val( SUM1PTR ), %val( SUMSQ1PTR ) )

*       Extract second beam into TSP2
         CALL FIGE_XTRACT4( %val( IPTR ), %val( IVPTR ), %val( IQPTR ),
     :     NX, NY, IYST2, IYEN2, VARIANCE, QUALITY, .FALSE., FBAD,
     :     %val( TSP2PTR ), %val( TSPV2PTR ), %val( TSPQ2PTR ),
     :     %val( NUM2PTR ), %val( SUM2PTR ), %val( SUMSQ2PTR ) )

*       Subtract them propagating to TSP3
         CALL GEN_SUBAFV( NX, %val( TSP1PTR ), %val( TSP2PTR ),
     :     %val( TSP3PTR ), %val( TSPQ1PTR ), %val( TSPQ2PTR ),
     :     %val( TSPQ3PTR ), %val( TSPV1PTR ), %val( TSPV2PTR ),
     :     %val( TSPV3PTR ), QUALITY, .FALSE., FBAD, VARIANCE )

*       Divide by two propagating to SPTR
         CALL GEN_MULCAFV( %val( TSP3PTR ), NX, 0.5, %val( SPTR ),
     :     %val( TSPQ3PTR ), %val( SQPTR ), %val( TSPV3PTR ),
     :     %val( SVPTR ), QUALITY, .FALSE., FBAD, VARIANCE )

*      Release work arrays
         CALL DSA_FREE_WORKSPACE( NUM1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUM1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUMSQ1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( NUM2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUM2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUMSQ2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSP1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSP2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSP3SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPV1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPV2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPV3SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPQ1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPQ2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPQ3SLT, STATUS )

*    Case 3: Three beam extraction. Result depends upon algorithm selected.
      ELSE IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NBEAMS .EQ. 3 ) ) THEN

*       Get some(!) work arrays and initialize them
         CALL DSA_GET_WORK_ARRAY( NX, 'INT',
     :     NUM1PTR, NUM1SLT, STATUS )
         CALL GEN_FILL( NX*INTSIZE, 0, %val(NUM1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'INT',
     :     NUM2PTR, NUM2SLT, STATUS )
         CALL GEN_FILL( NX*INTSIZE, 0, %val(NUM2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'INT',
     :     NUM3PTR, NUM3SLT, STATUS )
         CALL GEN_FILL( NX*INTSIZE, 0, %val(NUM3PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUM1PTR, SUM1SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUM1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUM2PTR, SUM2SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUM2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUM3PTR, SUM3SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUM3PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUMSQ1PTR, SUMSQ1SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUMSQ1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUMSQ2PTR, SUMSQ2SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUMSQ2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     SUMSQ3PTR, SUMSQ3SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(SUMSQ3PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSP1PTR, TSP1SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSP1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSP2PTR, TSP2SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSP2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSP3PTR, TSP3SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSP3PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSP4PTR, TSP4SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSP4PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSP5PTR, TSP5SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSP5PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSP6PTR, TSP6SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSP6PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSPV1PTR, TSPV1SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSPV1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSPV2PTR, TSPV2SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSPV2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSPV3PTR, TSPV3SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSPV3PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSPV4PTR, TSPV4SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSPV4PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSPV5PTR, TSPV5SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSPV5PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'FLOAT',
     :     TSPV6PTR, TSPV6SLT, STATUS )
         CALL GEN_FILL( NX*FLOATSIZE, 0.0, %val(TSPV6PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'BYTE',
     :     TSPQ1PTR, TSPQ1SLT, STATUS )
         CALL GEN_FILL( NX*BYTESIZE, 0, %val(TSPQ1PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'BYTE',
     :     TSPQ2PTR, TSPQ2SLT, STATUS )
         CALL GEN_FILL( NX*BYTESIZE, 0, %val(TSPQ2PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'BYTE',
     :     TSPQ3PTR, TSPQ3SLT, STATUS )
         CALL GEN_FILL( NX*BYTESIZE, 0, %val(TSPQ3PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'BYTE',
     :     TSPQ4PTR, TSPQ4SLT, STATUS )
         CALL GEN_FILL( NX*BYTESIZE, 0, %val(TSPQ4PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'BYTE',
     :     TSPQ5PTR, TSPQ5SLT, STATUS )
         CALL GEN_FILL( NX*BYTESIZE, 0, %val(TSPQ5PTR) )
         CALL DSA_GET_WORK_ARRAY( NX, 'BYTE',
     :     TSPQ6PTR, TSPQ6SLT, STATUS )
         CALL GEN_FILL( NX*BYTESIZE, 0, %val(TSPQ6PTR) )

*       Extract first beam into TSP1
         CALL FIGE_XTRACT4( %val( IPTR ), %val( IVPTR ), %val( IQPTR ),
     :     NX, NY, IYST1, IYEN1, VARIANCE, QUALITY, .FALSE., FBAD,
     :     %val( TSP1PTR ), %val( TSPV1PTR ), %val( TSPQ1PTR ),
     :     %val( NUM1PTR ), %val( SUM1PTR ), %val( SUMSQ1PTR ) )

*       Extract second beam into TSP2
         CALL FIGE_XTRACT4( %val( IPTR ), %val( IVPTR ), %val( IQPTR ),
     :     NX, NY, IYST2, IYEN2, VARIANCE, QUALITY, .FALSE., FBAD,
     :     %val( TSP2PTR ), %val( TSPV2PTR ), %val( TSPQ2PTR ),
     :     %val( NUM2PTR ), %val( SUM2PTR ), %val( SUMSQ2PTR ) )

*       Extract third beam into TSP3
         CALL FIGE_XTRACT4( %val( IPTR ), %val( IVPTR ), %val( IQPTR ),
     :     NX, NY, IYST3, IYEN3, VARIANCE, QUALITY, .FALSE., FBAD,
     :     %val( TSP3PTR ), %val( TSPV3PTR ), %val( TSPQ3PTR ),
     :     %val( NUM3PTR ), %val( SUM3PTR ), %val( SUMSQ3PTR ) )

*       Add first and third beams propagating to TSP4
         CALL GEN_ADDAFV( NX, %val( TSP1PTR ), %val( TSP3PTR ),
     :     %val( TSP4PTR ), %val( TSPQ1PTR ), %val( TSPQ3PTR ),
     :     %val( TSPQ4PTR ), %val( TSPV1PTR ), %val( TSPV3PTR ),
     :     %val( TSPV4PTR ), QUALITY, .FALSE., FBAD, VARIANCE )

*       Result = ( MIDDLE - ( TOP+BOTTOM ) ) / 4
         IF ( ALGORITHM .NE. 'FAINT' ) THEN

           CALL MSG_OUT( ' ', 'Using BRIGHT source algorithm '/
     :       /'to extract spectra', STATUS )

*         Subtract TSP4 from TSP2 propagating to TSP5
           CALL GEN_SUBAFV( NX, %val( TSP2PTR ), %val( TSP4PTR ),
     :       %val( TSP5PTR ), %val( TSPQ2PTR ), %val( TSPQ4PTR ),
     :       %val( TSPQ5PTR ), %val( TSPV2PTR ), %val( TSPV4PTR ),
     :       %val( TSPV5PTR ), QUALITY, .FALSE., FBAD, VARIANCE )

*         Multiply TSP5 by 0.25 propagating to SPTR
           CALL GEN_MULCAFV( %val( TSP5PTR ), NX, 0.25, %val( SPTR ),
     :       %val( TSPQ5PTR ), %val( SQPTR ), %val( TSPV5PTR ),
     :       %val( SVPTR ), QUALITY, .FALSE., FBAD, VARIANCE )

*    Result = ( MIDDLE - ( TOP+BOTTOM )/2 ) /3
         ELSE
           CALL MSG_OUT( ' ', 'Using FAINT source algorithm '/
     :       /'to extract spectra', STATUS )

*         Multiply TSP4 by 0.5 propagating to TSP5
           CALL GEN_MULCAFV( %val( TSP4PTR ), NX, 0.5, %val( TSP5PTR ),
     :       %val( TSPQ4PTR ), %val( TSPQ5PTR ), %val( TSPV4PTR ),
     :       %val( TSPV5PTR ), QUALITY, .FALSE., FBAD, VARIANCE )

*         Subtract TSP5 from TSP2 propagating to TSP6
           CALL GEN_SUBAFV( NX, %val( TSP2PTR ), %val( TSP5PTR ),
     :       %val( TSP6PTR ), %val( TSPQ2PTR ), %val( TSPQ5PTR ),
     :       %val( TSPQ6PTR ), %val( TSPV2PTR ), %val( TSPV5PTR ),
     :       %val( TSPV6PTR ), QUALITY, .FALSE., FBAD, VARIANCE )

*         Multiply TSP6 by 1.33333 propagating to SPTR
           CALL GEN_MULCAFV( %val( TSP6PTR ), NX, 0.33333333,
     :       %val( SPTR ), %val( TSPQ6PTR ), %val( SQPTR ),
     :       %val( TSPV6PTR ), %val( SVPTR ), QUALITY, .FALSE.,
     :       FBAD, VARIANCE )
         ENDIF

*      Release work arrays
         CALL DSA_FREE_WORKSPACE( NUM1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUM1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUMSQ1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( NUM2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUM2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUMSQ2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( NUM3SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUM3SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( SUMSQ3SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSP1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSP2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSP3SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSP4SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSP5SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSP6SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPV1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPV2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPV3SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPV4SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPV5SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPV6SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPQ1SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPQ2SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPQ3SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPQ4SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPQ5SLT, STATUS )
         CALL DSA_FREE_WORKSPACE( TSPQ6SLT, STATUS )
      ENDIF

*    Invert spectrum if required
      IF ( ( STATUS .EQ. SAI__OK ) .AND. INVERT_SPEC ) THEN
         CALL GEN_MULCAFV( %val( SPTR ), NX, -1.0, %val( SPTR ),
     :     %val( SQPTR ), %val( SQPTR ), %val( SVPTR ), %val( SVPTR ),
     :     QUALITY, .FALSE., FBAD, VARIANCE )
      ENDIF

*    Clean the data (clipping values below -65530.0)
      CALL GEN_CLIPF( %val(SPTR), NX, -65530.0, VAL__MAXR,
     :   NLOW, NHIGH, %val(SPTR) )

*    Grow the data, variance and quality into the output image
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL GEN_GROWX(  %val( SPTR ), NX, NY,
     :     1, NY, %val( ISPTR )  )
         CALL GEN_GROWX(  %val( SVPTR ), NX, NY,
     :     1, NY, %val( ISVPTR )  )
         CALL GEN_GROWXB(  %val( SQPTR ), NX, NY,
     :     1, NY, %val( ISQPTR )  )
      ENDIF

*    Clean the data quality
      CALL GEN_FILL( NX*BYTESIZE, 0, %val(SQPTR) )

*    Close down everything
 500  CONTINUE
      CALL DSA_CLOSE( STATUS )
      END
