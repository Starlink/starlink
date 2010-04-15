*+  RED4_POLYFIT, enhances sky subtraction by fitting a polynomial
      SUBROUTINE RED4_POLYFIT( STATUS )
*    Description :
*     This routine enhances sky subtraction by fitting a polynomial
*     to point contained in up to 4 sky areas. This routine is used
*     for reduced groups and objects only whereas OBJ-SKY pairs are
*     dealt with in RED4_RPOLYFIT.
*    Invocation :
*     CALL RED4_POLYFIT( STATUS )
*    Parameters :
*     STATUS    = INTEGER( UPDATE )
*           Global ADAM status.
*    Authors :
*     Phil Daly (JACH::PND)
*    History :
*     01-Jul-1990: Original version (PND)
*     22-Feb-1993: Conform to error strategy (PND)
*     07-Nov-1994: Make vaguely portable (AB)
*     09-Jan-1996: Remove NAG dependency (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS                 ! Inherited ADAM status
*    External references :
      INTEGER CHR_LEN                ! Character length determining function
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'      ! RED4 common block
*    Local Constants :
      INTEGER DSA__OK                ! DSA OK Status
      PARAMETER ( DSA__OK = 0 )
      INTEGER MAX_DEGREE
      PARAMETER ( MAX_DEGREE = 10 )  ! Max degree for fit (keep same as .ifl file!)
*    Local variables :
      INTEGER
     :  CLEN,                        ! Length of a string
     :  DIMS(2),                     ! Image dimensions
     :  NDIM,                        ! Number of image dimensions
     :  NELM,                        ! Number of elements in image - ignored
     :  NX,                          ! First dimension of image
     :  NY,                          ! Second dimension of image
     :  OPTR1,                       ! Dynamic memory element for image data
     :  OSLT1,                       ! Dynamic memory slot for image data
     :  QPTR1,                       ! Output quality pointer for image
     :  QSLT1                        ! Output quality slot for image
      INTEGER
     :  TEMP,                        ! Temporary value
     :  VPTR1,                       ! Dynamic memory element for image variance
     :  VSLT1,                       ! Dynamic memory slort for image variance
     :  WPTR,                        ! Dynamic memory element of W array
     :  WSLT,                        ! Dynamic memory slot of W array
     :  W1PTR,                       ! Dynamic memory element of W1 array
     :  W1SLT,                       ! Dynamic memory slot of W1 array
     :  W1BYTES,                     ! Number of bytes required
     :  XPTR,                        ! Dynamic memory element of X array
     :  XSLT,                        ! Dynamic memory slot of X array
     :  YPTR,                        ! Dynamic memory element of Y array
     :  YSLT,                        ! Dynamic memory slot of Y array
     :  COLON_POS                    ! position of colon in string
      LOGICAL
     :  EXIST                        ! TRUE if error information present
      CHARACTER
     :  COMMENT*4,                   ! Dummy comment
     :  OBS_TYPE*20,                 ! Observation type
     :  INPUT*80,                    ! The name of the input file
     :  OUTPUT*80                    ! The name of the output file
      CHARACTER*20 LPREFIX           ! prefix to add to file
*    Internal References :
*    Local data :
*-

*    Check for status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get name of input file for input and output
      CALL PAR_GET0C( 'INPUT', INPUT, STATUS )
      CLEN = MAX( 1, CHR_LEN( INPUT ) )
      OUTPUT = INPUT(1:CLEN) // '_polysky'
      CALL PAR_DEF0C( 'OUTPUT', OUTPUT, STATUS )
      CALL PAR_GET0C( 'OUTPUT', OUTPUT, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_POLYFIT: '/
     :     /'Failed to get input or set output', STATUS )
      ENDIF

*    Get POLYFIT option
      CALL PAR_GET0C( 'PF_POLYFIT', PF_POLYFIT, STATUS )

*    Make sure prefix is correct
      IF ( PF_POLYFIT .EQ. 'REDUCED_GRP') THEN

         CALL RED4_GET_PREFIX ('RG', LPREFIX, STATUS)

         COLON_POS = INDEX( INPUT, ':' )
         IF ( COLON_POS .EQ. 0 ) COLON_POS = INDEX( INPUT, '/' )
         IF ( COLON_POS .EQ. 0 ) INPUT = LPREFIX(:CHR_LEN(LPREFIX))//INPUT

         COLON_POS = INDEX( OUTPUT, ':' )
         IF ( COLON_POS .EQ. 0 ) COLON_POS = INDEX( OUTPUT, '/' )
         IF ( COLON_POS .EQ. 0 ) OUTPUT = LPREFIX(:CHR_LEN(LPREFIX))//OUTPUT

      ELSE IF ( PF_POLYFIT(1:3) .EQ. 'OBJ' ) THEN

         CALL RED4_GET_PREFIX ('RO', LPREFIX, STATUS)

         COLON_POS = INDEX( INPUT, ':' )
         IF ( COLON_POS .EQ. 0 ) COLON_POS = INDEX( INPUT, '/' )
         IF ( COLON_POS .EQ. 0 ) INPUT = LPREFIX(:CHR_LEN(LPREFIX))//INPUT

         COLON_POS = INDEX( OUTPUT, ':' )
         IF ( COLON_POS .EQ. 0 ) COLON_POS = INDEX( OUTPUT, '/' )
         IF ( COLON_POS .EQ. 0 ) OUTPUT = LPREFIX(:CHR_LEN(LPREFIX))//OUTPUT

      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_POLYFIT: '/
     :     /'Action only defined for group or object files', STATUS )
      ENDIF

*    Open DSA
      CALL DSA_OPEN( STATUS )
      IF ( STATUS .NE. DSA__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_POLYFIT: '/
     :     /'Error opening DSA', STATUS )
      END IF

*    Get name of input file and map it
      CALL RED4_CHECK_INPUT( INPUT, STATUS )
      CALL DSA_NAMED_INPUT( 'INFILE', INPUT, STATUS )

*    Get the observation type for OBJECTs only
      IF ( PF_POLYFIT .EQ. 'OBJECT' ) THEN

         CALL DSA_GET_FITS_C( 'INFILE', 'OBSTYPE', 0, OBS_TYPE,
     :      COMMENT, STATUS )
         CALL CHR_UCASE( OBS_TYPE )

         IF ( (OBS_TYPE(1:6).NE.'OBJECT')  .AND.
     :        (OBS_TYPE(1:3).NE.'SKY') )  THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBS_TYPE', OBS_TYPE )
            CALL ERR_REP( ' ', 'RED4_POLYFIT: '/
     :         /'Unable to sky subtract ^OBS_TYPE '/
     :         /'observations', STATUS )
         ENDIF
      ENDIF

*    Get size of data in the input image
      CALL DSA_DATA_SIZE( 'INFILE', 2, NDIM, DIMS, NELM, STATUS )
      IF ( STATUS .NE. DSA__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_POLYFIT: '/
     :     /'Error sizing data structure', STATUS )
      END IF

      IF ( NDIM.NE.2 ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_POLYFIT: '/
     :     /'Data Must be 2 Dimensional', STATUS )
      ELSE

         NX=DIMS(1)
         NY=DIMS(2)
      END IF

*    Is there error information?
      CALL DSA_SEEK_ERRORS( 'INFILE', EXIST, STATUS )

*    If there is error information, should it be used to weight the fit?
      IF ( EXIST ) THEN

         CALL PAR_GET0L( 'PF_WEIGHT', PF_WEIGHT, STATUS )
      ELSE

         PF_WEIGHT = .FALSE.
      ENDIF

*    Get the polynomial degree and rejection parameters
      CALL PAR_GET0I( 'PF_DEGREE', PF_DEGREE, STATUS )
      CALL PAR_GET0I( 'PF_NREJECT', PF_NREJECT, STATUS )

*    Get range of Y values for first region and put in ascending order
      CALL PAR_GET0I( 'PF_SAYS1', PF_SAYS1, STATUS )
      CALL PAR_GET0I( 'PF_SAYE1', PF_SAYE1, STATUS )
      IF ( PF_SAYE1 .LT. PF_SAYS1 ) THEN

         TEMP = PF_SAYE1
         PF_SAYE1 = PF_SAYS1
         PF_SAYS1 = TEMP
      ENDIF

*    Get range of Y values for second region and put in ascending order
      CALL PAR_GET0I( 'PF_SAYS2', PF_SAYS2, STATUS )
      CALL PAR_GET0I( 'PF_SAYE2', PF_SAYE2, STATUS )
      IF ( PF_SAYE2 .LT. PF_SAYS2 ) THEN

         TEMP = PF_SAYE2
         PF_SAYE2 = PF_SAYS2
         PF_SAYS2 = TEMP
      ENDIF

*    Get range of Y values for third region and put in ascending order
      CALL PAR_GET0I( 'PF_SAYS3', PF_SAYS3, STATUS )
      CALL PAR_GET0I( 'PF_SAYE3', PF_SAYE3, STATUS )
      IF ( PF_SAYE3 .LT. PF_SAYS3 ) THEN

         TEMP = PF_SAYE3
         PF_SAYE3 = PF_SAYS3
         PF_SAYS3 = TEMP
      ENDIF

*    Get range of Y values for fourth region and put in ascending order
      CALL PAR_GET0I( 'PF_SAYS4', PF_SAYS4, STATUS )
      CALL PAR_GET0I( 'PF_SAYE4', PF_SAYE4, STATUS )
      IF ( PF_SAYE4 .LT. PF_SAYS4 ) THEN

         TEMP = PF_SAYE4
         PF_SAYE4 = PF_SAYS4
         PF_SAYS4 = TEMP
      ENDIF

*    Check that regions 1 and 2 don't overlap
      IF ( ( PF_SAYS2 .LE. PF_SAYE1 ) .AND. ( PF_SAYS2 .GT. 0 ) ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_POLYFIT: '/
     :     /'Regions 1 and 2 must not overlap', STATUS )
      ENDIF

*    Check that regions 2 and 3 don't overlap
      IF ( ( PF_SAYS3 .LE. PF_SAYE2 ) .AND. ( PF_SAYS3 .GT. 0 ) ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_POLYFIT: '/
     :     /'Regions 2 and 3 must not overlap', STATUS )
      ENDIF

*    Check that regions 3 and 4 don't overlap
      IF ( ( PF_SAYS4 .LE. PF_SAYE3 ) .AND. ( PF_SAYS4 .GT. 0 ) ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_POLYFIT: '/
     :     /'Regions 3 and 4 must not overlap', STATUS )
      ENDIF

*    Create output file
      CALL DSA_NAMED_OUTPUT( 'OUTFILE', OUTPUT, 'INFILE', 0, 0, STATUS )
      CALL DSA_USE_QUALITY( 'OUTFILE', STATUS )

*    Map the output data
      CALL DSA_MAP_DATA( 'OUTFILE', 'UPDATE', 'FLOAT',
     :  OPTR1 , OSLT1, STATUS )
      CALL DSA_MAP_VARIANCE( 'OUTFILE', 'UPDATE', 'FLOAT',
     :  VPTR1, VSLT1, STATUS )
      CALL DSA_MAP_QUALITY('OUTFILE', 'UPDATE', 'BYTE',
     :  QPTR1, QSLT1, STATUS )

*    Get workspace arrays needed for polynomial fitting
      CALL DSA_GET_WORK_ARRAY( NY, 'DOUBLE', XPTR, XSLT, STATUS )
      CALL DSA_GET_WORK_ARRAY( NY, 'DOUBLE', YPTR, YSLT, STATUS )
      CALL DSA_GET_WORK_ARRAY( NY, 'DOUBLE', WPTR, WSLT, STATUS )
      W1BYTES = 4*NY + 3*(MAX_DEGREE+1)
      CALL DSA_GET_WORK_ARRAY( W1BYTES, 'DOUBLE', W1PTR, W1SLT, STATUS )

*    Process data
      IF ( STATUS .EQ. SAI__OK ) THEN

         CALL FIG_POLYFIT( NX, NY, W1BYTES, %val(OPTR1), %val(VPTR1),
     :     %val(QPTR1), %val(XPTR), %val(YPTR), %val(WPTR),
     :     %val(W1PTR), PF_SAYS1, PF_SAYE1, PF_SAYS2, PF_SAYE2,
     :     PF_SAYS3, PF_SAYE3, PF_SAYS4, PF_SAYE4, PF_DEGREE,
     :     PF_NREJECT, PF_WEIGHT )
      ENDIF

*    Exit from subroutine
      CALL DSA_CLOSE( STATUS )
      IF ( STATUS .NE. DSA__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_POLYFIT: '/
     :     /'Error closing DSA', STATUS )
      END IF
      END
