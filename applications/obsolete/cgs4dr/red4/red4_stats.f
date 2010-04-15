*+  RED4_STATS - get stats about image
      SUBROUTINE RED4_STATS( STATUS )
*    Description :
*     This routine displays stats about an image
*    Invocation :
*     CALL RED4_STATS( STATUS )
*    Authors :
*     P.N.Daly (JACH::PND)
*    History :
*     28-Apr-1995: Original version                           (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local Constants :
      INTEGER MAXDIM
      PARAMETER ( MAXDIM = 3 )            ! Maximum number of dimensions
*    Local variables :
      LOGICAL ERRORS                      ! T if want to plot errors
      LOGICAL QUALITY                     ! T if want to plot quality
      INTEGER NDIM                        ! info on size of data array
      INTEGER DIMS( MAXDIM )              !              "
      INTEGER NELM                        !              "
      INTEGER DATA_SLOT                   ! for data array
      INTEGER DATA_PTR                    !              "
      INTEGER QUAL_SLOT                   ! for quality array
      INTEGER QUAL_PTR                    !              "
      INTEGER WORK_SLOT                   ! for work array
      INTEGER WORK_PTR                    !              "
      INTEGER FLOATSIZE
      INTEGER DSA_TYPESIZE
*   Variables for parameters
      CHARACTER*132 DATA                  ! Name of data structure
      CHARACTER*80 PLANE                  ! DATA or ERRORS
      LOGICAL WHOLE                       ! T if want whole array analyzed
      INTEGER ISTART, IEND, IINCR         ! Sub-array and increment in X
      INTEGER JSTART, JEND, JINCR         ! Sub-array and increment in Y
      LOGICAL AUTOSCALE                   ! T if want autoscaling
      REAL LOW, HIGH                      ! High/Low value for autoscaling
*   Variables for returned values
      REAL MEAN, SIGMA, MEDIAN, MODE      ! Returned values
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get parameters
      FLOATSIZE = DSA_TYPESIZE( 'FLOAT', STATUS )
      CALL PAR_GET0C( 'DATA', DATA, STATUS )
      CALL PAR_GET0C( 'PLANE', PLANE, STATUS )
      CALL PAR_CANCL( 'PLANE', STATUS )
      CALL CHR_UCASE( PLANE )
      CALL CHR_RMBLK( PLANE )
      CALL PAR_GET0L( 'WHOLE', WHOLE, STATUS )
      CALL PAR_CANCL( 'WHOLE', STATUS )
      IF ( .NOT. WHOLE ) THEN
        CALL PAR_GET0I( 'ISTART', ISTART, STATUS )
        CALL PAR_GET0I( 'IEND', IEND, STATUS )
        CALL PAR_GET0I( 'JSTART', JSTART, STATUS )
        CALL PAR_GET0I( 'JEND', JEND, STATUS )
      ENDIF
      CALL PAR_GET0I( 'IINCR', IINCR, STATUS )
      CALL PAR_GET0I( 'JINCR', JINCR, STATUS )
      CALL PAR_GET0L( 'AUTOSCALE', AUTOSCALE, STATUS )
      CALL PAR_CANCL( 'AUTOSCALE', STATUS )
      IF ( .NOT. AUTOSCALE ) THEN
        CALL PAR_GET0R( 'LOW', LOW, STATUS )
        CALL PAR_GET0R( 'HIGH', HIGH, STATUS )
      ENDIF

*    Open DSA and the data structure
      CALL DSA_OPEN( STATUS )
      CALL RED4_CHECK_INPUT( DATA, STATUS )
      CALL DSA_NAMED_INPUT( 'DATA', DATA, STATUS )
      CALL DSA_DATA_SIZE( 'DATA', MAXDIM, NDIM, DIMS, NELM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ',
     :    'Error while opening DSA and data structure', STATUS )
        GOTO 500
      ENDIF
      IF ( NDIM .EQ. 1 ) DIMS(2) = 1

*    If the plane is 'errors', check that they are present
      ERRORS = .FALSE.
      IF ( PLANE .EQ. 'ERRORS' ) THEN
        CALL DSA_SEEK_ERRORS( 'DATA', ERRORS, STATUS )
        IF ( .NOT. ERRORS ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ',
     :       'The structure does not have an error array', STATUS )
          GOTO 500
        ENDIF
      ENDIF

*   Check for quality informatiom
      QUALITY = .FALSE.
      CALL DSA_SEEK_QUALITY( 'DATA', QUALITY, STATUS )
      IF ( QUALITY ) CALL DSA_USE_QUALITY( 'DATA', STATUS )

*   Map the array
      DATA_PTR = 0
      IF ( PLANE .EQ. 'DATA' ) THEN
         CALL DSA_MAP_DATA( 'DATA', 'READ', 'FLOAT',
     :      DATA_PTR, DATA_SLOT, STATUS )
      ELSE IF ( PLANE .EQ. 'ERRORS' ) THEN
         CALL DSA_MAP_ERRORS( 'DATA', 'READ', 'FLOAT',
     :      DATA_PTR, DATA_SLOT, STATUS )
      ENDIF

*   If it has quality information use it
      QUAL_PTR = 0
      IF ( QUALITY ) CALL DSA_MAP_QUALITY( 'DATA', 'READ', 'BYTE',
     :   QUAL_PTR, QUAL_SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ',
     :    'Error while mapping data, variance or quality array', STATUS )
        GOTO 500
      ENDIF

*   Get workspace for array
      WORK_PTR = 0
      WORK_SLOT = 0
      CALL DSA_GET_WORK_ARRAY( DIMS(1)*DIMS(2), 'DOUBLE', WORK_PTR,
     :   WORK_SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP(' ', 'RED4_STATS: Failed to obatin work array', STATUS )
        GOTO 500
      ELSE
        CALL GEN_FILL( FLOATSIZE*NELM, 0.0, %val(WORK_PTR) )
      ENDIF

*   Get the stats
      CALL RED4_GET_STATS( DIMS(1), DIMS(2), %val(DATA_PTR), %val(WORK_PTR),
     :   QUALITY, %val(QUAL_PTR), WHOLE, ISTART, IEND, IINCR, JSTART, JEND,
     :   JINCR, AUTOSCALE, HIGH, LOW, MEAN, SIGMA, MEDIAN, MODE, STATUS )

*   Unmap the work array
      IF ( WORK_SLOT .NE. 0 ) CALL DSA_FREE_WORKSPACE( WORK_SLOT, STATUS )

*  Set the values
      CALL PAR_PUT0R( 'MEAN', MEAN, STATUS )
      CALL PAR_PUT0R( 'SIGMA', SIGMA, STATUS )
      CALL PAR_PUT0R( 'MEDIAN', MEDIAN, STATUS )
      CALL PAR_PUT0R( 'MODE', MODE, STATUS )

*  Report the values
      CALL MSG_SETR( 'MEAN', MEAN )
      CALL MSG_OUT( ' ' , 'Mean   =  ^MEAN', STATUS )
      CALL MSG_SETR( 'SIGMA', SIGMA )
      CALL MSG_OUT( ' ' , 'Sigma  =  ^SIGMA', STATUS )
      CALL MSG_SETR( 'MEDIAN', MEDIAN )
      CALL MSG_OUT( ' ' , 'Median =  ^MEDIAN', STATUS )
      CALL MSG_SETR( 'MODE', MODE )
      CALL MSG_OUT( ' ' , 'Mode   =  ^MODE', STATUS )

*  Trap errors and close down DSA
500   CONTINUE
      CALL DSA_CLOSE( STATUS )
      END
