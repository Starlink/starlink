*+  RED4_RPOLYFIT, enhances sky subtraction by fitting a polynomial
      SUBROUTINE RED4_RPOLYFIT( NX, NY, DATA, VAR, QUAL, STATUS )
*    Description :
*     This routine enhances sky subtraction by fitting a polynomial
*     to points contained in up to 4 sky areas. This routine is used
*     for OBJECT and OBJ-SKY options whereas reduced groups are
*     dealt with in RED4_POLYFIT.
*    Invocation :
*     CALL RED4_RPOLYFIT( NX, NY, DATA, VAR, QUAL, STATUS )
*    Parameters :
*     NX        = INTEGER( READ)
*           First dimension
*     NY        = INTEGER( READ)
*           Second dimension
*     DATA      = REAL( UPDATE )
*           Address of image data
*     VAR       = REAL( UPDATE )
*           Address of image variance
*     QUAL      = REAL( UPDATE )
*           Address of image quality
*     STATUS    = INTEGER( UPDATE )
*           Global ADAM status.
*    Method :
*    Deficiencies :
*     None known
*    Bugs :
*     None known
*    Authors :
*     Phil Daly (JACH::PND)
*    History :
*     01-Jul-1990: Original version (PND)
*     23-Feb-1993: Conform to error strategy (PND)
*     09-Jan-1996: Remove NAG dependency (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'              ! Contains SAI__ERROR
*    Status :
      INTEGER STATUS                 ! Inherited ADAM status
*    External references :
*     INTEGER CHR_LEN                ! Character length determining function
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'      ! RED4 common block
*    Local Constants :
      INTEGER MAX_DEGREE
      PARAMETER ( MAX_DEGREE = 10 )
*    Local variables :
      INTEGER
     :  NX,                          ! First dimension of image
     :  NY,                          ! Second dimension of image
     :  TEMP,                        ! Temporary value
     :  WPTR,                        ! Dynamic memory element of W array
     :  WSLT,                        ! Dynamic memory slot of W array
     :  W1PTR,                       ! Dynamic memory element of W1 array
     :  W1SLT,                       ! Dynamic memory slot of W1 array
     :  W1BYTES,                     ! Number of bytes required
     :  XPTR,                        ! Dynamic memory element of X array
     :  XSLT,                        ! Dynamic memory slot of X array
     :  YPTR,                        ! Dynamic memory element of Y array
     :  YSLT                         ! Dynamic memory slot of Y array
      REAL
     :  DATA( NX, NY ),              ! Data array
     :  VAR( NX, NY ),               ! Variance array
     :  QUAL( NX, NY )               ! Quality array
*-

*    Check for status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Make sure option is for OBJ-SKY only
      IF ( PF_POLYFIT .EQ. 'NONE' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_RPOLYFIT:'/
     :     /'Polyfit option not enabled', STATUS )
      ELSEIF ( PF_POLYFIT .EQ. 'REDUCED_GRP' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_RPOLYFIT:'/
     :     /'Action not defined on this path for Group files', STATUS )
      ELSEIF ( PF_POLYFIT .EQ. 'OBJECT') THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_RPOLYFIT:'/
     :     /'Action not defined for OBJECTs only', STATUS )
      ELSE
         CALL MSG_OUT( ' ', 'Polyfitting to OBJ-SKY pairs', STATUS )
      ENDIF

*    Get range of Y values for regions and put in ascending order
      IF ( PF_SAYE1 .LT. PF_SAYS1 ) THEN
         TEMP = PF_SAYE1
         PF_SAYE1 = PF_SAYS1
         PF_SAYS1 = TEMP
      ENDIF
      IF ( PF_SAYE2 .LT. PF_SAYS2 ) THEN
         TEMP = PF_SAYE2
         PF_SAYE2 = PF_SAYS2
         PF_SAYS2 = TEMP
      ENDIF
      IF ( PF_SAYE3 .LT. PF_SAYS3 ) THEN
         TEMP = PF_SAYE3
         PF_SAYE3 = PF_SAYS3
         PF_SAYS3 = TEMP
      ENDIF
      IF ( PF_SAYE4 .LT. PF_SAYS4 ) THEN
         TEMP = PF_SAYE4
         PF_SAYE4 = PF_SAYS4
         PF_SAYS4 = TEMP
      ENDIF

*    Check that regions 1 and 2 don't overlap
      IF ( ( PF_SAYS2 .LE. PF_SAYE1 ) .AND. ( PF_SAYS2 .GT. 0 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_RPOLYFIT:'/
     :     /'Regions 1 and 2 must not overlap', STATUS )
      ENDIF

*    Check that regions 2 and 3 don't overlap
      IF ( ( PF_SAYS3 .LE. PF_SAYE2 ) .AND. ( PF_SAYS3 .GT. 0 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_RPOLYFIT:'/
     :     /'Regions 2 and 3 must not overlap', STATUS )
      ENDIF

*    Check that regions 3 and 4 don't overlap
      IF ( ( PF_SAYS4 .LE. PF_SAYE3 ) .AND. ( PF_SAYS4 .GT. 0 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_RPOLYFIT:'/
     :     /'Regions 3 and 4 must not overlap', STATUS )
      ENDIF

*    Get workspace arrays needed for polynomial fitting
*     DSA was already open so we can do this without fuss
      CALL DSA_GET_WORK_ARRAY( NY, 'DOUBLE', XPTR, XSLT, STATUS )
      CALL DSA_GET_WORK_ARRAY( NY, 'DOUBLE', YPTR, YSLT, STATUS )
      CALL DSA_GET_WORK_ARRAY( NY, 'DOUBLE', WPTR, WSLT, STATUS )
      W1BYTES = 4*NY + 3*(MAX_DEGREE+1)
      CALL DSA_GET_WORK_ARRAY( W1BYTES, 'DOUBLE', W1PTR, W1SLT, STATUS )

*    Process data (NB: We pass the data, variance and quality arrays here
*        rather than their pointers as they entered here - a subtle difference!
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL FIG_POLYFIT( NX, NY, W1BYTES, DATA, VAR, QUAL, %val(XPTR),
     :     %val(YPTR), %val(WPTR), %val(W1PTR), PF_SAYS1, PF_SAYE1,
     :     PF_SAYS2, PF_SAYE2, PF_SAYS3, PF_SAYE3, PF_SAYS4, PF_SAYE4,
     :     PF_DEGREE, PF_NREJECT, PF_WEIGHT )
      ENDIF

*    Exit from subroutine
      END
