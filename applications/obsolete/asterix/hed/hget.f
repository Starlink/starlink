*+  HGET - Writes information about object to environment variable
      SUBROUTINE HGET( STATUS )
*
*    Description :
*
*     This application returns many different pieces of information
*     depending on the value of the ITEM parameter.
*
*      Item code     Returned type     Description
*
*      PRIMITIVE     _LOGICAL          Object primitive
*      STRUCTURED    _LOGICAL          Object structured
*      NDIM          _INTEGER          Dimensionality
*      DIMS          _CHAR             Dimensions separated by commas
*      NELM          _INTEGER          Total number of elements
*      TYPE          _CHAR             Object type
*      VALUE         _CHAR             Object value
*      MIN, MAX      _REAL             Min,max values in array
*      INDEX         _INTEGER          The index position of the max
*                                      or min value
*
*    Parameters :
*
*     INP = HDS data object
*        Object to be interrogated
*     ITEM = CHAR(R)
*        Item of info wanted
*     ATTR = UNIV(W)
*        Attribute value
*     ECHO = LOGICAL(R)
*        Echo attribute value to standard output stream?
*
*    Method :
*
*     IF object valid THEN
*       Get attribute value and return it
*     ELSE
*       ATTR = INVALID
*     ENDIF
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 Apr 91 : V1.4-0  Original (DJA)
*     14 Oct 92 : V1.4-1  Outputs index of min or max value (RDS)
*      6 Jul 93 : V1.7-0  Added ECHO keyword (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                CHR_LEN
      LOGICAL                STR_ABBREV
*
*    Local constants :
*
      INTEGER                TYP_INT, TYP_CHAR, TYP_REAL, TYP_LOG
        PARAMETER            ( TYP_INT = 1, TYP_CHAR = 2,
     :                         TYP_LOG = 3, TYP_REAL = 4 )
*
*    Local variables :
*
      CHARACTER*200          CVALUE           ! Character attribute
      CHARACTER*(DAT__SZLOC) LOC              ! Locator to data object
      CHARACTER*40           ITEM             ! Item to get

      REAL                   RVALUE           ! Real attribute
      REAL                   MINVAL           ! Value of minimum
      REAL                   MAXVAL           ! Value of maximum

      INTEGER                ATYPE            ! Attribute type
      INTEGER                CLEN             ! Length of character attribute
      INTEGER                DIMS(DAT__MXDIM) ! Dimensions
      INTEGER                I                ! Loop over dimensions
      INTEGER                INDMAX           ! Index of maximum value
      INTEGER                INDMIN           ! Index of minimum value
      INTEGER                IVALUE           ! Integer attribute
      INTEGER                NDIM             ! Dimensionality
      INTEGER                NELM             ! # of elements
      INTEGER                PTR              ! Ptr to mapped data
      INTEGER                TSTAT            ! Temporary status

      LOGICAL                ECHO             ! Echo to standard output?
      LOGICAL                LVALUE           ! Logical attribute
      LOGICAL                OK               ! Validity check
      LOGICAL                PRIM             ! Input primitive?
*
*    Local data :
*
      CHARACTER*5            TRUTH(-1:1)
        DATA                 TRUTH/'TRUE','FALSE','TRUE'/
*
*    Version id :
*
      CHARACTER*40           VERSION
        PARAMETER            ( VERSION='HGET Version 1.7-0' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version id
c     CALL MSG_PRNT( VERSION )

*    Start ASTERIX
      CALL AST_INIT()

*    Get locator to data object and validate it
      CALL USI_DASSOC( 'INP', 'READ', LOC, STATUS )
      CALL DAT_VALID( LOC, OK, STATUS )

*    If OK so far carry on
      IF ( OK .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*      Get item code
        CALL USI_GET0C( 'ITEM', ITEM, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        CALL CHR_UCASE( ITEM )

*      Standard tests
        CALL DAT_PRIM( LOC, PRIM, STATUS )
        CALL DAT_SHAPE( LOC, DAT__MXDIM, DIMS, NDIM, STATUS )
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*      Test different codes

*      Object primitive?
        IF ( STR_ABBREV( ITEM, 'PRIMITIVE' ) ) THEN
          ATYPE = TYP_LOG
          LVALUE = PRIM

*      Object structured?
        ELSE IF ( STR_ABBREV( ITEM, 'STRUCTURED' ) ) THEN
          ATYPE = TYP_LOG
          LVALUE = ( .NOT. PRIM )

*      Dimensionality
        ELSE IF ( STR_ABBREV( ITEM, 'NDIM' ) ) THEN
          ATYPE = TYP_INT
          IVALUE = NDIM

*      Dimensions
        ELSE IF ( STR_ABBREV( ITEM, 'DIMS' ) ) THEN
          ATYPE = TYP_CHAR
          DO I = 1, NDIM
            CALL MSG_SETI( 'DIM', DIMS(I) )
            IF ( I .EQ. 1 ) THEN
              CALL MSG_MAKE( '^DIM', CVALUE, CLEN )
            ELSE
              CALL MSG_SETC( 'BIT', CVALUE(:CLEN) )
              CALL MSG_MAKE( '^BIT,^DIM', CVALUE, CLEN )
            END IF
          END DO

*      Number of elements
        ELSE IF ( STR_ABBREV( ITEM, 'NELM' ) ) THEN
          ATYPE = TYP_INT
          IVALUE = NELM

*      Object value
        ELSE IF ( STR_ABBREV( ITEM, 'VALUE' ) ) THEN
          ATYPE = TYP_CHAR
          CALL DAT_GET0C( LOC, CVALUE, STATUS )

*      Object type
        ELSE IF ( STR_ABBREV( ITEM, 'TYPE' ) ) THEN
          ATYPE = TYP_CHAR
          CALL DAT_TYPE( LOC, CVALUE, STATUS )

*      Min or max values
        ELSE IF ( ( ITEM(1:3) .EQ. 'MIN' ) .OR.
     :               ( ITEM(1:3) .EQ. 'MAX' ) ) THEN
          ATYPE = TYP_REAL
          CALL DAT_MAPV( LOC, '_REAL', 'READ', PTR, NELM, STATUS )
*
          IF ( STATUS .EQ. SAI__OK ) THEN

*          Get the min and max values and pixel indices
            CALL ARR_MINMAXPXR( NELM, %VAL(PTR), MINVAL, MAXVAL,
     :                          INDMIN, INDMAX )
*
            IF ( ITEM(1:3) .EQ. 'MIN' ) THEN
              RVALUE = MINVAL
              CALL PAR_PUT0I( 'INDEX', INDMIN, TSTAT )
            ELSE
              RVALUE = MAXVAL
              CALL PAR_PUT0I( 'INDEX', INDMAX, TSTAT )
            END IF
          END IF
          CALL DAT_UNMAP( LOC, STATUS )

        ELSE
          CALL MSG_SETC( 'ITEM', ITEM )
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Illegal item code /^ITEM/', STATUS )
          GOTO 99
        END IF

      END IF

      IF ( STATUS .NE. SAI__OK ) THEN
        ATYPE = TYP_CHAR
        CVALUE = 'INVALID'
      END IF

*    Echo to output?
      CALL USI_GET0L( 'ECHO', ECHO, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Write attribute
      TSTAT = SAI__OK
      IF ( ATYPE .EQ. TYP_INT ) THEN
        CALL USI_PUT0I( 'ATTR', IVALUE, TSTAT )
        IF ( ECHO ) CALL MSG_SETI( 'VAL', IVALUE )
      ELSE IF ( ATYPE .EQ. TYP_LOG ) THEN
        CVALUE = TRUTH(LVALUE)
        CALL USI_PUT0C( 'ATTR', CVALUE(:5), TSTAT )
        IF ( ECHO ) CALL MSG_SETC( 'VAL', CVALUE(1:5) )
      ELSE IF ( ATYPE .EQ. TYP_REAL ) THEN
        CALL USI_PUT0R( 'ATTR', RVALUE, TSTAT )
        IF ( ECHO ) CALL MSG_SETR( 'VAL', RVALUE )
      ELSE IF ( ATYPE .EQ. TYP_CHAR ) THEN
        CLEN = CHR_LEN( CVALUE )
        CALL USI_PUT0C( 'ATTR', CVALUE(:CLEN), TSTAT )
        IF ( ECHO ) CALL MSG_SETC( 'VAL', CVALUE(1:CLEN) )
      END IF

*    Echo the output?
      IF ( ECHO ) CALL MSG_PRNT( '^VAL' )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
