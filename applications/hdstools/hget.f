      SUBROUTINE HGET( STATUS )
*+
* Name:
*    HGET

* Purpose:
*    Return information about an object.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    hget inp item [attr] [index] [echo=] [version=]

* ADAM Parameters:
*     INP = UNIV (Read)
*        Object to be interrogated. <GLOBAL.HDSOBJ>
*     ITEM = _CHAR (Read)
*        Item wanted (see Description). Case is not significant and the
*        value may be abbreviated. <VALUE>
*     ATTR = UNIV (Write)
*        Attribute value.
*     INDEX = _INTEGER (Write)
*        The index position of the max or min value (treating arrays as
*        vectors).
*     ECHO = _LOGICAL (Read)
*        Echo attribute value to standard output stream? [TRUE]
*     VERSION = _LOGICAL (Read)
*        Whether application version number is to be output. If the MSG_FILTER
*        environment variable is set to 1, output will not occur anyway.
*        [FALSE]

* Description:
*    This application returns many different pieces of information
*    depending on the value of the ITEM parameter.
*
*    !bt3
*      Item code   !- Returned type  !- Description !n
*      !n
*      PRIMITIVE   !- _LOGICAL       !- True if Object primitive !n
*      STRUCTURED  !- _LOGICAL       !- True if Object structured !n
*      NDIM        !- _INTEGER       !- Dimensionality !n
*      DIMS        !- _CHAR          !- Dimensions separated by commas !n
*      NELM        !- _INTEGER       !- Total number of elements !n
*      TYPE        !- _CHAR          !- Object type !n
*      VALUE       !- Object type    !- Object value (primitive scalar only) !n
*      MIN, MAX    !- _REAL          !- Min,max values in numeric array
*    !et
*
*    The 'internal' parameter ATTR is set to the requested value and, if
*    ECHO is TRUE, the value is displayed on the user's terminal.
*
*    For items MIN and MAX, internal parameter INDEX is also set (but not
*    displayed).
*
*    The application is particularly useful from ICL, where the values of
*    the ATTR and INDEX parameters can be returned into ICL variables and
*    thus used to control applications. Note however that a character value
*    cannot be returned into a variable that has already been defined as
*    a numeric type.

* Notes:
*    Internal parameters (ATTR and INDEX) are not saved in the task's
*    parameter file. Their values can be written to HDS objects by
*    specifying the name of an existing object of a suitable type on the
*    command line. The object name must be preceded by @ for the ATTR
*    parameter but this is not necessary for INDEX (see Example 2).

* Examples:
*    % hget 'numvec(2)' value
*       Displays the value of the second element of object numvec
*
*    % hget file.data_array.data max attr=@info.max noecho
*       Writes the maximum value of the DATA component of the DATA_ARRAY
*       component of file into the MAX component of file info. The value
*       will not be displayed.
*
*    ICL> hget numvec min (minval) (index) echo=f
*    ICL> =index
*       Sets the ICL variable minval to the minimum value in vector numvec,
*       and variable index to the position of the minimum value within
*       numvec. The minimum value will not be displayed but the second
*       ICL command will display the index.

* Method :

* Deficiencies :
*    Only scalar values can be obtained.

* Bugs :

* Authors :
*    DJA: David J. Allan (Birmingham University)
*    RB: Richard Beard (ROSAT, University of Birmingham)
*    AJC: Alan J. Chipperfield (Starlink, RAL)
*    TIMJ: Tim Jenness (JAC, Hawaii)
*    DSB: David Berry (EAO)

*    History :
*     21-APR-1991 (DJA):
*        V1.4-0  Original
*     14-OCT-1992 (RDS):
*        V1.4-1  Outputs index of min or max value
*      6-JUL-1993 (DJA):
*        V1.7-0  Added ECHO keyword
*     24-NOV-1994 (DJA):
*        V1.8-0  Now use USI for user interface
*      9-MAY-1997 (RB):
*        V2.1-0  Get VALUE as correct type and convert to string
*      6-SEP-2001 (AJC):
*        V3.0-0 Remove Asterix stuff
*     18-JUL-2007 (TIMJ):
*        Add CNF_PVAL for 64-bit
*      4-SEP-2019 (TIMJ):
*        Cancel the INP parameter to avoid HDS locator leak reports.
*-
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'MSG_PAR'
      INCLUDE 'CNF_PAR'

*    Status :
      INTEGER STATUS

*    Functions :
      INTEGER                CHR_LEN
      LOGICAL                STR_ABBREV
      LOGICAL                HDX_NUMERIC

*    Local constants :
      INTEGER                TYP_INT, TYP_CHAR, TYP_LOG,
     :                       TYP_REAL, TYP_DP
        PARAMETER            ( TYP_INT = 1, TYP_CHAR = 2, TYP_LOG = 3,
     :                         TYP_REAL = 4, TYP_DP = 5 )

      CHARACTER*40           VERSION
        PARAMETER            ( VERSION='HGET Version 3.0-0' )

*    Local variables :
      CHARACTER*200          CVALUE           ! Character attribute
      CHARACTER*(DAT__SZLOC) LOC              ! Locator to data object
      CHARACTER*40           ITEM             ! Item to get
      CHARACTER*20           TYPSTR

      DOUBLE PRECISION       DVALUE           ! Double attribute

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
      INTEGER                FC
      INTEGER                ISTAT            ! Dummy STATUS

      LOGICAL                ECHO             ! Echo to standard output?
      LOGICAL                LVALUE           ! Logical attribute
      LOGICAL                OK               ! Validity check
      LOGICAL                PRIM             ! Input primitive?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*    Version id
      CALL PAR_GET0L( 'VERSION', OK, STATUS )
      IF ( OK ) THEN
        CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )
      END IF

*    Get locator to data object and validate it
      CALL DAT_ASSOC( 'INP', 'READ', LOC, STATUS )
      CALL DAT_VALID( LOC, OK, STATUS )

*    If OK so far carry on
      IF ( OK .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*      Get item code
        CALL PAR_GET0C( 'ITEM', ITEM, STATUS )
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
            ISTAT = SAI__OK
            IF ( I .EQ. 1 ) THEN
              CALL MSG_LOAD( ' ', '^DIM', CVALUE, CLEN, STATUS )
            ELSE
              CALL MSG_SETC( 'BIT', CVALUE(:CLEN) )
              CALL MSG_LOAD( ' ', '^BIT,^DIM', CVALUE, CLEN, ISTAT )
            END IF
          END DO

*      Number of elements
        ELSE IF ( STR_ABBREV( ITEM, 'NELM' ) ) THEN
          ATYPE = TYP_INT
          IVALUE = NELM

*      Object value
*      Convert from correct type to preserve precision (RB)
        ELSE IF ( STR_ABBREV( ITEM, 'VALUE' ) ) THEN
          CALL DAT_TYPE( LOC, TYPSTR, STATUS )
          FC = 1
          IF ( TYPSTR(1:1) .EQ. '_' ) FC = 2
          IF ( STR_ABBREV( 'CHAR', TYPSTR(FC:) ) ) THEN
            ATYPE = TYP_CHAR
            CALL DAT_GET0C( LOC, CVALUE, STATUS )
          ELSE IF ( STR_ABBREV( 'DOUB', TYPSTR(FC:) ) ) THEN
            ATYPE = TYP_DP
            CALL DAT_GET0D( LOC, DVALUE, STATUS )
          ELSE IF ( STR_ABBREV( 'REAL', TYPSTR(FC:) ) ) THEN
            ATYPE = TYP_REAL
            CALL DAT_GET0R( LOC, RVALUE, STATUS )
          ELSE IF ( STR_ABBREV( 'INTE', TYPSTR(FC:) ) ) THEN
            ATYPE = TYP_INT
            CALL DAT_GET0I( LOC, IVALUE, STATUS )
          ELSE IF ( STR_ABBREV( 'LOGI', TYPSTR(FC:) ) ) THEN
            ATYPE = TYP_LOG
            CALL DAT_GET0L( LOC, LVALUE, STATUS )
          ELSE
            ATYPE = TYP_CHAR
            CALL DAT_GET0C( LOC, CVALUE, STATUS )
          END IF

*      Object type
        ELSE IF ( STR_ABBREV( ITEM, 'TYPE' ) ) THEN
          ATYPE = TYP_CHAR
          CALL DAT_TYPE( LOC, CVALUE, STATUS )

*      Min or max values
        ELSE IF ( ( ITEM(1:3) .EQ. 'MIN' ) .OR.
     :               ( ITEM(1:3) .EQ. 'MAX' ) ) THEN
          IF(HDX_NUMERIC(LOC)) THEN
             ATYPE = TYP_REAL
             CALL DAT_MAPV( LOC, '_REAL', 'READ', PTR, NELM, STATUS )
*
             IF ( STATUS .EQ. SAI__OK ) THEN

*          Get the min and max values and pixel indices
               CALL ARR_PRANG1R( NELM, %VAL(CNF_PVAL(PTR)),
     :                           INDMIN, MINVAL,
     :                        INDMAX, MAXVAL, STATUS )

*
               TSTAT = SAI__OK
               IF ( ITEM(1:3) .EQ. 'MIN' ) THEN
                 RVALUE = MINVAL
                 CALL PAR_PUT0I( 'INDEX', INDMIN, TSTAT )
               ELSE
                 RVALUE = MAXVAL
                 CALL PAR_PUT0I( 'INDEX', INDMAX, TSTAT )
               END IF

               IF ( TSTAT .NE. SAI__OK ) THEN
                  STATUS = TSTAT
                  CALL ERR_ANNUL( TSTAT )
                  CALL ERR_REP( ' ',
     :              'Failed to ''put'' parameter INDEX', STATUS )
      ENDIF

             END IF
             CALL DAT_UNMAP( LOC, STATUS )

          ELSE
             STATUS = SAI__ERROR
             CALL MSG_SETC('ITEM',ITEM)
             CALL ERR_REP(' ','Can''t find ^ITEM of non-numeric array',
     &         STATUS)
          ENDIF

        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'ITEM', ITEM )
          CALL ERR_REP( ' ', 'Illegal item code /^ITEM/', STATUS )
          GOTO 99
        END IF

      END IF

*  Annul the locator.
      CALL DAT_CANCL( 'INP', STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
        ATYPE = TYP_CHAR
        CVALUE = 'INVALID'
      END IF

*    Echo to output?
      CALL PAR_GET0L( 'ECHO', ECHO, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Write attribute
      TSTAT = SAI__OK
      IF ( ATYPE .EQ. TYP_INT ) THEN
        CALL PAR_PUT0I( 'ATTR', IVALUE, TSTAT )
        IF ( ECHO ) CALL MSG_SETI( 'VAL', IVALUE )
      ELSE IF ( ATYPE .EQ. TYP_LOG ) THEN
        IF ( LVALUE ) THEN
          CVALUE = 'TRUE'
        ELSE
          CVALUE = 'FALSE'
        END IF
        CALL PAR_PUT0C( 'ATTR', CVALUE(:5), TSTAT )
        IF ( ECHO ) CALL MSG_SETC( 'VAL', CVALUE(1:5) )
      ELSE IF ( ATYPE .EQ. TYP_REAL ) THEN
        CALL PAR_PUT0R( 'ATTR', RVALUE, TSTAT )
        IF ( ECHO ) CALL MSG_SETR( 'VAL', RVALUE )
      ELSE IF ( ATYPE .EQ. TYP_DP ) THEN
        CALL PAR_PUT0D( 'ATTR', DVALUE, TSTAT )
        IF ( ECHO ) CALL MSG_SETD( 'VAL', DVALUE )
      ELSE IF ( ATYPE .EQ. TYP_CHAR ) THEN
        CLEN = CHR_LEN( CVALUE )
        CALL PAR_PUT0C( 'ATTR', CVALUE(:CLEN), TSTAT )
        IF ( ECHO ) CALL MSG_SETC( 'VAL', CVALUE(1:CLEN) )
      END IF

*    Echo the output?
      IF ( ECHO ) CALL MSG_OUTIF( MSG__QUIET, ' ', '^VAL', STATUS )

      IF ( TSTAT .NE. SAI__OK ) THEN
         STATUS = TSTAT
         CALL ERR_ANNUL( TSTAT )
         CALL ERR_REP( ' ',
     :     'Failed to ''put'' parameter ATTR', STATUS )
      ENDIF

 99   CONTINUE

      END
