*+  SSGET - Writes information about SSDS to environment variable
      SUBROUTINE SSGET( STATUS )
*
*    Description :
*
*     This application returns many different pieces of information
*     depending on the value of the ITEM parameter.
*
*      Item code     Returned type     Description
*
*      NSRC          _INTEGER          Number of sources in SSDS
*      NFILE         _INTEGER          Number of files searched
*      <FIELD>       _DOUBLE           Field value of ISRC'th source
*
*    Parameters :
*
*     INP = HDS data object
*        Object to be interrogated
*     ITEM = CHAR(R)
*        Item of info wanted
*     ISRC = INTEGER(R)
*        Source number
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
*      6 Oct 94 : V1.8-0 Original, adapted from HGET (DJA)
*     24 Nov 94 : V1.8-1 Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                	CHR_LEN
      LOGICAL                	STR_ABBREV
*
*    Local constants :
*
      INTEGER                	TYP_INT, TYP_CHAR, TYP_DBLE, TYP_LOG
        PARAMETER            	( TYP_INT = 1, TYP_CHAR = 2,
     :                            TYP_LOG = 3, TYP_DBLE = 4 )
*
*    Local variables :
*
      CHARACTER*200          	CVALUE           	! Character attribute
      CHARACTER*40           	FIELD            	! Field to get
      CHARACTER*40           	FITEM            	! Field item to get
      CHARACTER*(DAT__SZLOC) 	SLOC              	! Locator to data object
      CHARACTER*40           	ITEM             	! Item to get

      DOUBLE PRECISION       	DVALUE           	! Double value

      INTEGER                	ATYPE            	! Attribute type
      INTEGER                	CLEN             	! Length of character attribute
      INTEGER			CPOS			! Char pointer
      INTEGER			ISRC			! Source number
      INTEGER                	IVALUE           	! Integer attribute
      INTEGER                	NCOMP            	! # of files
      INTEGER			NCPOS			! Char pointer
      INTEGER                	NSRC             	! # of sources

      INTEGER                	PTR              	! Ptr to mapped data
      INTEGER                	TSTAT            	! Temporary status

      LOGICAL                	ECHO             	! Echo to standard output?
      LOGICAL		     	IS_SET			! SSDS is a set?
      LOGICAL                	LVALUE           	! Logical attribute
      LOGICAL                	OK               	! Validity check
*
*    Local data :
*
      CHARACTER*5            TRUTH(-1:1)
        DATA                 TRUTH/'TRUE','FALSE','TRUE'/
*
*    Version id :
*
      CHARACTER*40           VERSION
        PARAMETER            ( VERSION='SSGET Version 1.8-1' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise SSO system
      CALL AST_INIT()
      CALL SSO_INIT()

*    Get input object from user
      CALL SSO_ASSOCI( 'INP', 'READ', SLOC, IS_SET, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get number of sources
      CALL SSO_GETNSRC( SLOC, NSRC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get item code
      CALL USI_GET0C( 'ITEM', ITEM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL CHR_UCASE( ITEM )

*   Item code which does not require source number?
*    Number of sources
      IF ( STR_ABBREV( ITEM, 'NSRC' ) ) THEN
        ATYPE = TYP_INT
        IVALUE = NSRC

*    Number of files searched
      ELSE IF ( STR_ABBREV( ITEM, 'NFILE' ) ) THEN
        CALL SSO_CHKBOOK( SLOC, OK, NCOMP, STATUS )
        ATYPE = TYP_INT
        IVALUE = NCOMP

      ELSE

*      Extract field name
        CPOS = 0
 10     CONTINUE
        NCPOS = INDEX( ITEM(CPOS+1:), '_' )
        IF ( NCPOS .GT. 0 ) THEN
          CPOS = CPOS + NCPOS
          GOTO 10
        END IF
        IF ( CPOS .GT. 2 ) THEN
          FIELD = ITEM(:CPOS-1)
          FITEM = ITEM(CPOS+1:)
        ELSE
          FIELD = ITEM
          CPOS = 0
        END IF
        CALL SSO_CHKFLD( SLOC, FIELD, OK, STATUS )
        IF ( .NOT. OK ) THEN
          CALL MSG_SETC( 'FIELD', FIELD )
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Field ^FIELD not present', STATUS )
          GOTO 99
        END IF

*      Plain field name?
        IF ( CPOS .EQ. 0 ) THEN

*        Get source number
          CALL USI_GET0I( 'ISRC', ISRC, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

          CALL SSO_MAPFLD( SLOC, FIELD, '_DOUBLE', 'READ', PTR, STATUS )
          CALL ARR_ELEM1D( PTR, NSRC, ISRC, DVALUE, STATUS )
          CALL SSO_UNMAPFLD( SLOC, FIELD, STATUS )
          ATYPE = TYP_DBLE

*      Errors
        ELSE IF ( STR_ABBREV( FITEM, 'ERROR' ) ) THEN

*        Get source number
          CALL USI_GET0I( 'ISRC', ISRC, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

          CALL SSO_MAPFLDERR( SLOC, FIELD, '_DOUBLE', 'READ', PTR,
     :                        STATUS )
          CALL ARR_ELEM1D( PTR, NSRC, ISRC, DVALUE, STATUS )
          CALL SSO_UNMAPFLD( SLOC, FIELD, STATUS )
          ATYPE = TYP_DBLE

*      Other kind of field item
        ELSE
          CALL SSO_GETFITEM0C( SLOC, FIELD, FITEM, CVALUE, STATUS )
          CLEN = CHR_LEN(CVALUE)
          ATYPE = TYP_CHAR

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
      ELSE IF ( ATYPE .EQ. TYP_DBLE ) THEN
        CALL USI_PUT0D( 'ATTR', DVALUE, TSTAT )
        IF ( ECHO ) CALL MSG_SETD( 'VAL', DVALUE )
      ELSE IF ( ATYPE .EQ. TYP_CHAR ) THEN
        CLEN = CHR_LEN( CVALUE )
        CALL USI_PUT0C( 'ATTR', CVALUE(:CLEN), TSTAT )
        IF ( ECHO ) CALL MSG_SETC( 'VAL', CVALUE(1:CLEN) )
      END IF

*    Echo the output?
      IF ( ECHO ) CALL MSG_PRNT( '^VAL' )

*    Release input file
      CALL SSO_RELEASE( SLOC, STATUS )

*    Tidy up
 99   CALL SSO_CLOSE( STATUS )
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
