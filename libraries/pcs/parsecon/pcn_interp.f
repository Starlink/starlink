*+  PARSECON_INTERP - Obsolete routine only needed for transfer vector
      SUBROUTINE PARSECON_INTERP ( STRING, TYPE, RVAL, CVAL, 
     :  LVAL, STATUS )
*    Description :
*     Given a character string, the syntax of the string is checked, 
*     and if possible it is converted to the corresponding primitive 
*     data type and the value returned in the corresponding variable. 
*    Invocation :
*     CALL PARSECON_INTERP ( STRING, TYPE, RVAL, CVAL, 
*    :  LVAL, STATUS )
*    Parameters :
*     STRING=CHARACTER*(*) (given)
*           character string to be converted
*     TYPE=INTEGER (returned)
*           type identified, one of
*                SUBPAR__REAL
*                SUBPAR__CHAR
*                SUBPAR__LOGICAL
*     RVAL=REAL (returned)
*           variable to hold converted value
*     CVAL=CHARACTER*(*) (returned)
*           variable to hold converted value
*     LVAL=LOGICAL (returned)
*           variable to hold converted value
*     STATUS=INTEGER
*    Method :
*     The type is deduced from the syntax of the given string.
*     Any valid type conversions are performed, and the value is stored
*     in the corresponding variable.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     27.05.1985:  Original (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
!      INCLUDE 'PARSECON_ERR'
!      INCLUDE 'PARSECON_PAR'
!      INCLUDE 'SUBPAR_PAR'
!      INCLUDE '($SSDEF)'

*    Import :
      CHARACTER*(*) STRING

*    Export :
      INTEGER TYPE                             ! code for type of STRING
                                               ! deduced from its syntax

      REAL RVAL                                ! value if real

      CHARACTER*(*) CVAL                       ! value if character

      LOGICAL LVAL                             ! value if logical

*    Status :
      INTEGER STATUS

*    External references :
!      INTEGER LIB$CVT_DX_DX
!      EXTERNAL LIB$CVT_DX_DX
      
*    Local variables :
!      INTEGER ISTAT                            ! status return from 
                                               ! type-conversion utility

*-

      IF ( STATUS .NE. SAI__OK ) RETURN
!
!
!*
!*   Determine what type information can be deduced from the syntax of 
!*   STRING, and do any syntax-dependent string processing. - eg remove 
!*   single quotes from character constant.
!*
!      CALL PARSECON_DECVAL ( STRING, CVAL, TYPE, STATUS )
!*
!*   Convert the string to the variable of corresponding type.
!*
!      IF ( TYPE .EQ. PARSE__CHAR ) THEN
!
!         CONTINUE
!
!      ELSE IF ( TYPE .EQ. PARSE__NUMBER ) THEN
!
!         ISTAT = LIB$CVT_DX_DX ( %DESCR(CVAL), 
!     :     %DESCR(RVAL) )
!
!         IF ( ISTAT .NE. SS$_NORMAL ) THEN
!            STATUS = PARSE__IVCONV
!         ENDIF
!
!      ELSE IF ( TYPE .EQ. PARSE__LOGTRUE ) THEN
!
!         LVAL = .TRUE.
!
!      ELSE IF ( TYPE .EQ. PARSE__LOGFALSE ) THEN
!
!         LVAL = .FALSE.
!
!      ELSE
!
!         STATUS = PARSE__VALSYN
!
!      ENDIF
!
      END
