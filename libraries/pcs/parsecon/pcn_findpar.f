*+  PARSECON_FINDPAR - Search parameter-list for named parameter
      SUBROUTINE PARSECON_FINDPAR ( NAME, NAMECODE, STATUS )
*    Description :
*     Search the list of parameters for the given name, and if it is found, 
*     return its index.
*    Invocation :
*     CALL PARSECON_FINDPAR ( NAME; NAMECODE, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*           name of requested parameter
*     NAMECODE=INTEGER (returned)
*           index number of parameter, if found.
*     STATUS=INTEGER
*    Method :
*     The list of parameter names for the current program is searched 
*     sequentially
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     02.10.1984:  Original (REVAD::BDK)
*     23.08.1985:  start search from PROGADD(1,PROGNUM) (REVAD::BDK)
*     24.02.1991:  Report errors (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'

*    Import :
      CHARACTER*(*) NAME                 ! name to be found

*    Export :
      INTEGER NAMECODE                   ! index of NAME if found

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*    Local variables :
      LOGICAL FOUND

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( MONOLITH ) THEN
         NAMECODE = PROGADD(1,ACTPTR) - 1
      ELSE
         NAMECODE = 0
      ENDIF

      FOUND = .FALSE.

      DO WHILE ( ( .NOT. FOUND ) .AND. ( NAMECODE .LT. PARPTR ) )

         NAMECODE = NAMECODE + 1
         IF ( NAME .EQ. PARNAMES(NAMECODE) ) FOUND = .TRUE.

      ENDDO

      IF ( .NOT. FOUND ) THEN
         STATUS = PARSE__NOPAR
         CALL EMS_SETC ( 'NAME', NAME )
         CALL EMS_REP ( 'PCN_FINDPAR1',
     :   'PARSECON: Parameter ^NAME not defined', STATUS )
         NAMECODE = 0
      ENDIF

      END
