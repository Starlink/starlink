*+  PARSECON_FINDACT - Search action-list for named action.
      SUBROUTINE PARSECON_FINDACT ( NAME, NAMECODE, STATUS )
*    Description :
*     Search the list of actions for the given name, and if it is found, 
*     return its index.
*    Invocation :
*     CALL PARSECON_FINDACT ( NAME; NAMECODE, STATUS )
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*     NAME=CHARACTER*(*) (given)
*           name of requested action
*     NAMECODE=INTEGER (returned)
*           index number of action, if found.
*     STATUS=INTEGER
*    Method :
*     The list of action names is searched sequentially
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     02.10.1984:  Original (REVAD::BDK)
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

      NAMECODE = 0
      FOUND = .FALSE.

      DO WHILE ( ( .NOT. FOUND ) .AND. ( NAMECODE .LT. ACTPTR ) )

         NAMECODE = NAMECODE + 1
         IF ( NAME .EQ. ACTNAMES(NAMECODE) ) FOUND = .TRUE.

      ENDDO

      IF ( .NOT. FOUND ) THEN
         STATUS = PARSE__NOACT
         CALL EMS_SETC ( 'NAME', NAME )
         CALL EMS_REP ( 'PCN_FINDACT1',
     :   'PARSECON: Action ^NAME not defined', STATUS )
         NAMECODE = 0
      ENDIF

      END
