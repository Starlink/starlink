*+  HRETYPE - changes type of an HDS data object
      SUBROUTINE HRETYPE(STATUS)

*    Description :

*    Parameters :

*     INP  = CHAR  - the name of the object
*     TO   = CHAR - new type
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*
*     12 Jun 89 : V1.0-1 Now checks for primitive input  (RJV)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='HRETYPE Version 1.8-0')
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(DAT__SZTYP) TYPE
      LOGICAL STRUC
*-
      CALL MSG_PRNT(VERSION)

*    Start ASTERIX
      CALL AST_INIT()

      CALL USI_DASSOC('INP','UPDATE',LOC,STATUS)

      IF (STATUS.EQ.SAI__OK) THEN

        CALL DAT_STRUC(LOC,STRUC,STATUS)

        IF (STRUC) THEN
          CALL USI_GET0C('TO',TYPE,STATUS)
          CALL DAT_RETYP(LOC,TYPE,STATUS)
        ELSE
          CALL ERR_REP( ' ', 'Cannot retype primitive object', STATUS )
        ENDIF

        CALL DAT_ANNUL(LOC,STATUS)

      ENDIF

      CALL AST_CLOSE()
      CALL AST_ERR(STATUS)

      END

