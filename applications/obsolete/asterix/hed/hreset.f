*+  HRESET - changes status of an HDS data object to undefined
      SUBROUTINE HRESET(STATUS)

*    Description :

*    Parameters :

*     INP  = CHAR  - the name of the object
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='HRESET Version 2.2-0')
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC

      INTEGER ID
*-
      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

      CALL USI_ASSOC('INP','*','UPDATE',ID,STATUS)
      CALL ADI1_GETLOC(ID,LOC,STATUS)

      CALL DAT_RESET(LOC,STATUS)

      CALL AST_CLOSE()
      CALL AST_ERR(STATUS)

      END
