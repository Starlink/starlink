*+  HRENAME - renames an HDS data object
      SUBROUTINE HRENAME(STATUS)

*    Description :

*    Parameters :

*     INP  = CHAR  - the name of the object
*     TO   = CHAR - new name
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
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
      PARAMETER (VERSION='HRENAME Version 1.8-0')
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(DAT__SZNAM) NEW
*-
      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

      CALL USI_DASSOC('INP','UPDATE',LOC,STATUS)

      CALL USI_GET0C('TO',NEW,STATUS)

      CALL DAT_RENAM(LOC,NEW,STATUS)

      CALL DAT_ANNUL(LOC,STATUS)

      CALL AST_CLOSE()
      CALL AST_ERR(STATUS)

      END

