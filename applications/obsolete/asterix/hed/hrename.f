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
      PARAMETER (VERSION='HRENAME Version 1.0-0')
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(DAT__SZNAM) NEW
*-
      CALL MSG_PRNT(VERSION)

      CALL DAT_ASSOC('INP','UPDATE',LOC,STATUS)

      CALL PAR_GET0C('TO',NEW,STATUS)

      CALL DAT_RENAM(LOC,NEW,STATUS)

      CALL DAT_ANNUL(LOC,STATUS)

      CALL AST_ERR(STATUS)

      END

