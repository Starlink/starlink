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
*       12/6/89: now checks for primitive input  (RJV)
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
      PARAMETER (VERSION='HRETYPE Version 1.0-1')
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(DAT__SZTYP) TYPE
      LOGICAL STRUC
*-
      CALL MSG_PRNT(VERSION)

      CALL DAT_ASSOC('INP','UPDATE',LOC,STATUS)

      IF (STATUS.EQ.SAI__OK) THEN

        CALL DAT_STRUC(LOC,STRUC,STATUS)

        IF (STRUC) THEN
          CALL PAR_GET0C('TO',TYPE,STATUS)
          CALL DAT_RETYP(LOC,TYPE,STATUS)
        ELSE
          CALL MSG_PRNT('! cannot retype primitive object')
        ENDIF

        CALL DAT_ANNUL(LOC,STATUS)

      ENDIF

      CALL AST_ERR(STATUS)

      END

