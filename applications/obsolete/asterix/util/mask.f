*+  MASK - sets QUALITY mask
      SUBROUTINE MASK(STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) OLOC
      CHARACTER*8 MSTR
      BYTE MASKVAL
      LOGICAL PRIM

*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'MASK Version 1.8-0')
*-
      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

      CALL USI_ASSOCI('INP','UPDATE',OLOC,PRIM,STATUS)

      IF (.NOT.PRIM) THEN
        CALL BDA_GETMASK(OLOC,MASKVAL,STATUS)
        CALL STR_BTOC(MASKVAL,MSTR,STATUS)
        CALL USI_DEF0C('MASK',MSTR,STATUS)
        CALL USI_GET0C('MASK',MSTR,STATUS)
        CALL STR_CTOB(MSTR,MASKVAL,STATUS)
        CALL BDA_PUTMASK(OLOC,MASKVAL,STATUS)
      ELSE
        STATUS=SAI__ERROR
        CALL ERR_REP(' ','Input data primitive', STATUS )
      ENDIF

      CALL AST_CLOSE()

      END
