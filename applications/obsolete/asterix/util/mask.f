*+  MASK - sets QUALITY mask
      SUBROUTINE MASK(STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     15 Jan 95 : V1.8-1 New data interfaces (DJA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*8 		MSTR

      INTEGER			OFID			! File identifier

      BYTE 			MASKVAL

      LOGICAL 			PRIM

*    Version :
      CHARACTER*30 		VERSION
        PARAMETER 		( VERSION = 'MASK Version 1.8-1' )
*-
      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

      CALL USI_TASSOCI('INP','*','UPDATE',OFID,STATUS)
      CALL USI_PRIM( OFID, PRIM, STATUS )

      IF (.NOT.PRIM) THEN
        CALL BDI_GETMASK(OFID,MASKVAL,STATUS)
        CALL STR_BTOC(MASKVAL,MSTR,STATUS)
        CALL USI_DEF0C('MASK',MSTR,STATUS)
        CALL USI_GET0C('MASK',MSTR,STATUS)
        CALL STR_CTOB(MSTR,MASKVAL,STATUS)
        CALL BDI_PUTMASK(OFID,MASKVAL,STATUS)
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP(' ','Input data primitive', STATUS )
      END IF

      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
