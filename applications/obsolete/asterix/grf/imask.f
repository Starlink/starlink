*+  IMASK - modify QUALITY mask
      SUBROUTINE IMASK(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*     21 Jan 93: V1.7-0 original
*     16 Sep 94: V1.7-1 updates data min/max (RJV)
*      8 Mar 96: V2.0-0 Use BIT_ routines for bit operations (DJA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB,BIT_ORUB,BIT_NOTUB
*    Local constants :
*    Local variables :
      CHARACTER*8 MSTR
      LOGICAL TOGGLE
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IMASK Version 1.7-1')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')

      ELSE

        IF (I_QOK) THEN

          CALL USI_GET0L('TOGGLE',TOGGLE,STATUS)

          IF (TOGGLE) THEN

            CALL MSG_BLNK()
            CALL STR_BTOC(I_MASK,MSTR,STATUS)
            CALL MSG_PRNT(' Old mask value: '//MSTR)
            IF (BIT_ANDUB(I_MASK,QUAL__IGNORE).EQ.QUAL__GOOD) THEN
              I_MASK=BIT_ORUB(I_MASK,QUAL__IGNORE)
            ELSE
              I_MASK=BIT_ANDUB(I_MASK,BIT_NOTUB(QUAL__IGNORE))
            ENDIF
            CALL STR_BTOC(I_MASK,MSTR,STATUS)
            CALL MSG_PRNT(' New mask value: '//MSTR)

          ELSE

            CALL MSG_BLNK()
            CALL STR_BTOC(I_MASK,MSTR,STATUS)
            CALL MSG_PRNT(' Old mask value: '//MSTR)
            CALL USI_DEF0C('MASK',MSTR,STATUS)
            CALL USI_GET0C('MASK',MSTR,STATUS)
            CALL STR_CTOB(MSTR,I_MASK,STATUS)
            IF (STATUS.EQ.SAI__OK) THEN
              CALL MSG_PRNT(' New mask value: '//MSTR)
            ENDIF

          ENDIF

          CALL IMG_MINMAX(STATUS)

        ELSE
          CALL MSG_PRNT('AST_ERR: no QUALITY present')
        ENDIF

      ENDIF

      CALL AST_ERR(STATUS)

      CALL USI_CLOSE()

      END
