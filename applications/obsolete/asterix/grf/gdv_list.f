*+  GDV_LIST - list device names known to GNS
      SUBROUTINE GDV_LIST(STATUS)
*    Description :
*    Deficiencies :
*    Authors :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER  I, LD
      CHARACTER*72  DESCR
      CHARACTER*15 NAME
      LOGICAL MORE
      LOGICAL GDV_LIST_FILT
      EXTERNAL GDV_LIST_FILT

*    Local data :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        I = 0
        MORE=.TRUE.
        DO WHILE (MORE)

*     Get the next type in the list
          CALL GNS_GWNG(GDV_LIST_FILT,I,NAME,DESCR,LD,STATUS)
          CALL MSG_PRNT(NAME//DESCR(:LD))
          MORE=(I.NE.0.AND.STATUS.EQ.SAI__OK)
        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDV_LIST',STATUS)
        ENDIF

      ENDIF

      END

      LOGICAL FUNCTION GDV_LIST_FILT(I)

      GDV_LIST_FILT=.TRUE.

      END
