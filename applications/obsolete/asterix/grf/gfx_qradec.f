*+  GFX_QRADEC - inquire if RADEC axes to be plotted
      SUBROUTINE GFX_QRADEC(RADEC,STATUS)

*    Description :
*    Parameters :
*    Method :
*
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
      LOGICAL RADEC
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETL('AXES_RADEC',OK,RADEC,STATUS)
        IF (.NOT.OK) THEN
          RADEC=.FALSE.
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_QRADEC',STATUS)
        ENDIF
      ENDIF
      END
