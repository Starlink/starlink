
*+  GFX_QWINDOW - inquire limits of plotting window
      SUBROUTINE GFX_QWINDOW(X1,X2,Y1,Y2,STATUS)

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
*    Import-export :
      REAL X1,X2,Y1,Y2			! plot window in world coords
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      REAL LO,HI
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  if limits set then overide defaults
        CALL GCB_GETR('XAXIS_LO',OK,LO,STATUS)
        IF (OK) THEN
          X1=LO
        ENDIF
        CALL GCB_GETR('XAXIS_HI',OK,HI,STATUS)
        IF (OK) THEN
          X2=HI
        ENDIF
        CALL GCB_GETR('YAXIS_LO',OK,LO,STATUS)
        IF (OK) THEN
          Y1=LO
        ENDIF
        CALL GCB_GETR('YAXIS_HI',OK,HI,STATUS)
        IF (OK) THEN
          Y2=HI
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_QWINDOW',STATUS)
        ENDIF
      ENDIF
      END
