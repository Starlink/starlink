

*+  GFX_DEFLBL- constructs default labels
      SUBROUTINE GFX_DEFLBL(LABEL,UNITS,LBL,STATUS)

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
      CHARACTER*(*) LABEL,UNITS
*    Import-export :
*    Export :
      CHARACTER*(*) LBL
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      INTEGER LBLEN
      INTEGER UNLEN
*    Internal References :
      INTEGER CHR_LEN
*    Local data :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        LBLEN=CHR_LEN(LABEL)
        UNLEN=CHR_LEN(UNITS)
        IF (LBLEN.GT.0.AND.UNLEN.GT.0) THEN
          LBL=LABEL(:LBLEN)//'  ('//UNITS(:UNLEN)//')'
        ELSEIF (LBLEN.EQ.0.AND.UNLEN.GT.0) THEN
          LBL='('//UNITS(:UNLEN)//')'
        ELSEIF (LBLEN.GT.0.AND.UNLEN.EQ.0) THEN
          LBL=LABEL
        ELSE
          LBL=' '
        ENDIF

      ENDIF
      END
