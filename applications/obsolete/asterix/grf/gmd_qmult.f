*+  GMD_QMULT - determines if dataset is multiple type
      SUBROUTINE GMD_QMULT(LOC,MULT,STATUS)

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
      CHARACTER*(DAT__SZLOC) LOC
*    Import-export :
*    Export :
      LOGICAL MULT
*    Status :
      INTEGER STATUS
*    Global variables :
*    Local Constants :
*    Local variables :
      LOGICAL PRIM
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  check if primitive object
        CALL DAT_PRIM(LOC,PRIM,STATUS)
        IF (PRIM) THEN
          MULT=.FALSE.
        ELSE
*  otherwise look for NDF component
          CALL DAT_THERE(LOC,'NDF',MULT,STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_QMULT',STATUS)
        ENDIF

      ENDIF
      END
