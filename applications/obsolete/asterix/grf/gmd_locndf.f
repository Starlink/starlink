*+  GMD_LOCNDF - locates NDF within multiple dataset
      SUBROUTINE GMD_LOCNDF(LOC,NUM,GLOC,STATUS)

*    Description :
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
      CHARACTER*(DAT__SZLOC) LOC           ! locator to dataset
      INTEGER NUM			   ! NDF number
*    Import-export :
*    Export :
      CHARACTER*(DAT__SZLOC) GLOC	   ! graph locator
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) MLOC
      INTEGER NNDF
*-


      IF (STATUS.EQ.SAI__OK) THEN
        CALL DAT_FIND(LOC,'NDF',MLOC,STATUS)
        CALL DAT_SIZE(MLOC,NNDF,STATUS)
        IF (NUM.GT.NNDF.OR.NUM.LT.1) THEN
          CALL MSG_SETI('NUM',NUM)
          CALL MSG_PRNT('AST_ERR: invalid NDF number - ^NUM')
          CALL MSG_SETI('NUM',NNDF)
          CALL MSG_PRNT('         dataset contains ^NUM NDFs')
          STATUS=SAI__ERROR
          GLOC=' '
        ELSE
          CALL DAT_CELL(MLOC,1,NUM,GLOC,STATUS)
          CALL DAT_ANNUL(MLOC,STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_LOCNDF',STATUS)
        ENDIF

      ENDIF
      END
