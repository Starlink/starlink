*+  GMD_ADDNDF - appends further NDF to multiple dataset
      SUBROUTINE GMD_ADDNDF(LOC,GLOC,STATUS)

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
      CHARACTER*(DAT__SZLOC) LOC	! multiple dataset
      CHARACTER*(DAT__SZLOC) GLOC	! NDF
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) MLOC,MNLOC
      INTEGER NNDF
      LOGICAL THERE
*-

        IF (STATUS.EQ.SAI__OK) THEN

          CALL DAT_THERE(LOC,'NDF',THERE,STATUS)

*  create NDF array
          IF (.NOT.THERE) THEN
            CALL DAT_NEW(LOC,'NDF','NDF',1,1,STATUS)
            CALL DAT_FIND(LOC,'NDF',MLOC,STATUS)
            NNDF=1
*  or extend existing one
          ELSE
            CALL DAT_FIND(LOC,'NDF',MLOC,STATUS)
            CALL DAT_SIZE(MLOC,NNDF,STATUS)
            NNDF=NNDF+1
            CALL DAT_ALTER(MLOC,1,NNDF,STATUS)
          ENDIF

*  copy data
          CALL DAT_CELL(MLOC,1,NNDF,MNLOC,STATUS)
          CALL HDX_COPY(GLOC,MNLOC,STATUS)

*  tidy up
          CALL DAT_ANNUL(MNLOC,STATUS)
          CALL DAT_ANNUL(MLOC,STATUS)

          IF (STATUS.NE.SAI__OK) THEN
            CALL ERR_REP(' ','from GMD_ADDNDF',STATUS)
          ENDIF

      ENDIF

      END
