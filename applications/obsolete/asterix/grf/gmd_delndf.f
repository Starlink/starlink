*+  GMD_DELNDF - delete specified NDF from multiple dataset
      SUBROUTINE GMD_DELNDF(LOC,NUM,STATUS)

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
      CHARACTER*(DAT__SZLOC) LOC	! multigraph dataset
      INTEGER NUM			! graph number
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) MLOC
      CHARACTER*(DAT__SZLOC) MNLOC
      CHARACTER*(DAT__SZLOC) MNNLOC
      CHARACTER*(DAT__SZLOC) CLOC
      CHARACTER*(DAT__SZLOC) ILOC
      CHARACTER*(DAT__SZLOC) ELOC
      CHARACTER*(DAT__SZLOC) ENLOC
      CHARACTER*(DAT__SZNAM) NAME
      CHARACTER*75 ENTRY
      INTEGER NNDF
      INTEGER INDF
      INTEGER NCOMP
      INTEGER ICOMP
      INTEGER ISIZE
      LOGICAL ITHERE
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL DAT_FIND(LOC,'NDF',MLOC,STATUS)
        CALL DAT_SIZE(MLOC,NNDF,STATUS)

*  see if index present
        CALL DAT_THERE(LOC,'INDEX',ITHERE,STATUS)
        IF (ITHERE) THEN
          CALL CMP_SIZE(LOC,'INDEX',ISIZE,STATUS)
        ENDIF

        IF (NUM.LT.1.OR.NUM.GT.NNDF) THEN
          CALL DAT_ANNUL(MLOC,STATUS)
          CALL MSG_PRNT('AST_ERR: invalid NDF number for deletion')
          STATUS=SAI__ERROR

*  only one graph so delete whole structure
        ELSEIF (NNDF.EQ.1) THEN
          CALL DAT_ANNUL(MLOC,STATUS)
          CALL DAT_ERASE(LOC,'NDF',STATUS)
          IF (ITHERE) THEN
            CALL DAT_ERASE(LOC,'INDEX',STATUS)
          ENDIF
        ELSE
*  cascade other graphs to fill gap
          DO INDF=NUM,NNDF
            CALL DAT_CELL(MLOC,1,INDF,MNLOC,STATUS)
            CALL DAT_NCOMP(MNLOC,NCOMP,STATUS)
            DO ICOMP=1,NCOMP
              CALL DAT_INDEX(MNLOC,1,CLOC,STATUS)
              CALL DAT_NAME(CLOC,NAME,STATUS)
              CALL DAT_ANNUL(CLOC,STATUS)
              CALL DAT_ERASE(MNLOC,NAME,STATUS)
            ENDDO
            IF (INDF.LT.NNDF) THEN
              CALL DAT_CELL(MLOC,1,INDF+1,MNNLOC,STATUS)
              CALL DAT_NCOMP(MNNLOC,NCOMP,STATUS)
              DO ICOMP=1,NCOMP
                CALL DAT_INDEX(MNNLOC,ICOMP,CLOC,STATUS)
                CALL DAT_NAME(CLOC,NAME,STATUS)
                CALL DAT_COPY(CLOC,MNLOC,NAME,STATUS)
                CALL DAT_ANNUL(CLOC,STATUS)
              ENDDO
              CALL DAT_ANNUL(MNNLOC,STATUS)
              CALL DAT_ANNUL(MNLOC,STATUS)
            ELSE
              CALL DAT_ANNUL(MNLOC,STATUS)
            ENDIF
          ENDDO

*  cascade index if present
          IF (ITHERE.AND.NUM.LE.ISIZE) THEN
            CALL DAT_FIND(LOC,'INDEX',ILOC,STATUS)
            CALL DAT_CELL(ILOC,1,NUM,ELOC,STATUS)
            DO INDF=NUM,NNDF-1
              CALL DAT_CELL(ILOC,1,INDF+1,ENLOC,STATUS)
              CALL DAT_GET0C(ENLOC,ENTRY,STATUS)
              CALL DAT_PUT0C(ELOC,ENTRY,STATUS)
              CALL DAT_ANNUL(ELOC,STATUS)
              ELOC=ENLOC
            ENDDO
            ENTRY=' '
            CALL DAT_PUT0C(ELOC,ENTRY,STATUS)
            CALL DAT_ANNUL(ELOC,STATUS)
            CALL DAT_ALTER(ILOC,1,NNDF-1,STATUS)
            CALL DAT_ANNUL(ILOC,STATUS)

          ENDIF

*  adjust size
          CALL DAT_ALTER(MLOC,1,NNDF-1,STATUS)

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMD_DELNDF',STATUS)
        ENDIF

      ENDIF

      END
