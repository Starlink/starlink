*+  G_MARK - draw one or more graph markers
      SUBROUTINE G_MARK(STATUS)

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
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*1 CH
      CHARACTER*(DAT__SZLOC) XLOC,YLOC
      REAL X,Y
      INTEGER N,NX,NY
      INTEGER XPTR,YPTR
      INTEGER SYMBOL
      LOGICAL KEY,LEFT,RIGHT
*-
      CALL PAR_GET0L('KEY',KEY,STATUS)

      IF (KEY) THEN
        CALL DAT_ASSOC('X','READ',XLOC,STATUS)
        CALL DAT_ASSOC('Y','READ',YLOC,STATUS)
        CALL DAT_MAPV(XLOC,'_REAL','READ',XPTR,NX,STATUS)
        CALL DAT_MAPV(YLOC,'_REAL','READ',YPTR,NY,STATUS)
        CALL PAR_GET0I('SYMBOL',SYMBOL,STATUS)

        N=MIN(NX,NY)

        CALL PGPOINT(N,%VAL(XPTR),%VAL(YPTR),SYMBOL)

        CALL DAT_ANNUL(XLOC,STATUS)
        CALL DAT_ANNUL(YLOC,STATUS)

      ELSE
        CALL PAR_GET0I('SYMBOL',SYMBOL,STATUS)
        CALL MSG_PRNT('Select points to mark (X to eXit)...')
        CH=' '
        DO WHILE (CH.NE.'X'.AND.STATUS.EQ.SAI__OK)
          CALL GFX_CURS(X,Y,LEFT,RIGHT,CH,STATUS)
          IF (CH.NE.'X') THEN
            CALL PGPOINT(1,X,Y,SYMBOL)
          ENDIF
        ENDDO
      ENDIF

      END
