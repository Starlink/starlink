*+  G_LINE - draw a polyline
      SUBROUTINE G_LINE(STATUS)

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
      LOGICAL KEY,LEFT,RIGHT
*-
      CALL PAR_GET0L('KEY',KEY,STATUS)

      IF (KEY) THEN
        CALL DAT_ASSOC('X','READ',XLOC,STATUS)
        CALL DAT_ASSOC('Y','READ',YLOC,STATUS)
        CALL DAT_MAPV(XLOC,'_REAL','READ',XPTR,NX,STATUS)
        CALL DAT_MAPV(YLOC,'_REAL','READ',YPTR,NY,STATUS)

        N=MIN(NX,NY)

        CALL PGLINE(N,%VAL(XPTR),%VAL(YPTR))

        CALL DAT_ANNUL(XLOC,STATUS)
        CALL DAT_ANNUL(YLOC,STATUS)

      ELSE
        CALL MSG_PRNT('Select points on line (X to eXit)...')
        CALL GFX_CURS(X,Y,LEFT,RIGHT,CH,STATUS)
        CALL PGMOVE(X,Y)
        DO WHILE (CH.NE.'X'.AND.STATUS.EQ.SAI__OK)
          CALL GFX_CURS(X,Y,LEFT,RIGHT,CH,STATUS)
          IF (CH.NE.'X') THEN
            CALL PGDRAW(X,Y)
          ENDIF
        ENDDO
      ENDIF


      END
