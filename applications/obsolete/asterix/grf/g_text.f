*+  G_TEXT - write text at arbitrary position and angle
      SUBROUTINE G_TEXT(STATUS)

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
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*80 TEXT,CH*1,CJ*1
      REAL X,Y,ANGLE,JUST
      LOGICAL KEY,LEFT,RIGHT
*-
      CALL PAR_GET0L('KEY',KEY,STATUS)

      CALL PAR_GET0C('TEXT',TEXT,STATUS)
      CALL PAR_GET0C('JUST',CJ,STATUS)
      CALL CHR_UCASE(CJ)
      IF (CJ.EQ.'L') THEN
        JUST=0.0
      ELSEIF (CJ.EQ.'C') THEN
        JUST=0.5
      ELSEIF (CJ.EQ.'R') THEN
        JUST=1.0
      ELSE
        JUST=0.0
      ENDIF
      CALL PAR_GET0R('ANGLE',ANGLE,STATUS)

      IF (KEY) THEN
        CALL PAR_GET0R('X',X,STATUS)
        CALL PAR_GET0R('Y',Y,STATUS)
      ELSE
        CALL MSG_PRNT('Select position...')
        CALL GFX_CURS(X,Y,LEFT,RIGHT,CH,STATUS)
      ENDIF

      CALL PGPTEXT(X,Y,ANGLE,JUST,TEXT)


      END
