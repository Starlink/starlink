*+  ISEP - get angular separation between two points
      SUBROUTINE ISEP(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*           BHVAD::RJV
*    History :
*      24 Oct 90: operates in cursor and keyboard mode (RJV)
*       5 Dec 91: default for cursor mode (RJV)
*      21 Oct 93: removed extra STATUS arguments from MSG_SETx calls (DJA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      DOUBLE PRECISION PI,DTOR,RTOD,DTOM,DTOS
      PARAMETER (PI=3.141592653589979D0,DTOR=PI/180.0D0,RTOD=180.0D0/PI,
     :               DTOM=60.0D0,DTOS=3600.0D0)
*    Local variables :
      CHARACTER*1 CH
      CHARACTER*15 SRA,SDEC
      DOUBLE PRECISION AZ1,AZ2,EL1,EL2
      DOUBLE PRECISION SEPR,SEPD,SEPM,SEPS
      REAL X1,X2,Y1,Y2
      REAL XPIX1,XPIX2,YPIX1,YPIX2
      INTEGER FRAME
      LOGICAL CURSOR
*    Functions :
      DOUBLE PRECISION SLA_DSEP
*    Global Variables :
      INCLUDE 'IMG_CMN'
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='ISEP Version 1.3-4')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

*  set mode of operation
      IF (.NOT.I_OPEN) THEN
        CURSOR=.FALSE.
      ELSEIF (.NOT.I_DISP) THEN
        CURSOR=.FALSE.
      ELSE
        CURSOR=(I_MODE.EQ.1)
      ENDIF

*  cursor mode
      IF (CURSOR) THEN

*  ensure transformations are correct
        CALL GTR_RESTORE(STATUS)

*  get first point
        CALL MSG_PRNT(' ')
        X1=I_X
        Y2=I_Y
        CALL MSG_SETR('X',X1)
        CALL MSG_SETR('Y',Y1)
        CALL MSG_PRNT('Select first point/^X,^Y/...')
        CALL PGCURSE(X1,Y1,CH)
        IF (CH.EQ.CHAR(13).OR.CH.EQ.CHAR(50)) THEN
          X1=I_X
          Y1=I_Y
        ENDIF
        CALL PGPOINT(1,X1,Y1,2)

*  second point
        CALL MSG_PRNT('Select second point...')
        X2=I_X
        Y2=I_Y
        CALL PGCURSE(X2,Y2,CH)
        CALL PGPOINT(1,X2,Y2,2)

*  convert to spherical polars
        CALL IMG_WORLDTOCEL(X1,Y1,AZ1,EL1,STATUS)
        CALL IMG_WORLDTOCEL(X2,Y2,AZ2,EL2,STATUS)

*  keyboard mode
      ELSE


*  get coordinate frame
        CALL USI_GET0I('FRAME',FRAME,STATUS)

        IF (.NOT.I_OPEN.AND.FRAME.GT.3) THEN
          CALL MSG_PRNT('AST_ERR: image processing system must be '//
     :                        'active to use these coordinates')
          STATUS=SAI__ERROR

        ELSEIF (FRAME.EQ.1) THEN				! RA/DEC
          CALL USI_GET0C('RA1',SRA,STATUS)
          CALL USI_GET0C('DEC1',SDEC,STATUS)
          CALL CONV_RADEC(SRA,SDEC,AZ1,EL1,STATUS)
          CALL USI_GET0C('RA2',SRA,STATUS)
          CALL USI_GET0C('DEC2',SDEC,STATUS)
          CALL CONV_RADEC(SRA,SDEC,AZ2,EL2,STATUS)

        ELSEIF (FRAME.EQ.2) THEN			! Ecliptic
          CALL USI_GET0D('ELON1',AZ1,STATUS)
          CALL USI_GET0D('ELAT1',EL1,STATUS)
          CALL USI_GET0D('ELON2',AZ2,STATUS)
          CALL USI_GET0D('ELAT2',EL2,STATUS)

        ELSEIF (FRAME.EQ.3) THEN			! Galactic
          CALL USI_GET0D('L1',AZ1,STATUS)
          CALL USI_GET0D('B1',EL1,STATUS)
          CALL USI_GET0D('L2',AZ2,STATUS)
          CALL USI_GET0D('B2',EL2,STATUS)

        ELSEIF (FRAME.EQ.4) THEN			! axis coords
          CALL USI_GET0R('X1',X1,STATUS)
          CALL USI_GET0R('Y1',Y1,STATUS)
          CALL USI_GET0R('X2',X2,STATUS)
          CALL USI_GET0R('Y2',Y2,STATUS)
          CALL IMG_WORLDTOCEL(X1,Y1,AZ1,EL1,STATUS)
          CALL IMG_WORLDTOCEL(X2,Y2,AZ2,EL2,STATUS)

        ELSEIF (FRAME.EQ.5) THEN			! pixel coords
          CALL USI_GET0R('XPIX1',XPIX1,STATUS)
          CALL USI_GET0R('YPIX1',YPIX1,STATUS)
          CALL USI_GET0R('XPIX2',XPIX2,STATUS)
          CALL USI_GET0R('YPIX2',YPIX2,STATUS)
          CALL IMG_PIXTOWORLD(XPIX1,YPIX1,X1,Y1,STATUS)
          CALL IMG_PIXTOWORLD(XPIX2,YPIX2,X2,Y2,STATUS)
          CALL IMG_WORLDTOCEL(X1,Y1,AZ1,EL1,STATUS)
          CALL IMG_WORLDTOCEL(X2,Y2,AZ2,EL2,STATUS)

        ENDIF


      ENDIF

      IF (STATUS.EQ.SAI__OK) THEN
*  convert to radian
        AZ1=AZ1*DTOR
        EL1=EL1*DTOR
        AZ2=AZ2*DTOR
        EL2=EL2*DTOR

*  get separation
        SEPR=SLA_DSEP(AZ1,EL1,AZ2,EL2)
        SEPD=SEPR*RTOD
        SEPM=SEPD*DTOM
        SEPS=SEPD*DTOS

*  output result
        CALL MSG_BLNK()
        CALL MSG_PRNT('Angular separation:')
        CALL MSG_SETD('RAD',SEPR)
        CALL MSG_PRNT('          radian: ^RAD')
        CALL MSG_SETD('DEG',SEPD)
        CALL MSG_PRNT('          degree: ^DEG')
        IF (SEPM.GT.1.0D0.AND.SEPM.LT.60.0D0) THEN
          CALL MSG_SETD('MIN',SEPM)
          CALL MSG_PRNT('          arcmin: ^MIN')
        ENDIF
        IF (SEPS.GT.1.0D0.AND.SEPS.LT.60.0D0) THEN
          CALL MSG_SETD('SEC',SEPS)
          CALL MSG_PRNT('          arcsec: ^SEC')
        ENDIF
        CALL MSG_BLNK()

      ENDIF

      CALL USI_CLOSE()

      END


