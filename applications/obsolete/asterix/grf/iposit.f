*+  IPOSIT - set current position
      SUBROUTINE IPOSIT(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*     22 Oct 90: V1.2-1 positions in various frames (RJV)
*      1 Jul 93: V1.2-2 GTR used (RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Functions :
*    Local Constants :
*    Local variables :
      CHARACTER*1 CH
      CHARACTER*20 SRA,SDEC
      DOUBLE PRECISION RA,DEC,ELON,ELAT,B,L
      REAL X,Y,XPIX,YPIX
      INTEGER FRAME
*    Global Variables :
      INCLUDE 'IMG_CMN'
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='IPOSIT Version 1.2-2')
*-

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  ensure transformations correct
        CALL GTR_RESTORE(STATUS)

*  cursor mode
        IF (I_MODE.EQ.1) THEN
          CALL MSG_PRNT(' ')
          CALL MSG_PRNT('Select position...')

          CALL PGCURSE(X,Y,CH)

*  keyboard mode
        ELSE

*  get coordinate frame
          CALL PAR_GET0I('FRAME',FRAME,STATUS)

          IF (FRAME.EQ.1) THEN
            CALL PAR_GET0C('RA',SRA,STATUS)
            CALL PAR_GET0C('DEC',SDEC,STATUS)
            CALL CONV_RADEC(SRA,SDEC,RA,DEC,STATUS)
            CALL IMG_CELTOWORLD(RA,DEC,X,Y,STATUS)

          ELSEIF (FRAME.EQ.2) THEN
            CALL PAR_GET0D('ELON',ELON,STATUS)
            CALL PAR_GET0D('ELAT',ELAT,STATUS)
            CALL IMG_ECLTOWORLD(ELON,ELAT,X,Y,STATUS)

          ELSEIF (FRAME.EQ.3) THEN
            CALL PAR_GET0D('L',L,STATUS)
            CALL PAR_GET0D('B',B,STATUS)
            CALL IMG_GALTOWORLD(L,B,X,Y,STATUS)

          ELSEIF (FRAME.EQ.4) THEN
            CALL PAR_GET0R('X',X,STATUS)
            CALL PAR_GET0R('Y',Y,STATUS)

          ELSEIF (FRAME.EQ.5) THEN
            CALL PAR_GET0R('XPIX',XPIX,STATUS)
            CALL PAR_GET0R('YPIX',YPIX,STATUS)
            CALL IMG_PIXTOWORLD(XPIX,YPIX,X,Y,STATUS)

          ENDIF

        ENDIF

        CALL IMG_SETPOS(X,Y,STATUS)

      ENDIF

      END
