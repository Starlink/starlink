*+  GFX_VPORT - sets plotting viewport within given zone
      SUBROUTINE GFX_VPORT(X1,X2,Y1,Y2,ABS,STATUS)

*    Description :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'GCB_PAR'
*    Import :
*    Import-export :
      REAL X1,X2,Y1,Y2
      LOGICAL ABS
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      REAL STDSIZE
      PARAMETER (STDSIZE=0.025)
*    Local variables :
      CHARACTER*8 OPT
      INTEGER NLINE
      INTEGER ILINE
      REAL XX1,XX2,YY1,YY2
      REAL LEFTMARG,RIGHTMARG,BOTMARG,TOPMARG
      REAL NSIZE,LSIZE,TSIZE,DEFSIZE
      LOGICAL OK
      LOGICAL X1OK,X2OK,Y1OK,Y2OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        ABS=.FALSE.

*  see if position of viewport set explicitly
        CALL GCB_GETR('POS_X1',X1OK,XX1,STATUS)
        CALL GCB_GETR('POS_X2',X2OK,XX2,STATUS)
        CALL GCB_GETR('POS_Y1',Y1OK,YY1,STATUS)
        CALL GCB_GETR('POS_Y2',Y2OK,YY2,STATUS)

        IF (X1OK.AND.X2OK.AND.Y1OK.AND.Y2OK) THEN

*  values > 1.0 indicate absolute units (mm)
          IF (XX1.GT.1.0.OR.XX2.GT.1.0.OR.
     :        YY1.GT.1.0.OR.YY2.GT.1.0)  THEN
            ABS=.TRUE.
            XX1=XX1/25.4
            XX2=XX2/25.4
            YY1=YY1/25.4
            YY2=YY2/25.4

          ENDIF

*  if absolute units then all sides must be given
        ELSEIF ((X1OK.AND.XX1.GT.1.0).OR.
     :          (X2OK.AND.XX2.GT.1.0).OR.
     :          (Y1OK.AND.YY1.GT.1.0).OR.
     :          (Y2OK.AND.YY2.GT.1.0)) THEN
*  otherwise signal error and revert to default viewport
          X1OK=.FALSE.
          X2OK=.FALSE.
          Y1OK=.FALSE.
          Y2OK=.FALSE.
          CALL MSG_PRNT('*** Error setting absolute size of viewport')
          CALL MSG_PRNT('    - position of all sides must be set')
        ENDIF

*  get default character size
        CALL GCB_GETR('DEFAULT_SIZE',OK,DEFSIZE,STATUS)
        IF (.NOT.OK) THEN
          DEFSIZE=1.0
        ENDIF

*  set default positions for sides not specified taking text into account
        IF (.NOT.X1OK) THEN

          CALL GCB_GETR('AXES_SIZE',OK,NSIZE,STATUS)
          IF (.NOT.OK) THEN
            NSIZE=DEFSIZE
          ENDIF
          CALL GCB_GETR('YLABEL_SIZE',OK,LSIZE,STATUS)
          IF (.NOT.OK) THEN
            LSIZE=DEFSIZE
          ENDIF
          LEFTMARG=(1.6*NSIZE+1.5*LSIZE)*STDSIZE

          XX1=X1+LEFTMARG

        ENDIF

        IF (.NOT.X2OK) THEN

          CALL GCB_GETC('KEY_OPT',OK,OPT,STATUS)
          IF (OK.AND.OPT.NE.' ') THEN
            RIGHTMARG=0.1
          ELSE
            RIGHTMARG=0.05
          ENDIF

          XX2=X2-RIGHTMARG

        ENDIF

        IF (.NOT.Y1OK) THEN

          CALL GCB_GETR('AXES_SIZE',OK,NSIZE,STATUS)
          IF (.NOT.OK) THEN
            NSIZE=DEFSIZE
          ENDIF
          CALL GCB_GETR('XLABEL_SIZE',OK,LSIZE,STATUS)
          IF (.NOT.OK) THEN
            LSIZE=DEFSIZE
          ENDIF
          BOTMARG=(1.6*NSIZE+1.5*LSIZE)*STDSIZE

          YY1=Y1+BOTMARG

        ENDIF

        IF (.NOT.Y2OK) THEN

          CALL GCB_GETI('TITLE_N',OK,NLINE,STATUS)
          IF (OK) THEN
            TOPMARG=0.0
            DO ILINE=1,NLINE
              CALL GCB_GET1R('TITLE_SIZE',ILINE,1,OK,TSIZE,STATUS)
              IF (.NOT.OK) THEN
                TSIZE=DEFSIZE
              ENDIF
              TOPMARG=TOPMARG+1.5*TSIZE*STDSIZE
            ENDDO
          ELSE
            TOPMARG=2.0*DEFSIZE*STDSIZE
          ENDIF

          YY2=Y2-TOPMARG

        ENDIF

*  return viewport used
        X1=XX1
        X2=XX2
        Y1=YY1
        Y2=YY2

*  fix for xwindow - loss of right axis when NDC is 1.0
        IF (XX2.EQ.1.0) THEN
          XX2=0.999
        ENDIF

*  coordinates in absolute units
        IF (ABS) THEN
          CALL PGVSIZE(XX1,XX2,YY1,YY2)
*  NDC coords
        ELSE
          CALL PGVPORT(XX1,XX2,YY1,YY2)
        ENDIF



        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_VPORT',STATUS)
        ENDIF

      ENDIF

      END
