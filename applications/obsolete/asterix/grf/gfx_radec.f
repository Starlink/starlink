*+  GFX_RADEC - plot RADEC axes
      SUBROUTINE GFX_RADEC(PIXID,PRJID,SYSID,STATUS)

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
      INCLUDE 'MATH_PAR'
*    Import :
      INTEGER			PIXID,PRJID,SYSID
*    Status :
      INTEGER STATUS
*    Local Constants :
      REAL DEGTOMIN,SECTOMIN
      PARAMETER (DEGTOMIN=60.0,SECTOMIN=1.0/60.0)
*    Local variables :
      CHARACTER*1 ESC,U,D
      CHARACTER RALBL*25,DECLBL*25
      CHARACTER*20 XOPT,YOPT
      DOUBLE PRECISION DECRAD
      DOUBLE PRECISION RA,DEC,SPOINT(2),ROLL,UCONV(2)
      REAL RA1,RA2,DEC1,DEC2
      REAL RARAN,DECRAN
      REAL X1,X2,Y1,Y2
      REAL CONV
      REAL POS,RELPOS
      REAL COSDEC
      REAL XTICK,YTICK
      REAL SIZE
      INTEGER DD,HH,MM,SS
      INTEGER C
      INTEGER WID
      INTEGER FONT
      INTEGER BOLD
      INTEGER COL,DUM
      INTEGER DIV,XDIV,YDIV
      INTEGER DUMMY
      LOGICAL XSEC,YSEC
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  Extract info from ADI objects
        CALL ADI_CGET0D( PIXID, 'ROTATION', ROLL, STATUS )
        CALL ADI_CGET1D( PRJID, 'SPOINT', 2, SPOINT, DUM, STATUS )
        RA = SPOINT(1)
        DEC = SPOINT(2)

*  check for suitable orientation and latitude
        IF (ROLL.NE.0.0D0) THEN
          CALL MSG_PRNT('AST_ERR: non-zero roll angle')
          CALL MSG_PRNT('         RA/DEC axes not possible')
        ELSEIF (DEC.GT.85.0D0) THEN
          CALL MSG_PRNT('AST_ERR: declination too high')
          CALL MSG_PRNT('         RA/DEC axes not possible')
        ELSE

          DECRAD=DEC*MATH__DDTOR
          COSDEC=REAL(COS(DECRAD))

*      Get axis unit conversion factor to arcmin. ADI stores conversion to
*      radians
          CALL ADI_CGET1D( PIXID, 'UCONV', 2, UCONV, DUM, STATUS )
          CONV = UCONV(1)*MATH__DRTOD*60.0

*  find range of RA/DEC in minutes
          CALL PGQWIN(X1,X2,Y1,Y2)
          RA1=(REAL(RA)*DEGTOMIN+X1*CONV/COSDEC)/15.0
          RA2=(REAL(RA)*DEGTOMIN+X2*CONV/COSDEC)/15.0
          DEC1=REAL(DEC)*DEGTOMIN+Y1*CONV
          DEC2=REAL(DEC)*DEGTOMIN+Y2*CONV
          RARAN=ABS(RA1-RA2)
          DECRAN=ABS(DEC2-DEC1)

*  reset world coords temporarily
          CALL PGWINDOW(RA1,RA2,DEC1,DEC2)

*  if tick spacing hasn't been specified then set defaults
          CALL GCB_GETR('XAXIS_TICK',OK,XTICK,STATUS)
          IF (.NOT.OK) THEN
            IF (RARAN.GT.60.0) THEN
              XTICK=20.0
              DIV=5
            ELSEIF (RARAN.GT.20.0) THEN
              XTICK=10.0
              DIV=5
            ELSEIF (RARAN.GT.10.0) THEN
              XTICK=5.0
              DIV=5
            ELSEIF (RARAN.GT.5.0) THEN
              XTICK=2.0
              DIV=4
            ELSEIF (RARAN.GT.2.0) THEN
              XTICK=1.0
              DIV=6
            ELSEIF (RARAN.GT.1.0) THEN
              XTICK=0.5
              DIV=3
            ELSEIF (RARAN.GT.0.5) THEN
              XTICK=0.25
              DIV=3
            ELSEIF (RARAN.GT.1.0/6.0) THEN
              XTICK=1.0/12.0
              DIV=5
            ELSE
              XTICK=1.0/30.0
              DIV=2
            ENDIF
          ENDIF
          XSEC=(XTICK.LT.1.0)
          CALL GCB_GETI('XAXIS_DIV',OK,XDIV,STATUS)
          IF (.NOT.OK) THEN
            XDIV=DIV
          ENDIF

          CALL GCB_GETR('YAXIS_TICK',OK,YTICK,STATUS)
          IF (.NOT.OK) THEN
            IF (DECRAN.GT.600.0) THEN
              YTICK=60.0
              DIV=1
            ELSEIF (DECRAN.GT.300.0) THEN
              YTICK=60.0
              DIV=2
            ELSEIF (DECRAN.GT.120.0) THEN
              YTICK=60.0
              DIV=6
            ELSEIF (DECRAN.GT.60.0) THEN
              YTICK=30.0
              DIV=3
            ELSEIF (DECRAN.GT.30.0) THEN
              YTICK=10.0
              DIV=2
            ELSEIF (DECRAN.GT.10.0) THEN
              YTICK=5.0
              DIV=5
            ELSEIF (DECRAN.GT.5.0) THEN
              YTICK=2.0
              DIV=4
            ELSEIF (DECRAN.GT.2.0) THEN
              YTICK=1.0
              DIV=3
            ELSEIF (DECRAN.GT.1.0) THEN
              YTICK=0.5
              DIV=3
            ELSE
              YTICK=1.0/3.0
              DIV=4
            ENDIF
          ENDIF
          YSEC=(YTICK.LT.1.0)
          CALL GCB_GETI('YAXIS_DIV',OK,YDIV,STATUS)
          IF (.NOT.OK) THEN
            YDIV=DIV
          ENDIF

*  draw box and tick marks
          CALL GCB_GETC('XAXIS_OPT',OK,XOPT,STATUS)
          IF (.NOT.OK) THEN
            XOPT='BCTS'
          ELSE
            C=INDEX(XOPT,'N')
            IF (C.GT.0) THEN
              XOPT(C:C)=' '
            ENDIF
          ENDIF
          CALL GCB_GETC('YAXIS_OPT',OK,YOPT,STATUS)
          IF (.NOT.OK) THEN
            YOPT='BCTS'
          ELSE
            C=INDEX(YOPT,'N')
            IF (C.GT.0) THEN
              YOPT(C:C)=' '
            ENDIF
          ENDIF

          CALL GCB_SETDEF(STATUS)
          CALL GCB_GETI('AXES_WIDTH',OK,WID,STATUS)
          IF (OK) THEN
            CALL PGSLW(WID)
          ENDIF
          CALL GCB_GETI('AXES_COLOUR',OK,COL,STATUS)
          IF (OK) THEN
            CALL PGSCI(COL)
          ENDIF

          CALL PGBOX(XOPT,XTICK,XDIV,YOPT,YTICK,YDIV)
          CALL GCB_SETDEF(STATUS)

*  set attributes for numbers
          CALL GCB_GETI('AXES_BOLD',OK,BOLD,STATUS)
          IF (OK) THEN
            CALL PGSLW(BOLD)
          ENDIF
          CALL GCB_GETI('AXES_FONT',OK,FONT,STATUS)
          IF (OK) THEN
            CALL PGSCF(FONT)
          ENDIF
          CALL GCB_GETR('AXES_SIZE',OK,SIZE,STATUS)
          IF (OK) THEN
            CALL PGSCH(SIZE)
          ENDIF
          CALL GCB_GETI('AXES_COLOUR',OK,COL,STATUS)
          IF (OK) THEN
            CALL PGSCI(COL)
          ENDIF

*  fix for linux as it doesn't like backslash in string litorals
          ESC=CHAR(92)
          U=CHAR(92)//'u'
          D=CHAR(92)//'d'

*  x-axis numbers
          IF (RA1.GT.RA2) THEN
            POS=RA2+XTICK-MOD(RA2,XTICK)
            DO WHILE (POS.LT.RA1)
              HH=INT(POS/60.0)
              MM=INT(MOD(POS,60.0))
              SS=NINT(MOD(POS*60.0,60.0))
              IF (XSEC) THEN
                WRITE(RALBL,'(I2,A,I2.2,A,I2.2)',IOSTAT=DUMMY)
     :                HH,U//'h'//D,MM,U//'m'//D,SS
              ELSE
                WRITE(RALBL,'(I2,A,I2.2)',IOSTAT=DUMMY)
     :                               HH,U//'h'//D,MM
              ENDIF
              RELPOS=(RA1-POS)/RARAN
              CALL PGMTEXT('B',1.3,RELPOS,0.5,RALBL)
              POS=POS+XTICK
            ENDDO
          ELSE
            POS=RA1+XTICK-MOD(RA1,XTICK)
            DO WHILE (POS.LT.RA2)
              HH=INT(POS/60.0)
              MM=INT(MOD(POS,60.0))
              SS=NINT(MOD(POS*60.0,60.0))
              IF (XSEC) THEN
                WRITE(RALBL,'(I2,A1,I2.2,A1,I2.2)',IOSTAT=DUMMY)
     :                                            HH,'h',MM,'m',SS
              ELSE
                WRITE(RALBL,'(I2,A1,I2.2)',IOSTAT=DUMMY) HH,'h',MM
              ENDIF
              RELPOS=(POS-RA1)/RARAN
              CALL PGMTEXT('B',1.3,RELPOS,0.5,RALBL)
              POS=POS+XTICK
            ENDDO
          ENDIF

*  y-axis numbers
          IF (DEC1.LT.0.0) THEN
            POS=DEC1-MOD(DEC1,YTICK)
          ELSE
            POS=DEC1+YTICK-MOD(DEC1,YTICK)
          ENDIF
          DO WHILE (POS.LT.DEC2)
            DD=INT(POS/60.0)
            MM=ABS(INT(MOD(POS,60.0)))
            SS=ABS(NINT(MOD(POS*60.0,60.0)))
            IF (YSEC) THEN
              WRITE(DECLBL,'(SP,I3,A,SS,I2.2,A,I2.2,A)',IOSTAT=DUMMY)
     :                                DD,ESC//'(718)',MM,ESC//'(716)',
     :                                                 SS,ESC//'(717)'
            ELSE
              WRITE(DECLBL,'(SP,I3,A,SS,I2.2,A)',IOSTAT=DUMMY)
     :                        DD,ESC//'(718)',MM,ESC//'(716)'
            ENDIF
            RELPOS=(POS-DEC1)/DECRAN
            IF (INDEX(YOPT,'V').GT.0) THEN
              CALL PGMTEXT('LV',0.3,RELPOS,1.0,DECLBL)
            ELSE
              CALL PGMTEXT('L',0.3,RELPOS,0.5,DECLBL)
            ENDIF
            POS=POS+YTICK
          ENDDO

*  restore transformations
          CALL PGWINDOW(X1,X2,Y1,Y2)

*  restore default attributes
          CALL GCB_SETDEF(STATUS)

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_RADEC',STATUS)
        ENDIF
      ENDIF
      END
