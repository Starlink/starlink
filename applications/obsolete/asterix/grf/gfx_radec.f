*+  GFX_RADEC - plot RADEC axes
      SUBROUTINE GFX_RADEC(UNITS,RA,DEC,ROLL,STATUS)

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
      CHARACTER*(*) UNITS
      DOUBLE PRECISION RA,DEC,ROLL
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      REAL DEGTOMIN,SECTOMIN
      PARAMETER (DEGTOMIN=60.0,SECTOMIN=1.0/60.0)
      DOUBLE PRECISION DEGTORAD
      PARAMETER (DEGTORAD=3.14159265358979D0/180.0D0)
*    Local variables :
      CHARACTER RALBL*25,DECLBL*25
      CHARACTER*20 XOPT,YOPT
      DOUBLE PRECISION DECRAD
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
      INTEGER COL
      INTEGER DIV,XDIV,YDIV
      INTEGER DUMMY
      LOGICAL XSEC,YSEC
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  check for suitable orientation and latitude
        IF (ROLL.NE.0.0D0) THEN
          CALL MSG_PRNT('AST_ERR: non-zero roll angle')
          CALL MSG_PRNT('         RA/DEC axes not possible')
        ELSEIF (DEC.GT.85.0D0) THEN
          CALL MSG_PRNT('AST_ERR: declination too high')
          CALL MSG_PRNT('         RA/DEC axes not possible')
        ELSE

          DECRAD=DEC*DEGTORAD
          COSDEC=REAL(COS(DECRAD))

*  get axis unit conversion factor to arcmin
          CALL CHR_UCASE(UNITS)
          IF (INDEX(UNITS,'DEG').GT.0) THEN
            CONV=DEGTOMIN
          ELSEIF (INDEX(UNITS,'MIN').GT.0) THEN
            CONV=1.0
          ELSEIF (INDEX(UNITS,'SEC').GT.0) THEN
            CONV=SECTOMIN
          ELSE
            CONV=1.0
          ENDIF

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

*  x-axis numbers
          IF (RA1.GT.RA2) THEN
            POS=RA2+XTICK-MOD(RA2,XTICK)
            DO WHILE (POS.LT.RA1)
              HH=INT(POS/60.0)
              MM=INT(MOD(POS,60.0))
              SS=NINT(MOD(POS*60.0,60.0))
              IF (XSEC) THEN
                WRITE(RALBL,'(I2,A,I2.2,A,I2.2)',IOSTAT=DUMMY)
     :                HH,'\uh\d',MM,'\um\d',SS
              ELSE
                WRITE(RALBL,'(I2,A,I2.2)',IOSTAT=DUMMY) HH,'\uh\d',MM
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
     :                        DD,'\(718)',MM,'\(716)',SS,'\(717)'
            ELSE
              WRITE(DECLBL,'(SP,I3,A,SS,I2.2,A)',IOSTAT=DUMMY)
     :                        DD,'\(718)',MM,'\(716)'
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
