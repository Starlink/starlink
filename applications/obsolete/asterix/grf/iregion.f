*+  IREGION - select a region of the plot
      SUBROUTINE IREGION(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    3 Oct 94 : v1.7-0 original (RJV)
*   13 Dec 94 : v1.8-0 sub-mode added (RJV)
*    6 Jan 95 : v1.8-1 writes ARD (RJV)
*   25 Jan 95 : v1.8-2 MODE and SUBMODE merged (RJV)
*   27 Jan 95 : v1.8-3 XSPOKES mode added (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*16 CMD
      CHARACTER*8 MODE,SUBMODE
      INTEGER RPTR
      INTEGER ID
      LOGICAL EXCLUDE
      LOGICAL MERGE
      REAL XMIN,XMAX,YMIN,YMAX
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IREGION Version 2.2-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  make sure transformations are correct
        CALL GTR_RESTORE(STATUS)

*  get main mode
        MERGE=.FALSE.
        EXCLUDE=.FALSE.
        CMD=' '
        DO WHILE (CMD.EQ.' '.AND.STATUS.EQ.SAI__OK)
          CALL USI_GET0C('MODE',CMD,STATUS)
          CALL CHR_UCASE(CMD)
          IF (CMD.EQ.'HELP') THEN
            CALL IREGION_HELP()
            CALL USI_CANCL('MODE',STATUS)
            CMD=' '
          ENDIF
        ENDDO

        CALL IREGION_PARSE(CMD,MODE,SUBMODE,EXCLUDE,MERGE,STATUS)

*  if fresh region - clear decks
        IF (SUBMODE.EQ.'NEW') THEN
          CALL IMG_SETWHOLE(STATUS)
          MERGE=.FALSE.
          CALL ARX_RESET(I_ARD_ID,STATUS)
        ELSEIF (MERGE) THEN
*  otherwise save current region mask and get memory to store new stuff
          RPTR=I_REG_PTR
          CALL DYN_MAPB(1,I_NX*I_NY,I_REG_PTR,STATUS)
          IF (EXCLUDE) THEN
            CALL ARR_INIT1B('01'X,I_NX*I_NY,%val(I_REG_PTR),STATUS)
          ELSE
            CALL ARR_INIT1B('00'X,I_NX*I_NY,%val(I_REG_PTR),STATUS)
          ENDIF
        ENDIF

        IF (EXCLUDE.AND.I_REG_TYPE.EQ.'WHOLE') THEN
          CALL ARR_INIT1B('01'X,I_NX*I_NY,%val(I_REG_PTR),STATUS)
          I_REG_TYPE='COMPLEX'
        ENDIF

        IF (STATUS.EQ.SAI__OK) THEN


          IF (MODE.EQ.'CIR') THEN
            CALL IREGION_CIRCLE(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'BOX') THEN
            CALL IREGION_BOX(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'POL') THEN
            CALL IREGION_POLYGON(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'ANN') THEN
            CALL IREGION_ANNULUS(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'ELL') THEN
            CALL IREGION_ELLIPSE(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'WHO') THEN
            CALL IREGION_WHOLE(STATUS)
          ELSEIF (MODE.EQ.'SLI') THEN
            CALL IREGION_SLICE(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'CON') THEN
            CALL IREGION_CONTOUR(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'SHO') THEN
            CALL IREGION_SHOW(STATUS)
          ELSEIF (MODE.EQ.'LIS') THEN
            CALL IREGION_LIST(STATUS)
          ELSEIF (MODE.EQ.'EXP') THEN
            CALL IREGION_EXPORT(STATUS)
          ELSEIF (MODE.EQ.'IMP') THEN
            CALL IREGION_IMPORT(STATUS)
          ELSEIF (MODE.EQ.'INV') THEN
            CALL IREGION_INVERT(STATUS)
          ELSEIF (MODE.EQ.'XSP') THEN
            CALL IREGION_XSPOKES(SUBMODE,EXCLUDE,STATUS)
          ENDIF


        ENDIF

*  combine with previously defined regions
        IF (MERGE) THEN
          CALL IREGION_MERGE(SUBMODE,%val(I_REG_PTR),%val(RPTR),STATUS)
          CALL DYN_UNMAP(I_REG_PTR,STATUS)
          I_REG_PTR=RPTR
        ENDIF

*  get outer limits of box enclosing region
        CALL IMG_REGLIM(%val(I_REG_PTR),STATUS)
        CALL IMG_PIXTOWORLD(REAL(I_IX1)-0.5,REAL(I_IY1)-0.5,XMIN,YMIN,
     :                                                        STATUS)
        CALL IMG_PIXTOWORLD(REAL(I_IX2)+0.5,REAL(I_IY2)+0.5,XMAX,YMAX,
     :                                                        STATUS)


*  if running from GUI - save region type to noticeboard
        IF (I_GUI) THEN
          CALL NBS_FIND_ITEM(I_NBID,'REGION',ID,STATUS)
          CALL NBS_PUT_CVALUE(ID,0,I_REG_TYPE,STATUS)
          CALL NBS_FIND_ITEM(I_NBID,'REGXMIN',ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,XMIN,STATUS)
          CALL NBS_FIND_ITEM(I_NBID,'REGXMAX',ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,XMAX,STATUS)
          CALL NBS_FIND_ITEM(I_NBID,'REGYMIN',ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,YMIN,STATUS)
          CALL NBS_FIND_ITEM(I_NBID,'REGYMAX',ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,YMAX,STATUS)
        ENDIF


      ENDIF

      CALL USI_CLOSE()

      END



*+
      SUBROUTINE IREGION_MERGE(MODE,NEWREG,OLDREG,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import:
      CHARACTER*(*) MODE
      BYTE NEWREG(I_NX,I_NY)
*    Import/Export :
      BYTE OLDREG(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB,BIT_ORUB
*    Local constants :
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (MODE.EQ.'AND') THEN

          DO J=1,I_NY
            DO I=1,I_NX
              OLDREG(I,J)=BIT_ANDUB(OLDREG(I,J),NEWREG(I,J))
            ENDDO
          ENDDO

        ELSEIF (MODE.EQ.'ADD') THEN

          DO J=1,I_NY
            DO I=1,I_NX
              OLDREG(I,J)=BIT_ORUB(OLDREG(I,J),NEWREG(I,J))
            ENDDO
          ENDDO

        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_CIRCLE(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      INTEGER L
      REAL XC,YC,RAD
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETCIRC('XC','YC','RAD',XC,YC,RAD,STATUS)
        CALL IMG_SETCIRC(XC,YC,RAD,EXCLUDE,STATUS)

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (CIRCLE( '
        ELSE
          TEXT(L:)=' CIRCLE( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('RAD',RAD)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^RAD ',TEXT,L)
        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_CIRCLE',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_ANNULUS(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      REAL XC,YC,IRAD,ORAD
      INTEGER L
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETANNULUS('XC','YC','IRAD','ORAD',XC,YC,IRAD,ORAD,
     :                                                       STATUS)
        CALL IMG_SETANNULUS(XC,YC,IRAD,ORAD,EXCLUDE,STATUS)

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (CIRCLE( '
        ELSE
          TEXT(L:)=' (CIRCLE( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('RAD',ORAD)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^RAD )',TEXT,L)

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)


        TEXT='     .AND..NOT.CIRCLE('
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('RAD',IRAD)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^RAD ))',TEXT,L)
        IF (EXCLUDE) THEN
          L=L+1
          TEXT(L:)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)




        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_ANNULUS',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_ELLIPSE(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      INTEGER L
      REAL XC,YC,ANGLE,MAJOR,MINOR
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETELLIPSE('XC','YC','MAJOR','MINOR','ANGLE',
     :                           XC,YC,MAJOR,MINOR,ANGLE,STATUS)
        CALL IMG_SETELLIPSE(XC,YC,MAJOR,MINOR,ANGLE,EXCLUDE,STATUS)

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (ELLIPSE( '
        ELSE
          TEXT(L:)=' ELLIPSE( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('MAJ',MAJOR)
        CALL MSG_SETR('MIN',MINOR)
        ANGLE=ANGLE*I_XSCALE/ABS(I_XSCALE)*I_YSCALE/ABS(I_YSCALE)
        CALL MSG_SETR('AN',ANGLE)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^MAJ , ^MIN , ^AN ',
     :                                                    TEXT,L)
        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_ELLIPSE',STATUS)
        ENDIF

      ENDIF

      END





*+
      SUBROUTINE IREGION_SLICE(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
      REAL RTOD
      PARAMETER (RTOD=180.0/3.14159265)
*    Local variables :
      CHARACTER*80 TEXT
      INTEGER L
      REAL XC,YC,ANGLE,LENGTH,WIDTH
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETSLICE('XC','YC','ANGLE','LENGTH','WIDTH',
     :                           XC,YC,ANGLE,LENGTH,WIDTH,STATUS)
        CALL IMG_SETSLICE(XC,YC,ANGLE,LENGTH,WIDTH,EXCLUDE,STATUS)

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (ROTBOX( '
        ELSE
          TEXT(L:)=' ROTBOX( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('LN',LENGTH)
        CALL MSG_SETR('WD',WIDTH)
        ANGLE=ANGLE*RTOD*I_XSCALE/ABS(I_XSCALE)*I_YSCALE/ABS(I_YSCALE)
        CALL MSG_SETR('AN',ANGLE)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^LN , ^WD , ^AN ',
     :                                                    TEXT,L)
        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('IREGION_SLICE',STATUS)
        ENDIF

      ENDIF

      END






*+
      SUBROUTINE IREGION_POLYGON(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
      INTEGER NVMAX
      PARAMETER (NVMAX=100)
*    Local variables :
      CHARACTER*80 TEXT
      REAL XV(NVMAX),YV(NVMAX)
      INTEGER NV
      INTEGER L
      INTEGER I
      INTEGER NPAIR
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETPOLY(NVMAX,XV,YV,NV,STATUS)
        CALL IMG_SETPOLY(NV,XV,YV,EXCLUDE,STATUS)

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (POLYGON( '
        ELSE
          TEXT(L:)=' POLYGON( '
        ENDIF
        L=CHR_LEN(TEXT)
        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)
        TEXT = ' '
        L = 1
        NPAIR=0


        DO I=1,NV

*  write each vertex allowing for line continuation
          NPAIR=NPAIR+1
          CALL MSG_SETR( 'X', XV(I))
          CALL MSG_SETR( 'Y', YV(I))
          CALL MSG_MAKE( TEXT(:L)//' ^X , ^Y ,', TEXT, L )
          IF (NPAIR.EQ.3.AND.I.LT.NV) THEN
            CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)
            TEXT = ' '
            L = 1
            NPAIR=0
          ENDIF


        ENDDO

        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)



        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_POLYGON',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_BOX(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      REAL XC,YC,DX,DY
      INTEGER L
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETBOX('XC','YC','XWID','YWID',XC,YC,DX,DY,STATUS)
        CALL IMG_BOX(XC,YC,DX,DY,STATUS)
        CALL IMG_SETBOX(XC,YC,DX,DY,EXCLUDE,STATUS)

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (BOX( '
        ELSE
          TEXT(L:)=' BOX( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('XW',2.0*DX)
        CALL MSG_SETR('YW',2.0*DY)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^XW , ^YW ',TEXT,L)
        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_BOX',STATUS)
        ENDIF

      ENDIF

      END





*+  IREGION_XSPOKES - defines the ribs region on a PSPC image
      SUBROUTINE IREGION_XSPOKES(MODE,EXCLUDE,STATUS)
*    Description :
*     Defines the rib region on a PSPC image in an ARD spatial file.
*    Method :
*       The user is asked to define two opposite points on the image
*       where the inner circle and ribs intersect.
*    Deficiencies :
*    Bugs :
*    Authors :
*     LTVAD::RDS
*    History :
*      12 Feb 92: V1.6-1 original (RDS)
*      30 Mar 92: V1.6-2 allows existing ARD file to be updates (RDS)
*      10 Jun 92: V1.6-3 attempts to use the attitude file to determine
*                        the rib positions (RDS)
*      20 Apr 93: V1.6-4 uses a different attitude file for US data.
*      23 Apr 94: V1.6-5 converted for use with RAT lookup table
*      24 Apr 94: (V1.7-0) for new asterix release
*      03 May 94: (v1.7-1) takes correct action if image system not active
*      27-May-94: Opens output textfile with STATUS="UNKNOWN"
*      15-Jun-94: (v1.7-2)Takes a list (or file) of times to select the mean
*                 roll angle on. 0 roll is now valid.
*      20-Jun-94: (v1.7-3)workaround for bug in CHR_RTOC (uses DTOC)
*      27-Jan-95: incorporated into IREGION (RJV)
*      10-Jun-96: auto mode removed (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Structure definitions :
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
      DOUBLE PRECISION PI
        PARAMETER (PI=3.1415926535)
*    Local variables :
      CHARACTER CH
      CHARACTER*80 TEXT
c     CHARACTER*(DAT__SZLOC) ALOC       ! Locator to attitude file
      REAL X1,Y1,X2,Y2,XC,YC
      REAL RAD,RADWID,INNRAD,OUTRAD
      REAL SLENGTH                      ! length of spokes in degs
      REAL SWIDTH                       ! width of spokes in degs
      REAL XSPOKE,YSPOKE                ! centre of intersection of spoke
*                                       ! and central ring
      REAL DELTAX,DELTAY
      REAL XSP(4),YSP(4)                ! cornes of spoke
      REAL XEND,YEND                    ! centre of end of spoke
      REAL XANG                         ! X value to calc spoke angle from.
c     REAL RMEAN                        ! mean roll angle Degrees
      REAL THETA                        ! mean roll angle (radians)
      REAL EXTRA                        ! additional width of ribs (degs)
      REAL ANGLE
      REAL XSC,YSC
      INTEGER L
c     INTEGER RPNTR                     ! pointer to the roll angle array
c     INTEGER TPNTR                     ! pointer to the time array
c     INTEGER NVALS                     ! number of time/roll values
      INTEGER LP
*-

*  get first intersection
      CALL MSG_BLNK()
      CALL MSG_PRNT('Select an intersection of a '/
     &                 /'spoke and the central ring...')
      CALL PGCURSE(X1,Y1,CH)
      CALL PGPOINT(1,X1,Y1,2)

*  get second intersection
      CALL MSG_PRNT(' ')
      CALL MSG_PRNT('Select the intersection of the opposite '/
     &                 /'spoke and the central ring...')
      CALL PGCURSE(X2,Y2,CH)
      CALL PGPOINT(1,X2,Y2,2)

*  calculate radius
      RAD = SQRT( (X2 - X1)**2 + (Y2 - Y1)**2 ) / 2

*  calculate centre of ring
      XC = (X1 + X2) / 2.0
      YC = (Y1 + Y2) / 2.0

*  calculate the angle to south of the first spoke  (Terrestial west
*  is +90 degs). Find the angle from the centre with the largest
*  Y value
      IF (Y1 .GT. Y2) THEN
        XANG = X1
      ELSE
        XANG = X2
      ENDIF

      THETA = ASIN((XANG-XC)/RAD)


*  Allow the user to add a constant onto the width value
      CALL USI_GET0R('EXTRA', EXTRA, STATUS)

*  now create the spokes

*  calculate inner and outer radius of central ring
*  Set circle width - assuming image is in degrees.
      RADWID = 0.06 + EXTRA

      INNRAD = RAD - RADWID / 2.0
      OUTRAD = RAD + RADWID / 2.0

*  display central ring on image
      CALL IMG_CIRCLE(XC,YC,INNRAD,STATUS)
      CALL IMG_CIRCLE(XC,YC,OUTRAD,STATUS)

*  Write in the annulus description
      CALL IMG_SETANNULUS(XC,YC,INNRAD,OUTRAD,.FALSE.,STATUS)

      IF (MODE.EQ.'AND') THEN
        TEXT=' .AND.'
        L=7
      ELSE
        TEXT=' '
        L=1
      ENDIF

      IF (EXCLUDE) THEN
        TEXT(L:)=' .NOT.( (CIRCLE( '
      ELSE
        TEXT(L:)=' ( (CIRCLE( '
      ENDIF
      L=CHR_LEN(TEXT)

      CALL MSG_SETR('XC',XC)
      CALL MSG_SETR('YC',YC)
      CALL MSG_SETR('RAD',OUTRAD)
      CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^RAD )',TEXT,L)

      CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)


      TEXT='     .AND..NOT.CIRCLE('
      L=CHR_LEN(TEXT)

      CALL MSG_SETR('XC',XC)
      CALL MSG_SETR('YC',YC)
      CALL MSG_SETR('RAD',INNRAD)
      CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^RAD ))',TEXT,L)
      CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)

*  set the length and width of the spokes in degrees. NB: this does assume that
*  the image axes are degrees.
      SLENGTH = 0.69
      SWIDTH = 0.06 + EXTRA

*  Loop over each spoke
      DO LP=1,8

*  Calculate the X,Y position of the centre of the start of each spoke
        XSPOKE = RAD * SIN(THETA) + XC
        YSPOKE = RAD * COS(THETA) + YC

*  Calc. the four corners of the spoke
        DELTAX = COS(THETA) * SWIDTH / 2.
        DELTAY = SIN(THETA) * SWIDTH / 2.

        XSP(1) = XSPOKE - DELTAX
        XSP(2) = XSPOKE + DELTAX
        YSP(1) = YSPOKE + DELTAY
        YSP(2) = YSPOKE - DELTAY
*
*  Calc the centre coords of the end of the spoke
        XEND = XSPOKE + SLENGTH * SIN(THETA)
        YEND = YSPOKE + SLENGTH * COS(THETA)

*  get geometric centre of spoke
        XSC=XC+(RAD+SLENGTH/2.0)*SIN(THETA)
        YSC=YC+(RAD+SLENGTH/2.0)*COS(THETA)

        XSP(3) = XEND + DELTAX
        XSP(4) = XEND - DELTAX
        YSP(3) = YEND - DELTAY
        YSP(4) = YEND + DELTAY

*  Display this spoke on the image
        CALL PGMOVE(XSP(1),YSP(1))
        CALL PGDRAW(XSP(2),YSP(2))
        CALL PGDRAW(XSP(3),YSP(3))
        CALL PGDRAW(XSP(4),YSP(4))
        CALL PGDRAW(XSP(1),YSP(1))

        ANGLE=THETA*180.0/PI-90.0

*  Write  out the spoke description
        TEXT=' .OR. ROTBOX( '
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XSC)
        CALL MSG_SETR('YC',YSC)
        CALL MSG_SETR('LN',SLENGTH)
        CALL MSG_SETR('WD',SWIDTH)
        CALL MSG_SETR('AN',
     :          ANGLE*I_XSCALE/ABS(I_XSCALE)*I_YSCALE/ABS(I_YSCALE))
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^LN , ^WD , ^AN ',
     :                                                    TEXT,L)
        TEXT(L:L)=')'

        IF (LP.LT.8) THEN
          CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)
        ENDIF

*  add to mask
        ANGLE=ANGLE*3.142/180.0
        CALL IMG_SETSLICE(XSC,YSC,ANGLE,SLENGTH,SWIDTH,.FALSE.,STATUS)


*  Calculate the angle of the next spoke to south
        THETA = THETA + PI/4.

      ENDDO

      L=L+1
      TEXT(L:)=')'
      CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)

*  invert the mask if necessary
      IF (EXCLUDE) THEN
        CALL IMG_SETINV(STATUS)
      ENDIF


999   CONTINUE

      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from IREGION_XSPOKES',STATUS)
      ENDIF

      END



*+
      SUBROUTINE IREGION_WHOLE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN


        CALL IMG_SETWHOLE(STATUS)

        CALL ARX_RESET(I_ARD_ID,STATUS)
        CALL ARX_PUT(I_ARD_ID,0,'WHOLE',STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_WHOLE',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_CONTOUR(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      REAL LEV
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL USI_GET0R('LEV',LEV,STATUS)

        CALL IREGION_CONTOUR_SUB(LEV,%val(I_DPTR),MODE,EXCLUDE,
     :                               %val(I_REG_PTR),STATUS)
        I_REG_TYPE='COMPLEX'

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_CONTOUR',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_CONTOUR_SUB(LEV,D,MODE,EXCLUDE,REG,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL LEV
      REAL D(I_NX,I_NY)
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
      BYTE REG(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      REAL X,Y
      REAL XP,YP
      INTEGER L
      INTEGER I,J
      INTEGER NPIX
      BYTE FLAG
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (EXCLUDE) THEN
          FLAG='00'X
        ELSE
          FLAG='01'X
        ENDIF

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (PIXEL( '
        ELSE
          TEXT(L:)=' PIXEL( '
        ENDIF
        L=CHR_LEN(TEXT)
        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)
        TEXT = ' '
        L = 1
        NPIX=0


        DO J=1,I_NY
          DO I=1,I_NX

            IF (D(I,J).GE.LEV) THEN

              REG(I,J)=FLAG

*  write each pixel allowing for line continuation
              NPIX=NPIX+1
              XP=REAL(I)
              YP=REAL(J)
              CALL IMG_PIXTOWORLD(XP,YP,X,Y,STATUS)
              CALL MSG_SETR( 'X', X)
              CALL MSG_SETR( 'Y', Y)
              CALL MSG_MAKE( TEXT(:L)//' ^X , ^Y ,', TEXT, L )
              IF (NPIX.EQ.3.AND.I.LT.I_NX.AND.J.LT.I_NY) THEN
                CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)
                TEXT = ' '
                L = 1
                NPIX=0
              ENDIF



            ENDIF

          ENDDO
        ENDDO

        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_CONTOUR_SUB',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_IMPORT(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER MPTR
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL DYN_MAPI(1,I_NX*I_NY,MPTR,STATUS)
        CALL IMG_GETARD('TEXT',%val(MPTR),STATUS)
        CALL IMG_SETARD(%val(MPTR),.FALSE.,STATUS)
        CALL DYN_UNMAP(MPTR,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_IMPORT',STATUS)
        ENDIF

      ENDIF

      END





*+
      SUBROUTINE IREGION_INVERT(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ID
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_SETINV(STATUS)

        CALL ARX_OPEN('WRITE',ID,STATUS)
        CALL ARX_PUT(ID,0,'.NOT.(',STATUS)
        CALL ARX_COPY(I_ARD_ID,1,ID,0,STATUS)
        CALL ARX_PUT(ID,0,')',STATUS)
        CALL ARX_CLOSE(I_ARD_ID,STATUS)
        I_ARD_ID=ID

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_INVERT',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_SHOW(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GFX_QCONTOUR(I_NX,I_NY,
     :                 I_ZM_IX1,I_ZM_IX2,I_ZM_IY1,I_ZM_IY2,
     :                   %val(I_XPTR),%val(I_YPTR),%val(I_REG_PTR),
     :                                            QUAL__MASK,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_SHOW',STATUS)
        ENDIF

      ENDIF

      END





*+
      SUBROUTINE IREGION_EXPORT(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL ARX_WRITE('FILE',I_ARD_ID,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_EXPORT',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_IMPORT_MASK(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER			FID
      INTEGER PTR
      LOGICAL MATCH
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL USI_ASSOC('INP','BinDS|Array', 'READ', FID,STATUS)
        CALL IMG_MATCH(FID,MATCH,STATUS)
        CALL BDI_MAPB( FID, 'Data', 'READ', PTR, STATUS )
        CALL ARR_COP1B(I_NX*I_NY,%val(PTR),%val(I_REG_PTR),STATUS)
        I_REG_TYPE='COMPLEX'
        CALL USI_CANCL('INP',STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('IREGION_IMPORT',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_LIST(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL MSG_BLNK()
        CALL ARX_LIST(I_ARD_ID,STATUS)
        CALL MSG_BLNK()

      ENDIF

      END




*+
      SUBROUTINE IREGION_PARSE(CMD,MODE,SUBMODE,EXCLUDE,MERGE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
*    Import/Export :
      CHARACTER*(*) CMD
*    Export :
      CHARACTER*(*) MODE,SUBMODE
      LOGICAL EXCLUDE,MERGE
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN


        IF (INDEX(CMD,'CIR').NE.0) THEN		! circle
          MODE='CIR'
          MERGE=.TRUE.
        ELSEIF (INDEX(CMD,'BOX').NE.0) THEN	! box
          MODE='BOX'
          MERGE=.TRUE.
        ELSEIF (INDEX(CMD,'POL').NE.0) THEN	! polygon
          MODE='POL'
          MERGE=.TRUE.
        ELSEIF (INDEX(CMD,'ANN').NE.0) THEN	! annulus
          MODE='ANN'
          MERGE=.TRUE.
        ELSEIF (INDEX(CMD,'ELL').NE.0) THEN	! ellipse
          MODE='ELL'
          MERGE=.TRUE.
        ELSEIF (INDEX(CMD,'SLI').NE.0) THEN	! rectangular slice
          MODE='SLI'
          MERGE=.TRUE.
        ELSEIF (INDEX(CMD,'CON').NE.0) THEN	! inside contour
          MODE='CON'
          MERGE=.TRUE.
        ELSEIF (INDEX(CMD,'XSP').NE.0) THEN
          MODE='XSP'
          MERGE=.TRUE.
        ELSEIF (INDEX(CMD,'WHO').NE.0) THEN
          MODE='WHO'
          MERGE=.FALSE.
        ELSEIF (INDEX(CMD,'SHO').NE.0) THEN
          MODE='SHO'
          MERGE=.FALSE.
        ELSEIF (INDEX(CMD,'EXP').NE.0) THEN
          MODE='EXP'
          MERGE=.FALSE.
        ELSEIF (INDEX(CMD,'IMP').NE.0) THEN
          MODE='IMP'
          MERGE=.FALSE.
        ELSEIF (INDEX(CMD,'INV').NE.0) THEN
          MODE='INV'
          MERGE=.FALSE.
        ELSEIF (INDEX(CMD,'LIS').NE.0) THEN
          MODE='LIS'
          MERGE=.FALSE.

        ELSE
          CALL MSG_PRNT('AST_ERR: invalid mode')
          STATUS=SAI__ERROR
        ENDIF

*  if in a mergeable mode then get sub-mode
        IF (MERGE.AND.STATUS.EQ.SAI__OK) THEN


          IF (INDEX(CMD,'NOT').GT.0.OR.
     :           INDEX(CMD,'EXC').GT.0) THEN
            EXCLUDE=.TRUE.
          ELSE
            EXCLUDE=.FALSE.
          ENDIF

          IF (INDEX(CMD,'ADD').GT.0) THEN
            SUBMODE='ADD'
          ELSEIF (INDEX(CMD,'AND').GT.0) THEN
            SUBMODE='AND'
          ELSE
            SUBMODE='NEW'
          ENDIF

        ELSEIF (MODE.EQ.'IMP') THEN

          SUBMODE='NEW'

        ELSE

          SUBMODE=' '

        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_HELP()
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
*    Global variables :
*    Import :
*    Export :
*    Status :
*    Function declarations :
*    Local constants :
      INTEGER MLINE
      PARAMETER (MLINE=7)
      INTEGER PLINE
      PARAMETER (PLINE=7)
*    Local variables :
      CHARACTER*79 MTEXT(MLINE)
     :/' CIRcle  - circular region     BOX     - box parallel to axes',
     : ' POLygon - irregular polygon   SLIce   - rectangular slice',
     : ' ANNulus - annular region      ELLipse - elliptical region',
     : ' CONtour - within contour      XSPokes - ROSAT XRT spokes',
     : ' WHOle   - whole image         INVert  - invert region',
     : ' SHOw    - outline all regions LISt    - list ARD text',
     : ' IMPort  - input ARD           EXPort  - output ARD'/
      CHARACTER*79 PTEXT(PLINE)
     :/' ADD     - add a new region to previous definition',
     : ' AND     - select only overlap of new region with existing one',
     : ' NOT     - select pixels outside the specified region',
     : ' EXC     -   "      "       "     "      "       "',
     : ' ADDNOT  - add pixels outside new region to existing region',
     : ' ANDNOT  - select overlap of pixels outside new region',
     : '           with existing region'/
      INTEGER ILINE
*-

      CALL MSG_BLNK()
      CALL MSG_PRNT('Available modes are:')
      CALL MSG_BLNK()
      DO ILINE=1,MLINE
        CALL MSG_PRNT(MTEXT(ILINE))
      ENDDO

      CALL MSG_BLNK()
      CALL MSG_PRNT('With the following prefixes:')
      CALL MSG_BLNK()
      DO ILINE=1,PLINE
        CALL MSG_PRNT(PTEXT(ILINE))
      ENDDO


      END

