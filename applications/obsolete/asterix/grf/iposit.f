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
*      7 Apr 95: V1.8-0 list entry and selection (RJV)
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
      LOGICAL LOAD,SEL,SHOW
*    Global Variables :
      INCLUDE 'IMG_CMN'
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='IPOSIT Version 1.8-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  see if dealing with multiple positions
        CALL USI_GET0L('LOAD',LOAD,STATUS)
        IF (LOAD) THEN

          CALL IPOSIT_LOAD(STATUS)

        ELSEIF (I_NPOS.GT.0) THEN
          CALL USI_GET0L('SHOW',SHOW,STATUS)
          IF (SHOW) THEN

            CALL IPOSIT_SHOW(STATUS)

          ELSE
            CALL USI_GET0L('SEL',SEL,STATUS)
            IF (SEL) THEN

              CALL IPOSIT_SELECT(STATUS)

            ENDIF
          ENDIF
        ENDIF

*  single position mode
        IF (.NOT.(LOAD.OR.SHOW.OR.SEL)) THEN

          CALL IPOSIT_SINGLE(STATUS)

        ENDIF


      ENDIF

      CALL USI_CLOSE()

      END



*+  IPOSIT_SINGLE - set current position to given single position
      SUBROUTINE IPOSIT_SINGLE(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
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
*-
      IF (STATUS.EQ.SAI__OK) THEN

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
          CALL USI_GET0I('FRAME',FRAME,STATUS)

          IF (FRAME.EQ.1) THEN
            CALL USI_GET0C('RA',SRA,STATUS)
            CALL USI_GET0C('DEC',SDEC,STATUS)
            CALL CONV_RADEC(SRA,SDEC,RA,DEC,STATUS)
            CALL IMG_CELTOWORLD(RA,DEC,X,Y,STATUS)

          ELSEIF (FRAME.EQ.2) THEN
            CALL USI_GET0D('ELON',ELON,STATUS)
            CALL USI_GET0D('ELAT',ELAT,STATUS)
            CALL IMG_ECLTOWORLD(ELON,ELAT,X,Y,STATUS)

          ELSEIF (FRAME.EQ.3) THEN
            CALL USI_GET0D('L',L,STATUS)
            CALL USI_GET0D('B',B,STATUS)
            CALL IMG_GALTOWORLD(L,B,X,Y,STATUS)

          ELSEIF (FRAME.EQ.4) THEN
            CALL USI_GET0R('X',X,STATUS)
            CALL USI_GET0R('Y',Y,STATUS)

          ELSEIF (FRAME.EQ.5) THEN
            CALL USI_GET0R('XPIX',XPIX,STATUS)
            CALL USI_GET0R('YPIX',YPIX,STATUS)
            CALL IMG_PIXTOWORLD(XPIX,YPIX,X,Y,STATUS)

          ENDIF

        ENDIF

        CALL IMG_SETPOS(X,Y,STATUS)

      ENDIF


      END



*+  IPOSIT_LOAD - load a list of positions
      SUBROUTINE IPOSIT_LOAD(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIO_ERR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Functions :
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) SLOC
      CHARACTER*132 FILENAME
      CHARACTER*80 REC
      CHARACTER*20 RAS,DECS
      DOUBLE PRECISION RA,DEC
      REAL X,Y
      INTEGER IFD
      INTEGER RAPTR,DECPTR
      INTEGER ISRC,NSRC
      LOGICAL EXIST
      LOGICAL POK
*    Global Variables :
      INCLUDE 'IMG_CMN'
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get group id for storing positions
        IF (I_NPOS.EQ.0) THEN
          CALL GRP_NEW('Source list',I_POS_ID,STATUS)
*  or empty existing one
        ELSE
          CALL GRP_SETSZ(I_POS_ID,0,STATUS)
          I_NPOS=0
        ENDIF

*  get filename
        CALL USI_GET0C('LIST',FILENAME,STATUS)

        IF (STATUS.EQ.SAI__OK) THEN
*  see if file exists in form given
          INQUIRE(FILE=FILENAME,EXIST=EXIST)
*  if it does - assume it to be text file
          IF (EXIST) THEN
            CALL FIO_OPEN(FILENAME,'READ','NONE',0,IFD,STATUS)
            DO WHILE ( STATUS .EQ. SAI__OK )
              CALL FIO_READF(IFD,REC,STATUS)
              IF (STATUS.EQ.SAI__OK) THEN
*  ignore blank lines
                IF (REC.GT.' ') THEN
*  remove leading blanks
                  CALL CHR_LDBLK( REC )

*  split record into ra and dec
                  CALL CONV_SPLIT(REC,RAS,DECS,STATUS)
*  parse
                  CALL CONV_RADEC(RAS,DECS,RA,DEC,STATUS)
*  then convert back to a uniform format
                  CALL CONV_DEGHMS(REAL(RA),RAS)
                  CALL CONV_DEGDMS(REAL(DEC),DECS)
*  and store
                  CALL GRP_PUT(I_POS_ID,1,RAS(:11)//DECS(:11),0,STATUS)
                  I_NPOS=I_NPOS+1

*  make the first one the current position
                  IF (I_NPOS.EQ.1) THEN
                    CALL IMG_CELTOWORLD(RA,DEC,X,Y,STATUS)
                    CALL IMG_SETPOS(X,Y,STATUS)
                  ENDIF

                ENDIF
              ENDIF
            ENDDO
            IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )
            CALL FIO_CLOSE(IFD,STATUS)

*  otherwise assume HDS file in PSS format
          ELSE
            CALL HDS_OPEN( FILENAME,'READ',SLOC,STATUS )
            IF (STATUS.EQ.SAI__OK) THEN

*  check if sources
              CALL SSO_INIT( STATUS )
              CALL SSO_VALID( SLOC, POK, STATUS )
              CALL SSO_GETNSRC( SLOC, NSRC, STATUS )
              IF (.NOT. POK .OR.NSRC.EQ.0 ) THEN
                CALL MSG_PRNT('AST_ERR: No sources in this SSDS')
              ELSE
*  get RA DEC of sources
                CALL SSO_MAPFLD( SLOC, 'RA', '_DOUBLE', 'READ',
     :                                        RAPTR, STATUS )
                CALL SSO_MAPFLD( SLOC, 'DEC', '_DOUBLE', 'READ',
     :                                        DECPTR, STATUS )
                DO ISRC=1,NSRC
*  get each position
                  CALL ARR_ELEM1D(RAPTR,NSRC,ISRC,RA,STATUS)
                  CALL ARR_ELEM1D(DECPTR,NSRC,ISRC,DEC,STATUS)

*  convert to a uniform format
                  CALL CONV_DEGHMS(REAL(RA),RAS)
                  CALL CONV_DEGDMS(REAL(DEC),DECS)

*  and store
                  CALL GRP_PUT(I_POS_ID,1,RAS(:11)//DECS(:11),0,STATUS)
                  I_NPOS=I_NPOS+1

*  make the first one the current position
                  IF (I_NPOS.EQ.1) THEN
                    CALL IMG_CELTOWORLD(RA,DEC,X,Y,STATUS)
                    CALL IMG_SETPOS(X,Y,STATUS)
                  ENDIF

                ENDDO

              ENDIF

              CALL SSO_RELEASE(SLOC,STATUS)
              CALL HDS_CLOSE(SLOC,STATUS)

            ENDIF

          ENDIF

        ENDIF

      ENDIF

      END




*+  IPOSIT_SELECT - select current position from list
      SUBROUTINE IPOSIT_SELECT(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
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
      CHARACTER*80 REC
      CHARACTER*20 RAS,DECS
      DOUBLE PRECISION RA,DEC
      REAL X,Y
      INTEGER NUM
*    Global Variables :
      INCLUDE 'IMG_CMN'
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get number
        CALL USI_GET0I('NUM',NUM,STATUS)
        IF (NUM.LE.0.OR.NUM.GT.I_NPOS) THEN
          CALL MSG_PRNT('AST_ERR: invalid position number')

        ELSE

*  get entry from list
          CALL GRP_GET(I_POS_ID,NUM,1,REC,STATUS)
*  split record into ra and dec
          CALL CONV_SPLIT(REC,RAS,DECS,STATUS)
*  parse
          CALL CONV_RADEC(RAS,DECS,RA,DEC,STATUS)
*  and store as current position
          CALL IMG_CELTOWORLD(RA,DEC,X,Y,STATUS)
          CALL IMG_SETPOS(X,Y,STATUS)

        ENDIF

      ENDIF

      END





*+  IPOSIT_SHOW - list positions
      SUBROUTINE IPOSIT_SHOW(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
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
      CHARACTER*40 REC/' '/
      INTEGER IPOS
*    Global Variables :
      INCLUDE 'IMG_CMN'
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL MSG_BLNK()
        DO IPOS=1,I_NPOS
          CALL GRP_GET(I_POS_ID,IPOS,1,REC(6:),STATUS)
          WRITE(REC(:3),'(I3)') IPOS
          CALL MSG_PRNT(REC)
        ENDDO
        CALL MSG_BLNK()

      ENDIF

      END

