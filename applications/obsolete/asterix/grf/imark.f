*+  IMARK - marks points on image (current or from list)
      SUBROUTINE IMARK(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*       1 Oct 90: V1.2-2 option to mark current position
*                 V1.2-3 new SSO calls
*      18 Sep 91: V1.2-4 accepts text file (RJV)
*      19 Sep 91: V1.2-5 can set symbol and colour (RJV)
*       6 Feb 92: V1.2-5 individual HDS arrays for RA and DEC (RJV)
*      31 Jul 92: V1.2-7 NUMBER option added (DJA)
*      15 Sep 92: V1.2-8 Bug fixed when numbering ascii lists (DJA)
*       4 Oct 92: V1.2-9 " (DJA)
*      20 Jan 93: V1.7-0 Uses GCB to remember marks (RJV)
*       1 Jul 93: V1.7-1 GTR used (RJV)
*      16 Aug 93: V1.7-2 Error reporting for FIO corrected (DJA)
*       8 Aug 94: V1.7-3 Length of filename increased (RJV)
*       5 Sep 94: V1.7-4 OFF option added (RJV)
*       6 Sep 94: V1.7-5 numbering hived off to GFX routine (RJV)
*      10 Apr 95: V1.8-0 ALL option for internal list (RJV)
*      14 Nov 95: V2.0-0 Support for HEASARC database format (DJA)
*       8 Feb 96: V2.0-1 Changed access to SSDS files (DJA)
*      19 May 97: V2.1-2 Support for STL format (RJV)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIO_ERR'
      INCLUDE 'PAR_ERR'

*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) RLOC
      CHARACTER*(DAT__SZLOC) DLOC
      CHARACTER*132 FILENAME
      CHARACTER*80 REC,RAS*20,DECS*20
      DOUBLE PRECISION RA,DEC
      REAL SIZE
      INTEGER I
      INTEGER SFID,NSRC,NPOS
      INTEGER RAPTR,DECPTR
      INTEGER SYMBOL,COLOUR,BOLD
      INTEGER NMARK
      INTEGER IFD
      LOGICAL OK
      LOGICAL CURR
      LOGICAL ALL
      LOGICAL EXIST
      LOGICAL NUMBER
      LOGICAL OFF
      LOGICAL HDB
      LOGICAL STL

*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IMARK Version 2.2-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image being displayed')
      ELSE

*  ensure transformations correct
        CALL GTR_RESTORE(STATUS)
        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)

*  see if OFF-mode
        CALL USI_GET0L('OFF',OFF,STATUS)

        IF (OFF.AND.STATUS.EQ.SAI__OK) THEN
          CALL GCB_CANI('MARKER_N',STATUS)
          CALL GCB_CANL('MARKER_NUMBER',STATUS)

        ELSE

*  get symbol, colour and size
          CALL USI_GET0I('SYMBOL',SYMBOL,STATUS)
          CALL USI_GET0I('COLOUR',COLOUR,STATUS)
          CALL USI_GET0R('SIZE',SIZE,STATUS)
          CALL USI_GET0I('BOLD',BOLD,STATUS)

*  mark positions in internal list?
          IF (I_NPOS.GT.0) THEN
            CALL USI_GET0L('ALL',ALL,STATUS)
          ELSE
            ALL =.FALSE.
          ENDIF

*  see if only current position to be marked
          IF (.NOT.ALL) THEN
            CALL USI_GET0L('CURR',CURR,STATUS)
          ELSE
            CURR=.FALSE.
          ENDIF

*      Number sources?
          IF ( .NOT. CURR ) THEN
            CALL USI_GET0L('NUMBER',NUMBER,STATUS)
          ELSE
            NUMBER = .FALSE.
          ENDIF
          CALL GCB_SETL('MARKER_NUMBER',NUMBER,STATUS)


          IF (CURR) THEN

            CALL GCB_GETI('MARKER_N',OK,NMARK,STATUS)
            IF (.NOT.OK) THEN
              NMARK=0
            ENDIF
            NMARK=NMARK+1
            CALL GCB_SETI('MARKER_N',NMARK,STATUS)
            CALL GCB_SET1I('MARKER_SYMBOL',NMARK,1,SYMBOL,STATUS)
            CALL GCB_SET1R('MARKER_X',NMARK,1,I_X,STATUS)
            CALL GCB_SET1R('MARKER_Y',NMARK,1,I_Y,STATUS)
            CALL GCB_SET1R('MARKER_SIZE',NMARK,1,SIZE,STATUS)
            CALL GCB_SET1I('MARKER_BOLD',NMARK,1,BOLD,STATUS)
            CALL GCB_SET1I('MARKER_COLOUR',NMARK,1,COLOUR,STATUS)

*  mark positions in current list
          ELSEIF (ALL) THEN

            DO I=1,I_NPOS

*  get each entry
              CALL GRP_GET(I_POS_ID,I,1,REC,STATUS)
*  split into ra and dec
              CALL CONV_SPLIT(REC,RAS,DECS,STATUS)
*  parse and save
              CALL CONV_RADEC(RAS,DECS,RA,DEC,STATUS)
              CALL IMARK_SAVE(1,RA,DEC,SYMBOL,COLOUR,SIZE,BOLD,STATUS)

            ENDDO

*  otherwise get list of positions from file
          ELSE

            CALL USI_GET0C('LIST',FILENAME,STATUS)
            IF (STATUS.EQ.SAI__OK) THEN
*  see if file exists in form given
              INQUIRE(FILE=FILENAME,EXIST=EXIST)
*  if it does need to decide what type of file, so open and take a look
              IF (EXIST) THEN
                STL=.FALSE.
                HDB=.FALSE.
                CALL FIO_OPEN(FILENAME,'READ','NONE',0,IFD,STATUS)
*  Is it in STL format?
                CALL IMG_CHKSTL(IFD,STL,STATUS)
                CALL FIO_CLOSE(IFD,STATUS)

*  Is it a HEASARC file?
                IF (.NOT.STL) THEN
                  CALL USI_GET0L( 'HDB', HDB, STATUS )
                ENDIF

                IF (STL) THEN
                  CALL IMARK_STL(FILENAME,SYMBOL,COLOUR,SIZE,BOLD,
     :                                                      STATUS)

                ELSEIF (HDB) THEN
                  CALL IMARK_HDB(FILENAME,SYMBOL,COLOUR,SIZE,BOLD,
     :                                                      STATUS)

                ELSE
                  CALL IMARK_PLAINTEXT(FILENAME,
     :                SYMBOL,COLOUR,SIZE,BOLD,STATUS)

                ENDIF


*  otherwise assume HDS file in PSS format
              ELSE
                CALL ADI_FOPEN( FILENAME,'SSDSset|SSDS','READ',SFID,
     :                          STATUS )
                IF (STATUS.EQ.SAI__OK) THEN

*  check if sources
                  CALL ADI_CGET0I( SFID, 'NSRC', NSRC, STATUS )
                  IF ( NSRC.EQ.0 ) THEN
                    CALL MSG_PRNT('AST_ERR: No sources in this SSDS')
                  ELSE
*  get RA DEC of sources
                    CALL SSI_MAPFLD( SFID, 'RA', '_DOUBLE', 'READ',
     :                                        RAPTR, STATUS )
                    CALL SSI_MAPFLD( SFID, 'DEC', '_DOUBLE', 'READ',
     :                                        DECPTR, STATUS )

*  save source positions
                    CALL IMARK_SAVE(NSRC,%VAL(RAPTR),%VAL(DECPTR),
     :                              SYMBOL,COLOUR,SIZE,BOLD,STATUS)

                  ENDIF

                  CALL SSI_RELEASE( SFID,STATUS)
                  CALL ADI_FCLOSE(SFID,STATUS)

                ENDIF

              ENDIF

*  look for individual HDS arrays
            ELSEIF (STATUS.EQ.PAR__NULL) THEN
              CALL ERR_ANNUL(STATUS)
              CALL USI_DASSOC('RA','READ',RLOC,STATUS)
              CALL USI_DASSOC('DEC','READ',DLOC,STATUS)
              CALL DAT_SIZE(RLOC,NPOS,STATUS)
              CALL DYN_MAPD(1,NPOS,RAPTR,STATUS)
              CALL DYN_MAPD(1,NPOS,DECPTR,STATUS)
              CALL DAT_GET1D(RLOC,NPOS,%VAL(RAPTR),NPOS,STATUS)
              CALL DAT_GET1D(DLOC,NPOS,%VAL(DECPTR),NPOS,STATUS)
              CALL IMARK_SAVE(NPOS,%VAL(RAPTR),%VAL(DECPTR),
     :                        SYMBOL,COLOUR,SIZE,BOLD,STATUS)
              CALL DYN_UNMAP(RAPTR,STATUS)
              CALL DYN_UNMAP(DECPTR,STATUS)
              CALL DAT_ANNUL(RLOC,STATUS)
              CALL DAT_ANNUL(DLOC,STATUS)
            ENDIF

          ENDIF

          CALL GFX_MARKS(STATUS)

        ENDIF

        CALL GCB_CACHE(I_CACHE,STATUS)

      ENDIF

      CALL USI_CLOSE()

      END



      SUBROUTINE IMARK_SAVE(N,RA,DEC,SYMBOL,COLOUR,SIZE,BOLD,STATUS)

*    Type definitions :
      IMPLICIT NONE
      INTEGER N
      DOUBLE PRECISION RA(N),DEC(N)
      REAL SIZE
      INTEGER SYMBOL,COLOUR,BOLD

      INTEGER STATUS

      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

      REAL X,Y,XP,YP
      INTEGER I,IX,IY
      INTEGER NMARK
      LOGICAL OK

      INCLUDE 'IMG_CMN'

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETI('MARKER_N',OK,NMARK,STATUS)
        IF (.NOT.OK) THEN
          NMARK=0
        ENDIF

        DO I=1,N


          CALL IMG_CELTOWORLD(RA(I),DEC(I),X,Y,STATUS)
          CALL IMG_WORLDTOPIX(X,Y,XP,YP,STATUS)
          IX=INT(XP+0.5)
          IY=INT(YP+0.5)

          IF (STATUS.EQ.SAI__OK) THEN
            IF (IX.GE.I_IX1.AND.IX.LE.I_IX2.AND.
     :          IY.GE.I_IY1.AND.IY.LE.I_IY2) THEN

              NMARK=NMARK+1
              CALL GCB_SETI('MARKER_N',NMARK,STATUS)
              CALL GCB_SET1I('MARKER_SYMBOL',NMARK,1,SYMBOL,STATUS)
              CALL GCB_SET1R('MARKER_X',NMARK,1,X,STATUS)
              CALL GCB_SET1R('MARKER_Y',NMARK,1,Y,STATUS)
              CALL GCB_SET1R('MARKER_SIZE',NMARK,1,SIZE,STATUS)
              CALL GCB_SET1I('MARKER_BOLD',NMARK,1,BOLD,STATUS)
              CALL GCB_SET1I('MARKER_COLOUR',NMARK,1,COLOUR,STATUS)


              I_X=X
              I_Y=Y

            ENDIF
          ENDIF


        ENDDO

      ENDIF

      END



      SUBROUTINE IMARK_PLAINTEXT(FILENAME,SYMBOL,COLOUR,SIZE,BOLD,
     :                                                      STATUS)

*    Type definitions :
      IMPLICIT NONE

      CHARACTER*(*) FILENAME
      REAL SIZE
      INTEGER SYMBOL,COLOUR,BOLD

      INTEGER STATUS

      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIO_PAR'
      INCLUDE 'FIO_ERR'

      CHARACTER*80 REC,RAS*20,DECS*20
      DOUBLE PRECISION RA,DEC

      INTEGER IFD

      IF (STATUS.EQ.SAI__OK) THEN

        CALL FIO_OPEN(FILENAME,'READ','NONE',0,IFD,STATUS)
        DO WHILE ( STATUS .EQ. SAI__OK )
          CALL FIO_READF(IFD,REC,STATUS)
          IF (STATUS.EQ.SAI__OK) THEN
*  ignore blank lines
            IF (REC.GT.' '.AND.REC(1:1).NE.'#') THEN

*  remove leading blanks
              CALL CHR_LDBLK( REC )

*  split record into ra and dec
              CALL CONV_SPLIT(REC,RAS,DECS,STATUS)
*  parse and save
              CALL CONV_RADEC(RAS,DECS,RA,DEC,STATUS)
            ENDIF
            CALL IMARK_SAVE(1,RA,DEC,SYMBOL,COLOUR,SIZE,BOLD,STATUS)
          ENDIF
        ENDDO
        IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )
        CALL FIO_CLOSE(IFD,STATUS)

      ENDIF

      END



      SUBROUTINE IMARK_HDB(FILENAME,SYMBOL,COLOUR,SIZE,BOLD,STATUS)

*    Type definitions :
      IMPLICIT NONE

      CHARACTER*(*) FILENAME
      REAL SIZE
      INTEGER SYMBOL,COLOUR,BOLD

      INTEGER STATUS

      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIO_PAR'
      INCLUDE 'FIO_ERR'
      INCLUDE 'MATH_PAR'

      CHARACTER*80 REC
      DOUBLE PRECISION RA,DEC,CEL(2),CEL1950(2)

      INTEGER IFD,FSTAT

      INCLUDE 'IMG_CMN'

      IF (STATUS.EQ.SAI__OK) THEN

        CALL FIO_OPEN(FILENAME,'READ','NONE',0,IFD,STATUS)
        DO WHILE ( STATUS .EQ. SAI__OK )
          CALL FIO_READF(IFD,REC,STATUS)
          IF (STATUS.EQ.SAI__OK) THEN
*  ignore blank lines
            IF (REC.GT.' '.AND.REC(1:1).NE.'#') THEN

               READ( REC, '(F11.7,1X,F11.7)',IOSTAT=FSTAT)
     :                                     CEL1950(1),CEL1950(2)
               IF ( FSTAT.NE. 0 ) THEN
                 STATUS = SAI__ERROR
                 CALL ERR_REP( ' ', 'Error reading HEASARC'/
     :                                  /' database file', STATUS )
               ELSE

*   Convert to file system
                 CEL1950(1) = CEL1950(1) * MATH__DDTOR
                 CEL1950(2) = CEL1950(2) * MATH__DDTOR
                 CALL WCI_CNS2S( I_FK4SYS, CEL1950, I_SYSID,
     :                                           CEL, STATUS )
                 RA = CEL(1) * MATH__DRTOD
                 DEC = CEL(2) * MATH__DRTOD

                 CALL IMARK_SAVE(1,RA,DEC,SYMBOL,COLOUR,SIZE,BOLD,
     :                                                      STATUS)

               ENDIF
            ENDIF
          ENDIF
        ENDDO
        IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )
        CALL FIO_CLOSE(IFD,STATUS)

      ENDIF

      END


      SUBROUTINE IMARK_STL(FILENAME,SYMBOL,COLOUR,SIZE,BOLD,STATUS)

*    Type definitions :
      IMPLICIT NONE

      CHARACTER*(*) FILENAME
      REAL SIZE
      INTEGER SYMBOL,COLOUR,BOLD

      INTEGER STATUS

      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
c      INCLUDE 'CAT_INC'
c      INCLUDE 'CIO_INC'

      DOUBLE PRECISION PI, DTOR
      PARAMETER (PI = 3.141592654D0, DTOR = PI/180.0D0)


      DOUBLE PRECISION RA,DEC

      INTEGER ROWS
      INTEGER GAI
      INTEGER  RAI      ! Right Ascension.
      INTEGER  DECI     ! Declination.
      INTEGER  SYMBI    ! Symbol.
      INTEGER  COLI     ! Colour.
      INTEGER  SUNI     ! Units.
      INTEGER  LABLI    ! Label.
      INTEGER  SIZ1I    ! First  size.
      INTEGER  SIZ2I    ! Second  "  .
      INTEGER  SIZ3I    ! Third   "  .
      INTEGER  SIZ4I     ! Fourth  "  .

      LOGICAL NULFLG,MORE
      INTEGER ROW


      IF (STATUS.EQ.SAI__OK) THEN


        CALL CAT_TOPEN (FILENAME, 'OLD', 'READ', GAI, STATUS)

        IF (STATUS.EQ.SAI__OK) THEN

          CALL CAT_TIDNT (GAI, 'RA', RAI, STATUS)
          CALL CAT_TIDNT (GAI, 'DEC', DECI, STATUS)
c          CALL CAT_TIDNT (GAI, 'SYMBOL', SYMBI, STATUS)
c          CALL CAT_TIDNT (GAI, 'COLOUR', COLI, STATUS)
c          CALL CAT_TIDNT (GAI, 'SUNITS', SUNI, STATUS)
c          CALL CAT_TIDNT (GAI, 'LABEL', LABLI, STATUS)


          CALL CAT_TROWS (GAI, ROWS, STATUS)

*
*  Process all the row in the list (or until an error occurs).


          ROW = 1
          MORE = .TRUE.

          DO WHILE (MORE.AND.ROW.LE.ROWS)


            CALL CAT_RGET (GAI, ROW, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN

*   Attempt to get the basic plotting attributes.

              CALL CAT_EGT0D (RAI, RA, NULFLG, STATUS)
              CALL CAT_EGT0D (DECI, DEC, NULFLG, STATUS)
              RA=RA/DTOR
              DEC=DEC/DTOR

c              CALL CAT_EGT0I (SYMBI, SYMBOL, NULFLG, STATUS)
c              CALL CAT_EGT0I (COLI, COLOUR, NULFLG, STATUS)
c              CALL CAT_EGT0I (SUNI, SUNITS, NULFLG, STATUS)
C              CALL CAT_EGT0C (LABLI, LABEL, NULFLG, STATUS)

c              CALL CAT_TIDNT (GAI, 'SIZE1', SIZ1I, STATUS)
c              IF (STATUS .EQ. CAT__NOCMP) THEN
c                CALL ERR_ANNUL (STATUS)
c              END IF

c              CALL CAT_TIDNT (GAI, 'SIZE2', SIZ2I, STATUS)
c              IF (STATUS .EQ. CAT__NOCMP) THEN
c                CALL ERR_ANNUL (STATUS)
c              END IF

c              CALL CAT_TIDNT (GAI, 'SIZE3', SIZ3I, STATUS)
c              IF (STATUS .EQ. CAT__NOCMP) THEN
c                CALL ERR_ANNUL (STATUS)
c              END IF

c              CALL CAT_TIDNT (GAI, 'SIZE4', SIZ4I, STATUS)
c              IF (STATUS .EQ. CAT__NOCMP) THEN
c                CALL ERR_ANNUL (STATUS)
c              END IF



*  Convert to PGPLOT colour indices
c              IF (COLOUR .EQ. CIO__CRED) THEN
c                COLOUR = 2
c              ELSE IF (COLOUR .EQ. CIO__CGRN) THEN
c                COLOUR = 3
c              ELSE IF (COLOUR .EQ. CIO__CBLUE) THEN
c                COLOUR = 4
c              ELSE IF (COLOUR .EQ. CIO__CCYAN) THEN
c                COLOUR = 5
c              ELSE IF (COLOUR .EQ. CIO__CMAGN) THEN
c                COLOUR = 6
c              ELSE IF (COLOUR .EQ. CIO__CYELL) THEN
c                COLOUR = 7
c              ELSE
c                COLOUR = 1
c              END IF

*  Convert to PGPLOT symbol numbers
c              IF (SYMBOL .EQ. CIO__SOPCR) THEN
c                SYMBOL = 4
c              ELSE IF (SYMBOL .EQ. CIO__SFLCR) THEN
c                SYMBOL = 17
c              ELSE IF (SYMBOL .EQ. CIO__SOPSQ) THEN
c                SYMBOL = 6
c              ELSE IF (SYMBOL .EQ. CIO__SFLSQ) THEN
c                SYMBOL = 16
c              ELSE IF (SYMBOL .EQ. CIO__SOPTR) THEN
c                SYMBOL = 7
c              ELSE IF (SYMBOL .EQ. CIO__SFLTR) THEN
c                SYMBOL = 13
c              ELSE IF (SYMBOL .EQ. CIO__SOPSR) THEN
c                SYMBOL = 12
c              ELSE IF (SYMBOL .EQ. CIO__SFLSR) THEN
c                SYMBOL = 18
c              ELSE IF (SYMBOL .EQ. CIO__SPLUS) THEN
c                SYMBOL = 2
c              ELSE IF (SYMBOL .EQ. CIO__SMULT) THEN
c                SYMBOL = 5
c              ELSE IF (SYMBOL .EQ. CIO__SAST) THEN
c                SYMBOL = 3
c              ELSE
c                SYMBOL=2
c              END IF


              CALL IMARK_SAVE(1,RA,DEC,SYMBOL,COLOUR,SIZE,BOLD,
     :                                                   STATUS)

            ELSE
              MORE=.FALSE.

            ENDIF

            ROW=ROW+1

          ENDDO

*  close catalogue file
          CALL CAT_TRLSE (GAI, STATUS)

        ENDIF



      ENDIF

      END
