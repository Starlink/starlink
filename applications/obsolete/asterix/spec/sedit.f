*+  SEDIT - Displays fit model parameter values
      SUBROUTINE SEDIT( STATUS )
*
*    Description :
*
*     Allows creation and editing of FIT_MODEL structures using both a
*     command driven and screen oriented interface.
*
*    Environment parameters :
*
*     FIT_MOD=UNIV(U)
*		Object containing model (existing or not)
*     OVERRIDE=LOGICAL(R)
*		Overwrite existing model specification?
*     MODEL_SPEC=LITERAL(R)
*		String containing specification of composite model
*     VALUES=LITERAL(R)
*               String containing parameter value/limits
*     SCREEN=LOGICAL(R)
*               Run in screen mode?
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Peter Knight (BHVAD::PAK)
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     18 May 89 : V1.0    Modified from FIT_MODEL (BHVAD::TJP)
*      3 Jun 89 : V1.1    General tidying-up (PAK)
*     23 Apr 91 : V2.0    Version for ASTERIX88 (PAK)
*     23 Oct 92 : V1.7-0  Moved into ASTERIX (DJA)
*     25 Oct 92 : V1.7-1  SEDIT_LISTPAR changed to make it usable
*                         from SSHOW (DJA)
*     23 Aug 93 : V1.7-2  Screen mode added, handles model keys of any
*                         length. (DJA)
*     15 Apr 94 : V1.7-3  Reset errors added (DJA)
*      2 Aug 94 : V1.7-4  Allow Asterix list notation for parameter
*                         selection. (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'TSM_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*
*    Global variables :
*
      INCLUDE 'SPECLIB(SEDIT_CMN)'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                	CHR_LEN
      LOGICAL			CHR_SIMLR
*
*    Local variables :
*
      CHARACTER*200          	FILE, PATH       	! HDS_TRACE info
      CHARACTER*(DAT__SZLOC) 	FLOC             	! Locator to fit_model object
      CHARACTER*20           	NAME             	! Name of input file
      CHARACTER*(MAXKEYLEN)  	OPTION           	! Option character
      CHARACTER*(DAT__SZTYP) 	TYP              	! Object type

      INTEGER                	COMP(NPAMAX)     	! List of components
      INTEGER                	IOPT             	! Integer key code
      INTEGER                	NLEV             	! Number of levels from HDS_TRACE
      INTEGER                	OCH              	! Output channel id
      INTEGER                	PAR(NPAMAX)      	! List of parameters
      INTEGER                	PARTOT           	! Total number of parameters
      INTEGER			WIDTH			! Width of o/p channel

      LOGICAL                	QUIT             	! Quit program?
      LOGICAL                	SNEW             	! New fit_model object?
      LOGICAL                	UPDATE			! Update screen?
*
*    Version :
*
      CHARACTER*30         	VERSION            	! Version id
        PARAMETER               ( VERSION = 'SEDIT Version 1.7-4' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version
      CALL MSG_PRNT( VERSION )

*    Form or retrieve fit_model object
      CALL DAT_EXIST('FIT_MOD','UPDATE',FLOC,STATUS)

      IF ( STATUS .EQ. PAR__ERROR ) THEN

*      Object doesn't exist - Create it
        CALL ERR_ANNUL(STATUS)
	CALL DAT_CREAT('FIT_MOD','FIT_MODEL',0,0,STATUS)
        CALL DAT_ASSOC('FIT_MOD','WRITE',FLOC,STATUS)
        CALL DAT_NEWC(FLOC,'SPEC',80,0,0,STATUS)
        CALL DAT_NEWC(FLOC,'POLISH',80,0,0,STATUS)
        CALL DAT_NEW(FLOC,'NCOMP','_INTEGER',0,0,STATUS)
        SNEW=.TRUE.

      ELSE IF ( STATUS .NE. PAR__NULL ) THEN

*      Object already exists, so check it :
        CALL DAT_TYPE(FLOC,TYP,STATUS)
        IF ( TYP .NE. 'FIT_MODEL' ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Not a fit_model data object', STATUS )
        END IF
        SNEW = .FALSE.

      END IF
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Screen mode?
      CALL PAR_GET0L( 'SCREEN', SCREEN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    SEDIT needs a screen which is at least 132 columns
      IF ( SCREEN ) THEN
        CALL TSM_INIT( ' ', STATUS )
        CALL TSM_GETDIMS( 0, SNX, SNY, STATUS )
        IF ( SNX .LT. 132 ) THEN
          CALL MSG_PRNT( '** SEDIT needs a screen at least 132 columns'/
     :                   /' wide in screen mode **' )
          SCREEN = .FALSE.
        ELSE
          CALL TSM_CREWINB( SNX-2, SNY-5, 2, 2, W_BIG, STATUS )
          DNX = SNX-4
          DNY = SNY-11
          CALL TSM_CRESUB( W_BIG, DNX, DNY, 2, 2, W_DISP, STATUS )
          CALL TSM_PUTLABEL( W_BIG, '< Model Selection >', TSM__TOP,
     :                     TSM__CENTRE, STATUS )
          CALL TSM_CREWINB( SNX-2, 1, 2, SNY-1, W_STATUS, STATUS )
          CALL TSM_PUTSTRAT( W_STATUS, 'File : ', 2, 1, STATUS )
          DSTART = 1
          CLINE = 1
          CCOL = 0
          UPDATE=.TRUE.
        END IF
      END IF

*    Declare file to user :
      IF ( SCREEN ) THEN
        CALL HDS_TRACE( FLOC, NLEV, PATH, FILE, STATUS )
        CALL TSM_PUTSSTRAT( W_STATUS, FILE(:CHR_LEN(FILE)), TSM__BOLD,
     :                                                  9, 1, STATUS )
        CALL TSM_REFRESH( W_STATUS, STATUS )
      ELSE
        CALL DISP_FILENAM( FLOC, 'Model', STATUS )
      END IF

*    History file entry :
      CALL HIST_ADD( FLOC, VERSION, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    No existing model, so ask the user to input one :
      IF ( SNEW ) THEN
        CALL SEDIT_NEWMOD( FLOC, STATUS )
        IF ( SCREEN ) THEN
          CALL SEDIT_LISTPAR( FLOC, PARTOT, COMP, PAR, 6, STATUS )
        ELSE
          CALL SEDIT_LISTPAR( FLOC, PARTOT, COMP, PAR, -1, STATUS )
        END IF
      ELSE

*      Display component parameters
        CALL SEDIT_LISTPAR( FLOC, PARTOT, COMP, PAR, 6, STATUS )

*      Bad model?
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CALL MSG_PRNT( '** Input model file contains '/
     :                        /'invalid data items **' )
          CALL SEDIT_NEWMOD( FLOC, STATUS )
        END IF

      END IF
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Loop through menu :
      QUIT = .FALSE.
      DO WHILE ( (STATUS.EQ.SAI__OK) .AND. .NOT. QUIT )

*       Screen mode
         IF ( SCREEN ) THEN
           IF ( UPDATE ) THEN
             CALL SEDIT_UPDSCR( STATUS )
             UPDATE =.FALSE.
           END IF
           CALL SEDIT_CURSOR( .TRUE., STATUS )
           CALL TSM_RDCH( W_BIG, IOPT, STATUS )
           IF ( IOPT .LT. 255 ) OPTION = CHAR(IOPT)
         ELSE
           CALL PAR_GET0C('OPTION',OPTION,STATUS)
           CALL PAR_CANCL('OPTION',STATUS)
           IOPT = ICHAR(OPTION(1:1))
         END IF

*       Bad response?
         IF ( STATUS .NE. SAI__OK ) THEN
           CALL ERR_ANNUL( STATUS )

*       Reset errors?
         ELSE IF ( CHR_SIMLR(OPTION,'RE') ) THEN
           CALL SEDIT_ERESET(FLOC,PARTOT,COMP,PAR,STATUS)

*       Refresh screen
         ELSE IF ( (IOPT .EQ. TSM__K_CTRLL) .OR.
     :             (IOPT .EQ. TSM__K_UPPERCASE_R) .OR.
     :             (IOPT .EQ. TSM__K_LOWERCASE_R) ) THEN
           CALL TSM_REFRESH( W_BIG, STATUS )
           CALL TSM_REFRESH( W_STATUS, STATUS )
           UPDATE = .TRUE.

*       Alter parameters
         ELSE IF ( (IOPT .EQ. TSM__K_UPPERCASE_A) .OR.
     :             (IOPT .EQ. TSM__K_LOWERCASE_A) ) THEN
           CALL SEDIT_ALTERPAR(FLOC,PARTOT,COMP,PAR,STATUS)

*       Freeze parameters
         ELSE IF ( (IOPT .EQ. TSM__K_UPPERCASE_F) .OR.
     :             (IOPT .EQ. TSM__K_LOWERCASE_F) ) THEN
           CALL SEDIT_FR_OR_TH(FLOC,PARTOT,COMP,PAR,.TRUE.,STATUS)

*       Thaw parameters
         ELSE IF ( (IOPT .EQ. TSM__K_UPPERCASE_T) .OR.
     :             (IOPT .EQ. TSM__K_LOWERCASE_T) ) THEN
           CALL SEDIT_FR_OR_TH(FLOC,PARTOT,COMP,PAR,.FALSE.,STATUS)

*       Hardcopy of parameters
         ELSE IF ( (IOPT .EQ. TSM__K_UPPERCASE_P) .OR.
     :             (IOPT .EQ. TSM__K_LOWERCASE_P) ) THEN

*         Open the file
           CALL AIO_OPEN( 'PRINTER', 'LIST', OCH, WIDTH, STATUS )

*         Write model file description
           CALL DAT_NAME( FLOC, NAME, STATUS )
           CALL MSG_SETC( 'NAME', NAME )
           CALL AIO_WRITE( OCH, 'Input model file:- ^NAME', STATUS )

*         List parameters
           CALL SEDIT_LISTPAR( FLOC, PARTOT, COMP, PAR, OCH, STATUS )

*         Tidy up unit
           CALL AIO_CLOSE( OCH, STATUS )
           IF ( STATUS .NE. SAI__OK ) GOTO 99

*       List parameters to screen
         ELSE IF ( (IOPT .EQ. TSM__K_UPPERCASE_L) .OR.
     :             (IOPT .EQ. TSM__K_LOWERCASE_L) ) THEN
           CALL SEDIT_LISTPAR( FLOC, PARTOT, COMP, PAR, 6, STATUS )

*       New parameters
         ELSE IF ( CHR_SIMLR(OPTION,'NP') ) THEN
           CALL SEDIT_NEWPAR( FLOC, STATUS )

*       New model
         ELSE IF ( CHR_SIMLR(OPTION,'NM') ) THEN
           CALL SEDIT_NEWMOD( FLOC, STATUS )

*       Move cursor left
         ELSE IF ( SCREEN .AND. (IOPT.EQ.TSM__K_LEFT) ) THEN

*       Move cursor right
         ELSE IF ( SCREEN .AND. (IOPT.EQ.TSM__K_RIGHT) ) THEN

*       Move cursor down
         ELSE IF ( SCREEN .AND. (IOPT.EQ.TSM__K_DOWN) ) THEN

           CALL SEDIT_CURSOR( .FALSE., STATUS )
           IF ( CLINE .EQ. NLINE ) THEN
             CALL TSM_BELL( 1, STATUS )
           ELSE
             CALL SEDIT_SCRDISP( CLINE+1, STATUS )
             CLINE = CLINE + 1
           END IF

*       Move cursor up
         ELSE IF ( SCREEN .AND. (IOPT.EQ.TSM__K_UP) ) THEN

           CALL SEDIT_CURSOR( .FALSE., STATUS )
           IF ( CLINE .EQ. 1 ) THEN
             CALL TSM_BELL( 1, STATUS )
           ELSE
             CALL SEDIT_SCRDISP( CLINE-1, STATUS )
             CLINE = CLINE - 1
           END IF

*       Quit program?
         ELSE IF ( (IOPT .EQ. TSM__K_UPPERCASE_X) .OR.
     :             (IOPT .EQ. TSM__K_LOWERCASE_X) ) THEN
           IF ( SCREEN ) CALL SEDIT_CURSOR( .FALSE., STATUS )
           QUIT = .TRUE.

         END IF

      END DO

*    Exit
 99   CALL AST_ERR( STATUS )

      END


*+  SEDIT_NEWMOD -
      SUBROUTINE SEDIT_NEWMOD( FLOC, STATUS )
*    Description :
*     Allows the user to put a new model into the current model dataset.
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'TSM_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*
*    Global variables :
*
      INCLUDE 'SPECLIB(SEDIT_CMN)'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) FLOC               ! Locator to fit_model object
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER 			CHR_LEN
      LOGICAL 			CHR_ISALM
*
*    Local variables :
*
      CHARACTER*80 MODSPEC                      ! Model specification as typed
      CHARACTER*80 GENUS                        ! Model genus
      CHARACTER*80 POLISH                       ! Model spec in reverse Polish
      CHARACTER*(MAXKEYLEN) MENU(MAXIMP)        ! Menu of available pmodel keys
      CHARACTER*(MAXKEYLEN) MODL(MAXCOMP)       ! Keywords of pmodel in cmodel
      CHARACTER*1 SYM                           ! Characters in cmodel string

      INTEGER NCIMP                             ! No.of possible pmodels
      INTEGER NMCOMP                            ! No.of pmodels in cmodel
      INTEGER NOP                               ! No.of operations (eg.+-*/)
      INTEGER LSTR                              ! Length of cmodel string
      INTEGER NLB,NRB                           ! No.of left and right brackets
      INTEGER I,J,BEG                           ! Counters

      LOGICAL THERE                             ! .TRUE. if pmodel found
      LOGICAL LOOP                              ! .TRUE. if end of cmodel
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Delete old model if found :
      CALL DAT_ASSOC( 'FIT_MOD', 'WRITE', FLOC, STATUS )
      CALL DAT_THERE( FLOC, 'PMODEL', THERE, STATUS )
      IF ( THERE ) THEN
        CALL DAT_ERASE( FLOC, 'PMODEL', STATUS )
      END IF
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Get genus from user
      CALL PAR_DEF0C( 'GENUS', 'SPEC', STATUS )
      CALL PAR_GET0C( 'GENUS', GENUS, STATUS )
      CALL PAR_CANCL( 'GENUS', STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Display menu and enter new model
      IF ( SCREEN ) THEN
        CALL TSM_ERASE( W_BIG, STATUS )
        CALL SEDIT_FIT_MENU( GENUS, NCIMP, MENU, STATUS )
        CALL TSM_PUTSTRAT( W_BIG, 'Model specification > ', 2,
     :                                         SNY-6, STATUS )
        CALL TSM_REFRESH( W_BIG, STATUS )
        CALL TSM_RDSTR( W_BIG, MODSPEC, STATUS )
      ELSE
        CALL FIT_MENU( GENUS, NCIMP, MENU, STATUS )
        CALL PAR_GET0C('MODEL_SPEC',MODSPEC,STATUS)
        CALL PAR_CANCL('MODEL_SPEC',STATUS)
      END IF
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Extract model components and check them :
      NLB=0
      NRB=0
      NMCOMP=0
      NOP=0
      LSTR=CHR_LEN(MODSPEC)
      I=0
      DO WHILE (I.LT.LSTR)
         I=I+1
         SYM=MODSPEC(I:I)
         IF (SYM.EQ.'(') THEN
            NLB=NLB+1
         ELSE IF (SYM.EQ.')') THEN
            NRB=NRB+1
         ELSE IF (SYM.EQ.'+'.OR.SYM.EQ.'-'.OR.SYM.EQ.'*') THEN
            NOP=NOP+1
         ELSE IF ( CHR_ISALM(SYM) ) THEN

*          Scan to end of word
            BEG = I
            DO WHILE ( (I.LE.LSTR) .AND. CHR_ISALM(MODSPEC(I:I)) )
              I = I + 1
            END DO
            I = I - 1

*          Convert to upper case
            CALL CHR_UCASE( MODSPEC(BEG:I) )

	    NMCOMP=NMCOMP+1
	    MODL(NMCOMP)=MODSPEC(BEG:I)

*          Check such a model exists
            LOOP=.FALSE.
            DO  J=1,NCIMP
	       IF (MODL(NMCOMP).EQ.MENU(J)) LOOP=.TRUE.
	    END DO
            IF (.NOT.LOOP) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('UMOD',MODL(NMCOMP))
	       CALL ERR_REP('UNREC_MOD','Unrecognised model component ^UMOD',
     :         STATUS)
	       CALL ERR_FLUSH(STATUS)
	       CALL PAR_CANCL('MODEL_SPEC',STATUS)
               GOTO 99
            END IF
         END IF
      END DO

*    Perform simple syntax check :
      IF (NLB.NE.NRB.OR.NMCOMP.NE.NOP+1) THEN
        CALL ERR_REP('BAD_SYN','Bad model syntax',STATUS)
        CALL ERR_FLUSH(STATUS)
        CALL PAR_CANCL('MODEL_SPEC',STATUS)
        GOTO 99
      END IF

*    Translate model to reverse polish :
      CALL FIT_POLTRAN(MODSPEC,POLISH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Write components to fit_model object :
      CALL DAT_NEW(FLOC,'PMODEL','PRIM_MODEL',1,NMCOMP,STATUS)
      CALL DAT_ASSOC('FIT_MOD','UPDATE',FLOC,STATUS)
      CALL CMP_PUT0C(FLOC,'SPEC',MODSPEC,STATUS)
      CALL CMP_PUT0C(FLOC,'POLISH',POLISH,STATUS)
      CALL CMP_PUT0I(FLOC,'NCOMP',NMCOMP,STATUS)

*    Call parset to set up default parameters for the model components :
      CALL FIT_PARSET( GENUS, NMCOMP, MODL, FLOC, STATUS )

*    Get parameters from user :
      IF ( .NOT. SCREEN ) THEN
        CALL MSG_PRNT ('Enter new model parameters:-')
        CALL SEDIT_NEWPAR( FLOC, STATUS )
        IF (STATUS.EQ.PAR__NULL) CALL ERR_ANNUL( STATUS )
      END IF

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SEDIT_NEWMOD', STATUS )
      END IF

      END


*+
      SUBROUTINE SEDIT_ALTERPAR(FLOC,PARTOT,COMP,PAR,STATUS)
*    Description :
*     Allows alteration of VAL, LOW and HI for individual parameters.
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) FLOC		! Locator to fit_model object
      INTEGER PARTOT                            ! Total no. of pmodels
      INTEGER COMP(NPAMAX)                      ! Component of ith pmodel
      INTEGER PAR(NPAMAX)                       ! Parameter of ith pmodel
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*(DAT__SZLOC) MLOC               ! Locator to pmodels
      CHARACTER*(DAT__SZLOC) MILOC              ! Locator to pmodel i
      CHARACTER*(DAT__SZLOC) MIPLOC             ! Locator to parameters of
                                                !  pmodel i
      CHARACTER*(DAT__SZLOC) MIPJLOC            ! Locator to parameter j of
                                                !  pmodel i
      INTEGER PARNO                             ! Number of parameter to alter
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get parameter number and locate it :
      CALL PAR_GET0I('PAR',PARNO,STATUS)
      CALL PAR_CANCL('PAR',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 99
      IF ( (PARNO.GT.PARTOT) .OR. (PARNO.LT.1) ) THEN
        CALL MSG_SETI ('NUM',PARNO)
        CALL MSG_PRNT ('Illegal parameter number - ^NUM')
        GOTO 99
      END IF
      CALL DAT_FIND(FLOC,'PMODEL',MLOC,STATUS)
      CALL DAT_CELL(MLOC,1,COMP(PARNO),MILOC,STATUS)
      CALL DAT_FIND(MILOC,'PAR',MIPLOC,STATUS)
      CALL DAT_CELL(MIPLOC,1,PAR(PARNO),MIPJLOC,STATUS)

*    Input new values :
      CALL SEDIT_DECODE(MIPJLOC,STATUS)

*    Annul locators and exit :
      CALL DAT_ANNUL(MLOC,STATUS)
      CALL DAT_ANNUL(MILOC,STATUS)
      CALL DAT_ANNUL(MIPLOC,STATUS)
      CALL DAT_ANNUL(MIPJLOC,STATUS)

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SEDIT_ALTERPAR', STATUS )
      END IF

      END


*+
      SUBROUTINE SEDIT_NEWPAR(FLOC,STATUS)
*    Description  :
*     Allows modification of all parameters
*    Type definition :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*    Function declarations :
      INTEGER CHR_LEN
*    Import :
      CHARACTER*(DAT__SZLOC) FLOC               ! Locator to fit_model object
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*(DAT__SZLOC) MLOC               ! Locator to pmodels
      CHARACTER*(DAT__SZLOC) MILOC              ! Locator to pmodel i
      CHARACTER*(DAT__SZLOC) MIPLOC             ! Locator to components of
                                                !  pmodel i
      CHARACTER*(DAT__SZLOC) MIPJLOC            ! Locator to component j of
                                                !  pmodel i
      CHARACTER*80 PARNAME                      ! Name of parameter
      CHARACTER*80 UNITS,OUNITS                 ! Units of parameter
      CHARACTER*3 FROZ                          ! Frozen inicator string
      CHARACTER*(MAXKEYLEN) KEY                 ! pmodel key

      LOGICAL FROZEN                            ! .TRUE. if parameter frozen

      INTEGER NMCOMP                             ! No.of components
      INTEGER NPAR                              ! No.of parameters
      INTEGER COMPNO                            ! Current component no.
      INTEGER PARNO                             ! Current parameter no.
      INTEGER K                                 ! Counter

      REAL VAL,LOW,HI                           ! Current parameter values
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get no. of components in cmodel :
      CALL CMP_GET0I(FLOC,'NCOMP',NMCOMP,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Locate pmodels :
      CALL DAT_FIND(FLOC,'PMODEL',MLOC,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Loop through each component of pmodels :
      DO COMPNO=1,NMCOMP

        CALL DAT_CELL(MLOC,1,COMPNO,MILOC,STATUS)
        CALL CMP_GET0I(MILOC,'NPAR',NPAR,STATUS)
        CALL CMP_GET0C(MILOC,'KEY',KEY,STATUS)
        CALL DAT_FIND(MILOC,'PAR',MIPLOC,STATUS)
        IF (STATUS.NE.SAI__OK) GOTO 99

*      Type out component key :
        CALL MSG_SETI ('COMPNO',COMPNO)
        CALL MSG_SETC ('KEY',KEY)
        CALL MSG_PRNT ('Component ^COMPNO: ^KEY')

*      Loop through each parameter of current component :
        DO PARNO=1,NPAR
          CALL DAT_CELL(MIPLOC,1,PARNO,MIPJLOC,STATUS)
          CALL CMP_GET0C(MIPJLOC,'NAME',PARNAME,STATUS)
          CALL CMP_GET0C(MIPJLOC,'UNITS',UNITS,STATUS)
          CALL CMP_GET0L(MIPJLOC,'FROZEN',FROZEN,STATUS)
          CALL CMP_GET0R(MIPJLOC,'VAL',VAL,STATUS)
          CALL CMP_GET0R(MIPJLOC,'LOW',LOW,STATUS)
          CALL CMP_GET0R(MIPJLOC,'HI',HI,STATUS)
          IF (STATUS.NE.SAI__OK) GOTO 99
          IF (FROZEN) THEN
            FROZ=' F '
          ELSE
            FROZ='   '
          END IF
	  K=CHR_LEN(UNITS)
	  IF (K.GT.0) THEN
	    OUNITS='('//UNITS(1:K)//')'

*          Print out current parameter settings :
	    WRITE(6,200)PARNO,PARNAME,OUNITS,FROZ,VAL,LOW,HI
200	    FORMAT(3X,I3,') ',A25,2X,A25,A3,1PG12.5,4X,1PG12.5,
     :                                             '  ->',1PG12.5)
	  ELSE
	    WRITE(6,300)PARNO,PARNAME,FROZ,VAL,LOW,HI
300	    FORMAT(3X,I3,') ',A25,A3,1PG12.5,2X,1PG12.5,
     :                                      '  ->',1PG12.5)
          END IF

*        Get new parameter values from user :
          CALL SEDIT_DECODE(MIPJLOC,STATUS)
          IF (STATUS.NE.SAI__OK) GOTO 99

          CALL DAT_ANNUL(MIPJLOC,STATUS)

*      Next parameter :
        END DO

*      Next component :
        CALL DAT_ANNUL(MILOC,STATUS)
        CALL DAT_ANNUL(MIPLOC,STATUS)

      END DO

      CALL DAT_ANNUL(MLOC,STATUS)

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SEDIT_NEWPAR', STATUS )
      END IF

      END


*+  SEDIT_LISTPAR - List parameters to a logical unit
      SUBROUTINE SEDIT_LISTPAR( FLOC, PARTOT, COMP, PAR, OCH, STATUS )
*
*    Description :
*
*     Lists current parameter values and specifications.
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'TSM_PAR'
*
*    Global variables :
*
      INCLUDE 'SPECLIB(SEDIT_CMN)'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) FLOC               ! Locator to fit_model object
      INTEGER PARTOT                            ! Total number of parameters
      INTEGER COMP(NPAMAX)                      ! Component of pmodel i
      INTEGER PAR(NPAMAX)                       ! Parameter of pmodel i
      INTEGER			OCH			! Output channel id
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER CHR_LEN
*
*    Local varaiables :
*
      CHARACTER*(DAT__SZLOC) MLOC		! Locator to pmodels
      CHARACTER*(DAT__SZLOC) MILOC		! Locator to pmodel i
      CHARACTER*(DAT__SZLOC) MIPLOC		! Locator to parameters of
                                                !  pmodel i
      CHARACTER*(DAT__SZLOC) MIPJLOC		! Locator to parameter j of
                                                !  pmodel i
      CHARACTER*130 LINE
      CHARACTER*80 MODSPEC			! Cmodel specification string
      CHARACTER*(MAXKEYLEN)  KEY                ! Pmodel key
      CHARACTER*3  FROZ                         ! Frozen character string
      CHARACTER*80 PARNAME, UNITS, OUNITS	! Parameter name
      CHARACTER*50 VSTR
      CHARACTER*20  PFMT

      REAL VAL,LOW,HI                           ! Current parameter values

      INTEGER MNPAR				! No of pmodel parameters
      INTEGER NMCOMP				! No of pmodels in cmodel
      INTEGER I,J,K                             ! Counters
      INTEGER IL                                ! Current line number

      LOGICAL ETHERE				! Errors there
      LOGICAL FROZEN				! .TRUE. if parameter frozen
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Display existing model :
      CALL CMP_GET0C(FLOC,'SPEC',MODSPEC,STATUS)
      IF (STATUS.NE.SAI__OK) THEN
        CALL ERR_REP( ' ', 'Dataset contains no model specification',
     :                                                       STATUS )
        GOTO 99
      END IF

      IF ( SCREEN ) THEN
        CALL TSM_ERASE( W_BIG, STATUS )
        CALL MSG_SETC( 'MODEL', MODSPEC )
        CALL TSM_PUTMSGAT( W_STATUS, 'Model is : ^MODEL',
     :                                    85, 1, STATUS )
        CALL TSM_PUTSSTRAT( W_BIG, 'Component/Parameter          '/
     :                  /'   Units                          Value'/
     :                  /'                    Range',
     :                     TSM__BOLD, 2, 1, STATUS )
        CALL TSM_DRAWLINE( W_BIG, 1, SNY-9, SNX-2, SNY-9, STATUS )
        CALL TSM_PUTSTRAT( W_BIG, '[H]ELP              [R]efresh'/
     :             /' screen           E[X]it', 1, SNY-8, STATUS )
        CALL TSM_REFRESH( W_STATUS, STATUS )

        IL = 0
      ELSE IF ( OCH .GT. 0 ) THEN
        CALL AIO_BLNK( OCH, STATUS )
        CALL MSG_SETC( 'MODEL', MODSPEC )
        CALL AIO_WRITE( OCH, 'Existing model is: ^MODEL', STATUS )
      END IF

      CALL CMP_GET0I(FLOC,'NCOMP',NMCOMP,STATUS)
      CALL DAT_FIND(FLOC,'PMODEL',MLOC,STATUS)
      IF (STATUS.NE.SAI__OK) THEN
        CALL ERR_REP( ' ', 'Primitive models not found in fit_model '
     :                                           //'object', STATUS )
        GOTO 99
      END IF

*    Step through model components :
      PARTOT=0
      DO I=1,NMCOMP
         CALL DAT_CELL(MLOC,1,I,MILOC,STATUS)
         IF (STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
         CALL CMP_GET0I(MILOC,'NPAR',MNPAR,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 99

*       Write component id
         CALL CMP_GET0C( MILOC, 'KEY', KEY, STATUS )
         WRITE( LINE, '(A,I1,2A)' ) 'Component ',I,' - ',KEY
         IF ( SCREEN ) THEN
           IL = IL + 1
           DLINE(IL) = LINE
           FLINE(I) = IL
           NPAR(I) = MNPAR
         ELSE IF ( OCH .GT. 0 ) THEN
           CALL AIO_WRITE( OCH, LINE, STATUS )
         END IF

*    Step through the parameters of the model component :
         CALL DAT_FIND(MILOC,'PAR',MIPLOC,STATUS)
         DO J=1,MNPAR
            PARTOT=PARTOT+1
            COMP(PARTOT)=I
            PAR(PARTOT)=J
            CALL DAT_CELL(MIPLOC,1,J,MIPJLOC,STATUS)
	    CALL CMP_GET0C(MIPJLOC,'NAME',PARNAME,STATUS)
	    CALL CMP_GET0C(MIPJLOC,'UNITS',UNITS,STATUS)
	    CALL CMP_GET0L(MIPJLOC,'FROZEN',FROZEN,STATUS)
	    CALL CMP_GET0R(MIPJLOC,'VAL',VAL,STATUS)
	    CALL CMP_GET0R(MIPJLOC,'LOW',LOW,STATUS)
	    CALL CMP_GET0R(MIPJLOC,'HI',HI,STATUS)
	    IF (STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
            CALL DAT_THERE(MIPJLOC,'LERROR',ETHERE,STATUS)
            CALL DAT_ANNUL(MIPJLOC,STATUS)

*          Format to use for value printing
            IF ( (INDEX(PARNAME,'RA').GT.0) .OR.
     :           (INDEX(PARNAME,'DEC').GT.0) ) THEN
              PFMT = '2X,F8.4,2X'
            ELSE
              PFMT = '1PG12.5'
            END IF

            IF (FROZEN) THEN
               FROZ=' F '
            ELSE
               FROZ='   '
            END IF
            IF ( ETHERE ) FROZ = 'E'//FROZ(2:3)

	    K=CHR_LEN(UNITS)
            WRITE( VSTR, '('//PFMT//',4X,'//PFMT//',A4,'//PFMT//')' )
     :                                            VAL, LOW, ' -> ', HI
	    IF (K.GT.0) THEN
	      OUNITS='('//UNITS(1:K)//')'
	      WRITE(LINE,200)PARTOT,PARNAME,OUNITS,FROZ,VSTR
 200	      FORMAT(I3,') ',A25,2X,A25,A3,A44 )
	    ELSE
	      WRITE(LINE,300)J,PARNAME,FROZ,VSTR
 300	      FORMAT(I3,') ',A25,A3,A44 )
	    END IF
            IF ( SCREEN ) THEN
              IL = IL + 1
	      DLINE(IL) = LINE
            ELSE IF ( OCH .GT. 0 ) THEN
              CALL AIO_IWRITE( OCH, 1, LINE, STATUS )
            END IF
         END DO
	 CALL DAT_ANNUL(MIPLOC,STATUS)

      END DO
      NLINE = IL

*    Ensure screen is up to date
      IF ( SCREEN ) THEN
        CALL TSM_REFRESH( W_BIG, STATUS )
      END IF

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SEDIT_LISTPAR', STATUS )
      END IF

      END


*+  SEDIT_UPDSCR - Update display in screen mode
      SUBROUTINE SEDIT_UPDSCR( STATUS )
*
*    Description :
*
*     Lists current parameter values and specifications.
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'TSM_PAR'
*
*    Global variables :
*
      INCLUDE 'SPECLIB(SEDIT_CMN)'
*
*    Status :
*
      INTEGER STATUS
*
*    Local varaiables :
*
      INTEGER I
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Wipe the display surface
      CALL TSM_ERASE( W_DISP, STATUS )

*    Output lines of text
      DO I = DSTART, MIN(DSTART+DNY-1,NLINE)
        CALL TSM_PUTSTRAT( W_DISP, DLINE(I), 1, I-DSTART+2, STATUS )
      END DO

*    Ensure visible version is up to date
      CALL TSM_REFRESH( W_DISP, STATUS )

      END



*+  SEDIT_CURSOR - Update display in screen mode
      SUBROUTINE SEDIT_CURSOR( ON, STATUS )
*
*    Description :
*
*     Lists current parameter values and specifications.
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'TSM_PAR'
*
*    Global variables :
*
      INCLUDE 'SPECLIB(SEDIT_CMN)'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      LOGICAL         ON
*
*    Local variables :
*
      INTEGER         CX, CY, I, IMOD,ASTYLE
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Cursor Y position
      CY = CLINE - DSTART + 2

*   Work out the cursor text and position
*    Cursor in last column?
      IF ( CCOL .EQ. 0 ) THEN

*      Set arrow style
        ASTYLE = TSM__BOLD + TSM__BLINK

      ELSE

*      Are we on a model component name line?
        IMOD = 0
        DO I = 1, NCOMP
          IF ( CLINE .EQ. FLINE(I) ) IMOD = I
        END DO

*      Set arrow style
        ASTYLE = TSM__BOLD

      END IF

*    Always display the arrow
      CX = DNX - 2
      IF ( ON ) THEN
        CALL TSM_PUTSSTRAT( W_DISP, '<<', ASTYLE,
     :                      CX, CY, STATUS )
      ELSE
        CALL TSM_PUTSTRAT( W_DISP, '  ', CX, CY, STATUS )
      END IF

*    Ensure visible version is up to date
      CALL TSM_REFRESH( W_DISP, STATUS )

      END



*+  SEDIT_SCRDISP - Ensure screen would contain new cursor position
      SUBROUTINE SEDIT_SCRDISP( NEWLINE, STATUS )
*
*    Description :
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'TSM_PAR'
*
*    Global variables :
*
      INCLUDE 'SPECLIB(SEDIT_CMN)'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER            NEWLINE
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Is the new position above the current top of display?
      IF ( NEWLINE .LT. DSTART ) THEN

*      Scroll window down
        CALL TSM_SCROLL( W_DISP, -1, STATUS )
        DSTART = DSTART - 1

*    Is it off the bottom
      ELSE IF ( NEWLINE .GT. (DSTART+DNY-1) ) THEN

*      Scroll window up
        CALL TSM_SCROLL( W_DISP, 1, STATUS )
        DSTART = DSTART + 1

      END IF

*    Ensure visible version is up to date
      CALL TSM_REFRESH( W_DISP, STATUS )

      END


*+  SEDIT_FR_OR_TH - Freeze or thaw selected parameters
      SUBROUTINE SEDIT_FR_OR_TH(FLOC,PARTOT,COMP,PAR,FREEZE,STATUS)
*
*    Description :
*
*     Allows parameters to be frozen or thawed
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) FLOC		! Locator to fit_model object
      INTEGER PARTOT                            ! Total no.of parameters
      INTEGER COMP(NPAMAX)                      ! Component of pmodel i
      INTEGER PAR(NPAMAX)                       ! Parameter of pmodel i
      LOGICAL FREEZE                            ! True to freeze, false to thaw
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) MLOC               ! Locator to pmodels
      CHARACTER*(DAT__SZLOC) MILOC              ! Locator to pmodel i
      CHARACTER*(DAT__SZLOC) MIPLOC             ! Locator to components of
                                                !  pmodel i
      CHARACTER*(DAT__SZLOC) MIPJLOC            ! Locator to component j of
                                                !  pmodel i
      INTEGER I                                 ! Parameter counter
      INTEGER N                                 ! Number of params to be frozen
      INTEGER IP(NPAMAX)                        ! Parameters to be frozen
                                                !  or thawed
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Ask user for parameters to be frozen or thawed:
      CALL PRS_GETLIST( 'PARS', PARTOT, IP, N, STATUS )
      IF (STATUS.NE.SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL PAR_CANCL ('PARS',STATUS)
        GOTO 99
      END IF
      CALL PAR_CANCL ('PARS',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Get locator to pmodels :
      CALL DAT_FIND(FLOC,'PMODEL',MLOC,STATUS)

*    Loop through required parameters, setting 'FROZEN'=FREEZE :
      DO I=1,N
         IF (IP(I).GT.PARTOT.OR.IP(I).LT.1) THEN
            CALL MSG_SETI ('IP',IP(I))
            CALL MSG_PRNT ('Illegal parameter number - ^IP')
         ELSE
            CALL DAT_CELL(MLOC,1,COMP(IP(I)),MILOC,STATUS)
            CALL DAT_FIND(MILOC,'PAR',MIPLOC,STATUS)
            CALL DAT_CELL(MIPLOC,1,PAR(IP(I)),MIPJLOC,STATUS)
            CALL CMP_PUT0L(MIPJLOC,'FROZEN',FREEZE,STATUS)
            CALL DAT_ANNUL(MILOC,STATUS)
            CALL DAT_ANNUL(MIPLOC,STATUS)
            CALL DAT_ANNUL(MIPJLOC,STATUS)
         END IF
      END DO

*    Annul locators and exit :
      CALL DAT_ANNUL(MLOC,STATUS)

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SEDIT_FR_OR_TH', STATUS )
      END IF

      END


*+  SEDIT_ERESET - Reset errors on selected parameters
      SUBROUTINE SEDIT_ERESET(FLOC,PARTOT,COMP,PAR,STATUS)
*
*    Description :
*
*     Allows parameters to be frozen or thawed
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) FLOC		! Locator to fit_model object
      INTEGER PARTOT                            ! Total no.of parameters
      INTEGER COMP(NPAMAX)                      ! Component of pmodel i
      INTEGER PAR(NPAMAX)                       ! Parameter of pmodel i
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) MLOC               ! Locator to pmodels
      CHARACTER*(DAT__SZLOC) MILOC              ! Locator to pmodel i
      CHARACTER*(DAT__SZLOC) MIPLOC             ! Locator to components of
                                                !  pmodel i
      CHARACTER*(DAT__SZLOC) MIPJLOC            ! Locator to component j of
                                                !  pmodel i
      INTEGER I                                 ! Parameter counter
      INTEGER N                                 ! Number of params to be frozen
      INTEGER IP(NPAMAX)                        ! Parameters to be frozen
                                                !  or thawed

      LOGICAL			THERE			! Component exists?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Ask user for parameters to have errors reset
      CALL PRS_GETLIST( 'PARS', PARTOT, IP, N, STATUS )
      IF (STATUS.NE.SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL PAR_CANCL ('PARS',STATUS)
        GOTO 99
      END IF
      CALL PAR_CANCL ('PARS',STATUS)

*    Get locator to pmodels :
      CALL DAT_FIND(FLOC,'PMODEL',MLOC,STATUS)

*    Loop through required parameters, setting 'FROZEN'=FREEZE :
      DO I=1,N
        IF (IP(I).GT.PARTOT.OR.IP(I).LT.1) THEN
          CALL MSG_SETI ('IP',IP(I))
          CALL MSG_PRNT ('Illegal parameter number - ^IP')
        ELSE
          CALL DAT_CELL(MLOC,1,COMP(IP(I)),MILOC,STATUS)
          CALL DAT_FIND(MILOC,'PAR',MIPLOC,STATUS)
          CALL DAT_CELL(MIPLOC,1,PAR(IP(I)),MIPJLOC,STATUS)
          CALL DAT_THERE( MIPJLOC, 'LERROR', THERE, STATUS )
          IF ( THERE ) THEN
            CALL DAT_ERASE( MIPJLOC, 'LERROR', STATUS )
          END IF
          CALL DAT_THERE( MIPJLOC, 'UERROR', THERE, STATUS )
          IF ( THERE ) THEN
            CALL DAT_ERASE( MIPJLOC, 'UERROR', STATUS )
          END IF
          CALL DAT_ANNUL(MIPJLOC,STATUS)
          CALL DAT_ANNUL(MIPLOC,STATUS)
          CALL DAT_ANNUL(MILOC,STATUS)
        END IF
      END DO

*    Annul locators and exit :
      CALL DAT_ANNUL(MLOC,STATUS)

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SEDIT_ERESET', STATUS )
      END IF

      END



*+  SEDIT_DECODE
      SUBROUTINE SEDIT_DECODE( MIPJLOC, STATUS )
*
*    Description :
*
*     Decodes input character string into a list of values, and writes these
*     values to the current parameter pointed to by MIPJLOC
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) MIPJLOC            ! Locator to component j of
                                                !  pmodel i
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER CHR_LEN
*
*    Local variables :
*
      CHARACTER*80 STRING                       ! Input string
      CHARACTER*20  SUBSTR                      ! Substring
      CHARACTER*1   SYM                         ! Current character in STRING

      DOUBLE PRECISION DUMMY,DPAR		!

      REAL PARVAL                               ! Input parameter value i
      REAL VAL                                  ! Parameter value
      REAL LOW                                  ! Parameter min. value
      REAL HI                                   ! Parameter max. value

      INTEGER NUM                               ! Number of input values
      INTEGER LS                                ! Length of STRING
      INTEGER LSUB                              ! Length of substring
      INTEGER K                                 ! Counter

      LOGICAL OK                                ! .TRUE. if LOW<VAL<HI
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      OK=.FALSE.

*    Loop round until OK is not true
      DO WHILE ( .NOT. OK )

*      Get input string :
        CALL PAR_DEF0C('VALUES','none',STATUS)
        CALL PAR_GET0C('VALUES',STRING,STATUS)
        CALL PAR_CANCL('VALUES',STATUS)
        IF (STATUS.NE.SAI__OK) GOTO 99

*      Check for default values taken :
        NUM=0
        CALL CHR_LDBLK( STRING )
        CALL CHR_UCASE(STRING)
        LS=CHR_LEN(STRING)
        IF (STRING.NE.'NONE'.AND.LS.NE.0) THEN

*    Cut STRING up into substrings separated by commas or spaces :
            LSUB=0
            DO K=1,LS
               SYM=STRING(K:K)

*    If not the end of the substring, add current character to the substring :
               IF (SYM.NE.' '.AND.SYM.NE.',')THEN
                  IF (LSUB.EQ.0) THEN
                     SUBSTR=SYM
                  ELSE
                     SUBSTR=SUBSTR(1:LSUB)//SYM
                  END IF
                  LSUB=LSUB+1
               END IF

*    Check for end of substring :
               IF (SYM.EQ.' '.OR.SYM.EQ.','.OR.K.EQ.LS) THEN

*    End of substring, so increment no. of inputs, and form into REAL variable :
                  NUM=NUM+1
                  IF (LSUB.NE.0) THEN


*            Trap user entering HMS or DMS
              IF ( (INDEX(SUBSTR(1:LSUB),'h') .NE. 0 ) .OR.
     :             (INDEX(SUBSTR(1:LSUB),'H') .NE. 0 ) ) THEN
                CALL CONV_RADEC( SUBSTR(1:LSUB), '0.0', DPAR, DUMMY,
     :                           STATUS )
                PARVAL = DPAR

              ELSE IF ( (INDEX(SUBSTR(1:LSUB),'d') .NE. 0 ) .OR.
     :                  (INDEX(SUBSTR(1:LSUB),'D') .NE. 0 ) ) THEN
                CALL CONV_RADEC( '0.0', SUBSTR(1:LSUB), DUMMY, DPAR,
     :                           STATUS )
                PARVAL = DPAR

              ELSE
                CALL CHR_CTOR( SUBSTR(1:LSUB), PARVAL, STATUS )
              END IF

*    Write current value to appropriate place in pmodel component :
                     IF (NUM.EQ.1)THEN
                        CALL CMP_PUT0R(MIPJLOC,'VAL',PARVAL,STATUS)
                     ELSE IF (NUM.EQ.2)THEN
                        CALL CMP_PUT0R(MIPJLOC,'LOW',PARVAL,STATUS)
                     ELSE IF (NUM.EQ.3)THEN
                        CALL CMP_PUT0R(MIPJLOC,'HI',PARVAL,STATUS)
                     END IF
                     IF (STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
                     LSUB=0
                  END IF

               END IF

*    Get next character from STRING :
            END DO

         END IF

*    Check that LOW < VAL < HI :
         CALL CMP_GET0R(MIPJLOC,'VAL',VAL,STATUS)
         CALL CMP_GET0R(MIPJLOC,'LOW',LOW,STATUS)
         CALL CMP_GET0R(MIPJLOC,'HI',HI,STATUS)
         OK=.TRUE.
         IF (LOW.GT.HI) THEN
            CALL MSG_SETR ('LOW',LOW)
            CALL MSG_SETR ('HI',HI)
            CALL MSG_PRNT ('Lower bound (^LOW) > upper bound (^HI)'/
     :                                           /' - Try again !!')
            OK=.FALSE.
         ELSE IF(LOW.GT.VAL) THEN
            CALL MSG_SETR ('LOW',LOW)
            CALL MSG_SETR ('VAL',VAL)
            CALL MSG_PRNT ('Lower bound (^LOW) > parameter value '/
     :                                    /'(^VAL) - Try again !!')
            OK=.FALSE.
         ELSE IF(HI.LT.VAL) THEN
            CALL MSG_SETR ('HI',HI)
            CALL MSG_SETR ('VAL',VAL)
            CALL MSG_PRNT ('Parameter value (^VAL) > upper bound '/
     :                                     /'(^HI) - Try again !!')
            OK=.FALSE.
         END IF

*    Get input again if LOW, VAL, HI are not in order :
      END DO

*    Exit
 99   IF ( STATUS.NE.SAI__OK ) THEN
        CALL AST_REXIT( 'SEDIT_DECODE', STATUS )
      END IF

      END



*+  SEDIT_FIT_MENU - Displays menu of available primitive spectral models
      SUBROUTINE SEDIT_FIT_MENU( GENUS, NCIMP, MENU, STATUS )
*    Description :
*     Retrieves keywords and names of the supported primitive models from model
*     menu file (e.g. SPEC_MENU) and displays these as a menu on the user's
*     terminal.
*     No.of models implemented and an array of the model keywords are returned
*     to the calling routine.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*
*      4 Aug 93 : Taken from FIT_MENU (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'TSM_PAR'
*
*    Global variables :
*
      INCLUDE 'SPECLIB(SEDIT_CMN)'
*
*    Import :
*
      CHARACTER*(*) GENUS			! Model genus
*
*    Export :
*
      INTEGER       NCIMP			! No of models supported
      CHARACTER*(*) MENU(*)			! Menu of model keys
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
	CHARACTER*80 LINE			! Line from menu file
	CHARACTER*80 STRING			! String from LINE
	CHARACTER*(MAXKEYLEN) KEY		! Model keyword
	CHARACTER*40 NAME			! Model name
	CHARACTER*14 TYPE			! Model type

      INTEGER         IL                        ! Line for printing
      INTEGER         MFD                       ! Menu file descriptor
      INTEGER         N
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Access menu file
      CALL FIT_MEN_OPEN( GENUS, MFD, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      NCIMP = 0
      IL = 3

*    Write menu prologue
      CALL TSM_PUTSTRAT( W_BIG, ' A composite model can be '/
     :        /'synthesised using + - * ( ) and any of the '/
     :         /'following primitive models:', 1, 1, STATUS )

*    Read line from menu file, leave loop if end of menu
      DO WHILE ( STATUS .EQ. SAI__OK )
 10	CALL AIO_READF( MFD, LINE, STATUS )
	IF(INDEX(LINE,'endmenu').GT.0) GO TO 99	! Jump to exit

*      Look for 'model' header
	IF(INDEX(LINE,'  model').EQ.0) GO TO 10		! Read next line

*      Header found - read model key, name & type
	CALL AIO_READF( MFD, LINE, STATUS )
	N=INDEX(LINE,'key:')
	IF(N.EQ.0)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('BADMNU','Error in menu file',STATUS)
	  GO TO 99
	ENDIF

*      Strip out keyword
	STRING=LINE(N+4:80)
	CALL CHR_LDBLK(STRING)
	IF(STATUS.NE.SAI__OK) GO TO 99
	KEY=STRING(1:)

	CALL AIO_READF( MFD, LINE, STATUS )
	N=INDEX(LINE,'mname:')
	IF(N.EQ.0)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('BADMNU','Error in SPECFIT.MNU',STATUS)
	  GO TO 99
	ENDIF

*      Strip out model name
	STRING=LINE(N+6:80)
	CALL CHR_LDBLK(STRING)
	IF(STATUS.NE.SAI__OK) GO TO 99
	NAME=STRING(1:40)

	CALL AIO_READF( MFD, LINE, STATUS )
	N=INDEX(LINE,'type:')
	IF(N.EQ.0)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('BADMNU','Error in menu file',STATUS)
	  GO TO 99
	ENDIF

*      Strip out model type
	STRING=LINE(N+5:80)
	CALL CHR_LDBLK(STRING)
	IF(STATUS.NE.SAI__OK) GO TO 99
	TYPE=STRING(1:14)

*      If model's type is properly defined then add it to menu
	IF(TYPE.EQ.'additive'.OR.TYPE.EQ.'multiplicative')THEN
	  NCIMP=NCIMP+1
	  MENU(NCIMP)=KEY
	  WRITE(LINE,50)KEY,NAME,TYPE
 50	  FORMAT(4X,A4,3X,A40,5X,A14)
          CALL TSM_PUTSTRAT( W_BIG, LINE, 2, IL, STATUS )
          IL = IL + 1
	ENDIF

      END DO

*    Close menu file
 99   CALL FIT_MEN_CLOSE( MFD, STATUS )

      END
