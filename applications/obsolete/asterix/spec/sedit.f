*+  SEDIT - Displays fit model parameter values
      SUBROUTINE SEDIT( STATUS )
*
*    Description :
*
*     Allows creation and editing of FIT_MODEL structures
*
*    Environment parameters :
*
*     MODEL=UNIV(U)
*		Object containing model (existing or not)
*     OVERRIDE=LOGICAL(R)
*		Overwrite existing model specification?
*     MODEL_SPEC=LITERAL(R)
*		String containing specification of composite model
*     VALUES=LITERAL(R)
*               String containing parameter value/limits
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
*     18 May 89 : V1.0   Modified from FIT_MODEL (BHVAD::TJP)
*      3 Jun 89 : V1.1   General tidying-up (PAK)
*     23 Apr 91 : V2.0   Version for ASTERIX88 (PAK)
*     23 Oct 92 : V1.7-0 Moved into ASTERIX (DJA)
*     25 Oct 92 : V1.7-1 SEDIT_LISTPAR changed to make it usable
*                        from SSHOW (DJA)
*     23 Aug 93 : V1.7-2 Screen mode added, handles model keys of any
*                        length. (DJA)
*     15 Apr 94 : V1.7-3 Reset errors added (DJA)
*      2 Aug 94 : V1.7-4 Allow Asterix list notation for parameter
*                        selection. (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     14 Dec 94 : V1.8-1 Display ties (DJA)
*     24 Apr 95 : V1.8-2 Use ADI to store locators (DJA)
*     11 Jan 1996 : V1.8-3 TSM removed (DJA)
*
*    Type Definitions :
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
*    Global variables :
*
      INCLUDE 'SEDIT_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      LOGICAL			CHR_SIMLR
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) 	FLOC             	! Locator to fit_model object
      CHARACTER*20           	NAME             	! Name of input file
      CHARACTER*(MAXKEYLEN)  	OPTION           	! Option character
      CHARACTER*(DAT__SZTYP) 	TYP              	! Object type

      INTEGER                	COMP(NPAMAX)     	! List of components
      INTEGER			FID			! Fit model dataset id
      INTEGER                	IOPT             	! Integer key code
      INTEGER                	OCH              	! Output channel id
      INTEGER                	PAR(NPAMAX)      	! List of parameters
      INTEGER                	PARTOT           	! Total number of parameters
      INTEGER			WIDTH			! Width of o/p channel

      LOGICAL                	QUIT             	! Quit program?
      LOGICAL                	SNEW             	! New fit_model object?
*
*    Version :
*
      CHARACTER*30         	VERSION            	! Version id
        PARAMETER               ( VERSION = 'SEDIT Version 1.8-3' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Start ASTERIX
      CALL AST_INIT()

*    Version
      CALL MSG_PRNT( VERSION )

*    Form or retrieve fit_model object
      CALL USI_DEXIST('MODEL','UPDATE',FLOC,STATUS)

      IF ( STATUS .EQ. PAR__ERROR ) THEN

*      Object doesn't exist - Create it
        CALL ERR_ANNUL(STATUS)
        CALL USI_TASSOCO( 'MODEL','FIT_MODEL',FID,STATUS)
        CALL ADI1_GETLOC( FID, FLOC, STATUS )
        CALL DAT_NEWC(FLOC,'SPEC',80,0,0,STATUS)
        CALL DAT_NEWC(FLOC,'POLISH',80,0,0,STATUS)
        CALL DAT_NEW(FLOC,'NCOMP','_INTEGER',0,0,STATUS)
        SNEW = .TRUE.

      ELSE IF ( STATUS .NE. PAR__NULL ) THEN

*      Object already exists, so check it :
        CALL DAT_TYPE(FLOC,TYP,STATUS)
        IF ( TYP .NE. 'FIT_MODEL' ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Not a fit_model data object', STATUS )
        END IF
        SNEW = .FALSE.
        CALL ADI1_PUTLOC( FLOC, FID, STATUS )

      END IF
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Declare file to user :
      CALL DISP_FILENAM( FID, 'Model', STATUS )

*    History file entry :
      CALL HSI_ADD( FID, VERSION, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    No existing model, so ask the user to input one :
      IF ( SNEW ) THEN
        CALL SEDIT_NEWMOD( FLOC, STATUS )
        CALL SEDIT_LISTPAR( FID, PARTOT, COMP, PAR, -1, STATUS )
      ELSE

*      Display component parameters
        CALL SEDIT_LISTPAR( FID, PARTOT, COMP, PAR, 6, STATUS )

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

         CALL USI_GET0C('OPTION',OPTION,STATUS)
         CALL USI_CANCL('OPTION',STATUS)
         IOPT = ICHAR(OPTION(1:1))

*       Bad response?
         IF ( STATUS .NE. SAI__OK ) THEN
           CALL ERR_ANNUL( STATUS )

*       Reset errors?
         ELSE IF ( CHR_SIMLR(OPTION,'RE') ) THEN
           CALL SEDIT_ERESET(FLOC,PARTOT,COMP,PAR,STATUS)

*       Alter parameters
         ELSE IF ( CHR_SIMLR(OPTION,'A') ) THEN
           CALL SEDIT_ALTERPAR(FLOC,PARTOT,COMP,PAR,STATUS)

*       Freeze parameters
         ELSE IF ( CHR_SIMLR(OPTION,'F') ) THEN
           CALL SEDIT_FR_OR_TH(FLOC,PARTOT,COMP,PAR,.TRUE.,STATUS)

*       Thaw parameters
         ELSE IF ( CHR_SIMLR(OPTION,'T') ) THEN
           CALL SEDIT_FR_OR_TH(FLOC,PARTOT,COMP,PAR,.FALSE.,STATUS)

*       Hardcopy of parameters
         ELSE IF ( CHR_SIMLR(OPTION,'P') ) THEN

*         Open the file
           CALL AIO_OPEN( 'PRINTER', 'LIST', OCH, WIDTH, STATUS )

*         Write model file description
           CALL DAT_NAME( FLOC, NAME, STATUS )
           CALL MSG_SETC( 'NAME', NAME )
           CALL AIO_WRITE( OCH, 'Input model file:- ^NAME', STATUS )

*         List parameters
           CALL SEDIT_LISTPAR( FID, PARTOT, COMP, PAR, OCH, STATUS )

*         Tidy up unit
           CALL AIO_CLOSE( OCH, STATUS )
           IF ( STATUS .NE. SAI__OK ) GOTO 99

*       List parameters to screen
         ELSE IF ( CHR_SIMLR(OPTION,'L') ) THEN
           CALL SEDIT_LISTPAR( FID, PARTOT, COMP, PAR, 6, STATUS )

*       New parameters
         ELSE IF ( CHR_SIMLR(OPTION,'NP') ) THEN
           CALL SEDIT_NEWPAR( FLOC, STATUS )

*       New model
         ELSE IF ( CHR_SIMLR(OPTION,'NM') ) THEN
           CALL SEDIT_NEWMOD( FLOC, STATUS )

*       Quit program?
         ELSE IF ( CHR_SIMLR(OPTION,'X') ) THEN
           QUIT = .TRUE.

         END IF

      END DO

*    Exit
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

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
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*
*    Global variables :
*
      INCLUDE 'SEDIT_CMN'
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
      CALL USI_DASSOC( 'MODEL', 'WRITE', FLOC, STATUS )
      CALL DAT_THERE( FLOC, 'PMODEL', THERE, STATUS )
      IF ( THERE ) THEN
        CALL DAT_ERASE( FLOC, 'PMODEL', STATUS )
      END IF
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Get genus from user
      CALL USI_DEF0C( 'GEN', 'SPEC', STATUS )
      CALL USI_GET0C( 'GEN', GENUS, STATUS )
      CALL USI_CANCL( 'GEN', STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Display menu and enter new model
      CALL FIT_MENU( GENUS, NCIMP, MENU, STATUS )
      CALL USI_GET0C('MODEL_SPEC',MODSPEC,STATUS)
      CALL USI_CANCL('MODEL_SPEC',STATUS)
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
	       CALL USI_CANCL('MODEL_SPEC',STATUS)
               GOTO 99
            END IF
         END IF
      END DO

*    Perform simple syntax check :
      IF (NLB.NE.NRB.OR.NMCOMP.NE.NOP+1) THEN
        CALL ERR_REP('BAD_SYN','Bad model syntax',STATUS)
        CALL ERR_FLUSH(STATUS)
        CALL USI_CANCL('MODEL_SPEC',STATUS)
        GOTO 99
      END IF

*    Translate model to reverse polish :
      CALL FIT_POLTRAN(MODSPEC,POLISH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 99

*    Write components to fit_model object :
      CALL DAT_NEW(FLOC,'PMODEL','PRIM_MODEL',1,NMCOMP,STATUS)
      CALL USI_DASSOC('MODEL','UPDATE',FLOC,STATUS)
      CALL CMP_PUT0C(FLOC,'SPEC',MODSPEC,STATUS)
      CALL CMP_PUT0C(FLOC,'POLISH',POLISH,STATUS)
      CALL CMP_PUT0I(FLOC,'NCOMP',NMCOMP,STATUS)

*    Call parset to set up default parameters for the model components :
      CALL FIT_PARSET( GENUS, NMCOMP, MODL, FLOC, STATUS )

*    Get parameters from user :
      CALL MSG_PRNT ('Enter new model parameters:-')
      CALL SEDIT_NEWPAR( FLOC, STATUS )
      IF (STATUS.EQ.PAR__NULL) CALL ERR_ANNUL( STATUS )

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
      CALL USI_GET0I('PAR',PARNO,STATUS)
      CALL USI_CANCL('PAR',STATUS)
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
      SUBROUTINE SEDIT_LISTPAR( FID, PARTOT, COMP, PAR, OCH, STATUS )
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
*
*    Global variables :
*
      INCLUDE 'SEDIT_CMN'
*
*    Import :
*
      INTEGER			FID			! Fit model dataset
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
      CHARACTER*(DAT__SZLOC) FLOC               ! Locator to fit_model object
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
      INTEGER NTIE				! Tied parameters
      INTEGER TSTART(MAXTIE)
      INTEGER TGROUP(NPAMAX)

      LOGICAL ETHERE				! Errors there
      LOGICAL FROZEN				! .TRUE. if parameter frozen
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Extract locator
      CALL ADI1_GETLOC( FID, FLOC, STATUS )

*    Display existing model :
      CALL CMP_GET0C(FLOC,'SPEC',MODSPEC,STATUS)
      IF (STATUS.NE.SAI__OK) THEN
        CALL ERR_REP( ' ', 'Dataset contains no model specification',
     :                                                       STATUS )
        GOTO 99
      END IF

      IF ( OCH .GT. 0 ) THEN
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

*    Get ties if present
      CALL FIT_MODGET_TIES( FLOC, NTIE, TSTART, TGROUP, STATUS )

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
         IF ( OCH .GT. 0 ) THEN
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
            IF ( (NTIE.GT.0) .AND. (TGROUP(PARTOT).GT.0) ) THEN
              IF ( PARTOT .NE. TSTART(TGROUP(PARTOT)) ) THEN
                FROZ(2:2) = 'C'
              END IF
            END IF
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
            IF ( OCH .GT. 0 ) THEN
              CALL AIO_IWRITE( OCH, 1, LINE, STATUS )
            END IF
         END DO
	 CALL DAT_ANNUL(MIPLOC,STATUS)

      END DO

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SEDIT_LISTPAR', STATUS )
      END IF

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
        CALL USI_CANCL ('PARS',STATUS)
        GOTO 99
      END IF
      CALL USI_CANCL ('PARS',STATUS)
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
        CALL USI_CANCL ('PARS',STATUS)
        GOTO 99
      END IF
      CALL USI_CANCL ('PARS',STATUS)

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
        CALL USI_DEF0C('VALUES','none',STATUS)
        CALL USI_GET0C('VALUES',STRING,STATUS)
        CALL USI_CANCL('VALUES',STATUS)
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
