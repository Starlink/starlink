*+  SMODEL - Sets up spectral model and associated parameter values
      SUBROUTINE SMODEL( STATUS )
*
*    Description :
*
*     Creates or updates a fit_model data object containing a composite
*     model (cmodel) specification and initial values and ranges for the
*     parameter set associated with each component primitive model (pmodel).
*     Pmodels all have keywords consisting of two alphanumeric characters and
*     may be of two basic types. Additive pmodels (e.g. BR, =bremmstrahlung)
*     have units of flux, whilst multiplicative models (e.g. AB, =absorption)
*     are dimensionless, and act to modify the additive model components.
*     Cmodels are specified using a staightforward mathematical syntax in
*     which brackets and + - and * operations are allowed, for example:
*		PL+AB*(BR-LI)
*     Parameter values and upper & lower bounds can be specified for each
*     model parameter, reasonable defaults being available from the menu
*     file.
*
*    Method :
*
*     The cmodel is checked by the program, and translated into a reverse
*     Polish string from which it is more readily computed.
*     All details of the pmodels are stored in the ASCII file <GENUS>_MENU,
*     so this must be extended (using the editor) when a new pmodel is added
*     to the menu available. Also the subroutine which actually calls the
*     pmodel subroutines ( FIT_MCALL ) will need appropriate extension.
*     The resulting cmodel structure definition and parameter values are
*     written into the HDS structured model object.
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
*     RESET=LOGICAL(R)
*               Reset parameters and limits bcak to original values
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*
*    History :
*
*     11 Aug 88 : V0.6-1  Original (TJP)
*     20 JUN 89 : V1.0-1  ASTERIX88 version (TJP)
*     18 Jan 85 :  Original (SMODEL)
*     12 Feb 85 :  FREEZE flag incorporated
*     25 Oct 85 :  Parameter i/o bugs fixed
*      1 Oct 86 :  V0.5-1 ADAM version
*     29 Jun 87 :  V0.6-1 (FMODEL) New fitting system - some changes to
*                  fit_model structure (TJP)
*      7 Jul 87 :  V0.6-2 VALUES parameter added (TJP)
*     11 Aug 88 :  Converted to subroutine FIT_MODEL (TJP)
*     23 Sep 88 :  Model file name displayed for user (TJP)
*     19 Jun 89 :  ASTERIX88 version, limited tidying of code (TJP)
*      1 Apr 92 : V1.0-2 FIT_MODEL merged into top level (RJV)
*     10 Nov 92 : V1.2-1 FIT_PARSET changed to read frozen flag from MENU (TJP)
*     13 Jan 93 : V1.7-0 RESET option added (DJA)
*      6 Aug 93 : V1.7-1 Added missing AST_INIT, removed direct i/o (DJA)
*     11 Jan 94 : V1.7-2 Added traps for HMS and DMS type input (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
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
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER CHR_LEN
      LOGICAL CHR_ISALF
      LOGICAL CHR_ISALM
*
*    Local constants :
*
      CHARACTER*4            GENUS              ! Fitting genus
        PARAMETER            ( GENUS = 'SPEC' )
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) FLOC		! Locator to fit_model object
      CHARACTER*(DAT__SZLOC) MLOC		! Locator to pmodels
      CHARACTER*(DAT__SZLOC) MILOC		! Locator to pmodel I
      CHARACTER*(DAT__SZLOC) MIPLOC		! Locator to parameters of
						! pmodel I
      CHARACTER*(DAT__SZLOC) MIPJLOC		! Locator to param J of pmod I
      CHARACTER*(DAT__SZTYP) TYP		! Object type
      CHARACTER*80 MODSPEC			! Cmodel specification string
      CHARACTER*80 POLISH			! Polish translation of cmodel
      CHARACTER*1 SYM				! Characters from cmodel string
      CHARACTER*(MAXKEYLEN) MODL(MAXCOMP)	! Keywords of pmodels in cmodel
      CHARACTER*(MAXKEYLEN) MENU(MAXIMP)	! Menu of available pmodelkeys
      CHARACTER*(MAXKEYLEN) KEY			! Pmodel key
      CHARACTER*132          LINE               ! Output line
      CHARACTER*60 MNAME			! Pmodel name
      CHARACTER*14 TYPE				! Pmodel type (mult/additive)
      CHARACTER*55 PARNAME			! Parameter name
      CHARACTER*30 UNITS			! Parameter units
      CHARACTER*32 OUNITS			! Units enclosed in brackets
      CHARACTER*60 STRING			! Input character string
      CHARACTER*20 SUBSTR			! Character substring

      DOUBLE PRECISION DPAR, DUMMY		! Celestial positions

      REAL HI					! Parameter ceiling value
      REAL LOW					! Parameter floor value
      REAL PARVAL				! New parameter value
      REAL VAL					! Current parameter value

      INTEGER BEG                               ! Start of a pmodel name
      INTEGER LSTR				! Length of cmodel spec string
      INTEGER NLB,NRB				! No of left & right brackets
      INTEGER NCIMP				! No of pmodels supported
      INTEGER NCOMP				! No of pmodels in cmodel
      INTEGER NOP				! No of operators
      INTEGER IKEY                              ! Number of KEY in MENU
      INTEGER NPAR				! No of pmodel parameters
      INTEGER LS				! Length of character string
      INTEGER LSUB				! Length of substring
      INTEGER NIN				! Number of input values
      INTEGER I,J,K

      LOGICAL FROZEN				! Parameter frozen
      LOGICAL LOG
      LOGICAL OVER				! Override existing cmodel?
      LOGICAL RESET 				! Reset pars and limits?
      LOGICAL SNEW				! New fit_model obj.created?
*
*    Version :
*
      CHARACTER*30 VERSION
	PARAMETER		(VERSION = 'SMODEL Version 1.8-0')
*-

*    Announce version
      CALL MSG_PRNT( VERSION )

*    Initialise ASTERIX
      CALL AST_INIT()

*    Create or retrieve FIT_MODEL
      RESET = .FALSE.
      CALL USI_DEXIST('FIT_MOD','UPDATE',FLOC,STATUS)
      IF ( STATUS .EQ. PAR__ERROR ) THEN
	CALL ERR_ANNUL(STATUS)
	CALL USI_ASSOCO('FIT_MOD','FIT_MODEL',FLOC,STATUS)
	SNEW=.TRUE.

      ELSE

*      Object already exists, so check it
	CALL DAT_TYPE(FLOC,TYP,STATUS)
	IF(TYP.NE.'FIT_MODEL')THEN
          STATUS=SAI__ERROR
	  CALL ERR_REP( ' ', 'Not a fit_model data object', STATUS )
	END IF
	SNEW=.FALSE.

*      Reset model?
        CALL USI_GET0L( 'RESET', RESET, STATUS )
	IF(STATUS.NE.SAI__OK) GOTO 99

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Declare file to user
      CALL DISP_FILENAM(FLOC,'Input model',STATUS)
      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)

* Existing model file accessed
	IF(.NOT.SNEW)THEN

*   Display model
	  CALL CMP_GET0C(FLOC,'SPEC',MODSPEC,STATUS)
	  IF(STATUS.NE.SAI__OK.OR.CHR_LEN(MODSPEC).EQ.0)THEN
	    CALL MSG_PRNT('Existing fit_model data object contains no'
     :      //' model')
	    OVER=.TRUE.
	  ELSE
            CALL MSG_BLNK()
            CALL MSG_PRNT( 'Existing model is :' )
            CALL MSG_BLNK()
            CALL MSG_PRNT( '  '//MODSPEC )
            CALL MSG_BLNK()

*   Model spec. to be overridden?
            IF ( .NOT. RESET ) THEN
	      CALL USI_GET0L('OVERRIDE',OVER,STATUS)
	      IF(STATUS.NE.SAI__OK) GOTO 99
            END IF
	  ENDIF

*     Delete the old pmodel array structure
	  IF(OVER.OR.RESET)THEN
	    CALL DAT_THERE(FLOC,'PMODEL',LOG,STATUS)
	    IF(LOG)THEN
	      CALL DAT_ERASE(FLOC,'PMODEL',STATUS)
	    ENDIF
	  ELSE
	    CALL CMP_GET0I(FLOC,'NCOMP',NCOMP,STATUS)

	    IF(STATUS.NE.SAI__OK) GOTO 99
	    GOTO 200		! Jump to parameter display/modification

	  ENDIF

	  IF(STATUS.NE.SAI__OK) GOTO 99
	ELSE

* No existing model, so create components
	  CALL DAT_NEWC(FLOC,'SPEC',80,0,0,STATUS)
	  CALL DAT_NEWC(FLOC,'POLISH',80,0,0,STATUS)
	  CALL DAT_NEW(FLOC,'NCOMP','_INTEGER',0,0,STATUS)
	ENDIF
	IF(STATUS.NE.SAI__OK) GOTO 99

*    Enter new model spec.
 30   IF ( .NOT. RESET ) THEN
        CALL FIT_MENU( GENUS, NCIMP, MENU, STATUS )
	CALL USI_GET0C( 'MODEL_SPEC', MODSPEC, STATUS )
      END IF
      IF(STATUS.NE.SAI__OK) GOTO 99

*    Extract component pmodel names and check them
      NLB=0
      NRB=0
      NCOMP=0
      NOP=0
      LSTR=CHR_LEN(MODSPEC)
      I=0

*    Loop over specification
      DO WHILE ( I .LE. LSTR )
	I=I+1					! 1st character of new pmodel
	SYM=MODSPEC(I:I)
	IF(SYM.EQ.'(')THEN
	  NLB=NLB+1
	ELSE IF ( SYM.EQ.')' ) THEN
	  NRB=NRB+1
	ELSE IF(SYM.EQ.'+'.OR.SYM.EQ.'-'.OR.SYM.EQ.'*')THEN
	  NOP=NOP+1

*      Alphabetic, must be start of pmodel
	ELSE IF( CHR_ISALF(SYM) ) THEN

*        Loop until non-alphanumeric or end of string found
          BEG = I
          DO WHILE ( CHR_ISALM(SYM) .OR.
     :               (SYM.EQ.'_') .AND. (I.LE.LSTR) )
            I = I + 1
	    SYM = MODSPEC(I:I)
          END DO

*        The loop above always leaves I one greater than the last valid
*        key name character position
          KEY = MODSPEC(BEG:I-1)
          IF ( (I-BEG) .GT. MAXKEYLEN ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Maximum model name length exceeded',
     :                                                      STATUS )
          END IF

*        Check this model key, unless RESET mode
          IF ( .NOT. RESET ) THEN
            CALL SFIT_LOCKEY( KEY, NCIMP, MENU, IKEY, STATUS )
          END IF

*        Status bad if this wasn't a good pmodel name
          IF ( STATUS .NE. SAI__OK ) THEN
	    CALL ERR_FLUSH( STATUS )
	    CALL USI_CANCL( 'MODEL_SPEC', STATUS )
            GOTO 30

*        Good pmodel syntax...
          ELSE

*          Coerce to upper case
            CALL CHR_UCASE( KEY )
            CALL CHR_UCASE( MODSPEC(BEG:I-1) )

*          Store the component
	    NCOMP = NCOMP + 1
	    MODL(NCOMP) = KEY
            I = I - 1

          END IF

        END IF

      END DO					! End of DO WHILE

*    Perform simple syntax check on cmodel
      IF(NLB.NE.NRB.OR.NCOMP.NE.NOP+1)THEN
        STATUS = SAI__ERROR
	CALL ERR_REP('BAD_SYN','Bad model syntax',STATUS)
	CALL ERR_FLUSH(STATUS)
	CALL USI_CANCL('MODEL_SPEC',STATUS)
	GOTO 30					! Allow reentry of model spec
      END IF

*    Translate pmodel algebra into reverse polish
      CALL FIT_POLTRAN( MODSPEC, POLISH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Write components to FIT_MODEL object
      CALL CMP_PUT0C(FLOC,'SPEC',MODSPEC,STATUS)
      CALL CMP_PUT0C(FLOC,'POLISH',POLISH,STATUS)
      CALL CMP_PUT0I(FLOC,'NCOMP',NCOMP,STATUS)
 190  CALL DAT_NEW(FLOC,'PMODEL','PRIM_MODEL',1,NCOMP,STATUS)

*    Call PARSET to set up defaults for parameter values and bounds
      CALL FIT_PARSET(GENUS,NCOMP,MODL,FLOC,STATUS)
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Display parameters and allow user modification
 200  CALL DAT_FIND(FLOC,'PMODEL',MLOC,STATUS)
      IF ( STATUS .NE. SAI__OK ) THEN
	CALL ERR_REP( ' ', 'Primitive models not found in fit_model '
     :                                           //'object', STATUS )
	GOTO 99
      END IF

*    Step through model components
      DO I=1,NCOMP
	CALL DAT_CELL(MLOC,1,I,MILOC,STATUS)
	CALL CMP_GET0C(MILOC,'KEY',KEY,STATUS)
	CALL CMP_GET0C(MILOC,'NAME',MNAME,STATUS)
	CALL CMP_GET0C(MILOC,'TYPE',TYPE,STATUS)
	IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
        CALL MSG_BLNK()
        CALL MSG_SETI( 'C', I )
        CALL MSG_PRNT( '                       COMPONENT ^C' )
        CALL MSG_PRNT( '                      **************' )
        CALL MSG_BLNK()
	WRITE(LINE,220) MNAME,KEY,TYPE(1:CHR_LEN(TYPE))

 220	FORMAT( 'Name: ',A40, ' Keyword: ', A, 5X, '[', A, ']' )
        CALL MSG_PRNT( LINE )

        CALL CMP_GET0I(MILOC,'NPAR',NPAR,STATUS)
	IF(STATUS.NE.SAI__OK) GOTO 99

*      Step through the parameters of the model component
	CALL DAT_FIND(MILOC,'PAR',MIPLOC,STATUS)
	DO J=1,NPAR
	  CALL DAT_CELL(MIPLOC,1,J,MIPJLOC,STATUS)
	  CALL CMP_GET0C(MIPJLOC,'NAME',PARNAME,STATUS)
	  CALL CMP_GET0C(MIPJLOC,'UNITS',UNITS,STATUS)
	  CALL CMP_GET0L(MIPJLOC,'FROZEN',FROZEN,STATUS)
	  IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
	  K=CHR_LEN(UNITS)
	  IF(K.GT.0)THEN
	    OUNITS='('//UNITS(1:K)//')'
	    WRITE(LINE,250)J,PARNAME,OUNITS
 250	    FORMAT(2X,'PARAMETER',I3,' - ',A25,2X,A32)
	  ELSE
	    WRITE(LINE,260)J,PARNAME
 260	    FORMAT(2X,'PARAMETER',I3,' - ',A25)
	  END IF
          CALL MSG_PRNT( LINE )
	  IF(FROZEN)THEN
            CALL MSG_PRNT( '                 *frozen*' )
	  ENDIF
	  CALL CMP_GET0R(MIPJLOC,'VAL',VAL,STATUS)
	  CALL CMP_GET0R(MIPJLOC,'LOW',LOW,STATUS)
	  CALL CMP_GET0R(MIPJLOC,'HI',HI,STATUS)
	  IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
	  WRITE(LINE,270)VAL,LOW,HI
 270	  FORMAT(17X,'value,min,max = ',3(1PG12.5,2X))
          CALL MSG_PRNT( LINE )

*        Get new values from user
	  CALL USI_DEF0C('VALUES','unchanged',STATUS)
	  CALL USI_GET0C('VALUES',STRING,STATUS)
	  IF ( STATUS.EQ.PAR__ABORT)THEN
	    GOTO 99
	  ELSE IF(STATUS.NE.SAI__OK)THEN
	    CALL ERR_ANNUL(STATUS)
	    CALL USI_CANCL('VALUES',STATUS)
	    GOTO 350						! Next parameter
	  END IF
	  CALL USI_CANCL( 'VALUES', STATUS )
	  LS = CHR_LEN(STRING)
	  IF(STRING.EQ.'unchanged'.OR.LS.EQ.0) GOTO 350	! Next parameter

*        Decode input string into numbers
	  LSUB=0
	  NIN=0
	  DO K=1,LS
	    SYM=STRING(K:K)
	    IF(SYM.EQ.' '.OR.SYM.EQ.','.OR.K.EQ.LS)THEN
	      IF(K.EQ.LS.AND.SYM.NE.',')THEN
	        IF(LSUB.EQ.0)THEN
	          SUBSTR=SYM
	        ELSE
	          SUBSTR=SUBSTR(1:LSUB)//SYM
	        ENDIF
	        LSUB=LSUB+1
	      ENDIF
              NIN=NIN+1
	      IF(NIN.GT.3) GOTO 350		! Next parameter
	      IF(LSUB.EQ.0) GOTO 300		! Next string symbol

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

*            Update parameter value
	      IF(NIN.EQ.1)THEN
	        CALL CMP_PUT0R(MIPJLOC,'VAL',PARVAL,STATUS)
	      ELSE IF(NIN.EQ.2)THEN
	        CALL CMP_PUT0R(MIPJLOC,'LOW',PARVAL,STATUS)
	      ELSE
	        CALL CMP_PUT0R(MIPJLOC,'HI',PARVAL,STATUS)
	      ENDIF
	      IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)
	      LSUB=0
	    ELSE
	      IF(LSUB.EQ.0)THEN
	        SUBSTR=SYM
	      ELSE
	        SUBSTR=SUBSTR(1:LSUB)//SYM
	      ENDIF
	      LSUB=LSUB+1
	    ENDIF
 300	  ENDDO
 350	  CALL DAT_ANNUL(MIPJLOC,STATUS)

	ENDDO
	CALL DAT_ANNUL(MILOC,STATUS)
	CALL DAT_ANNUL(MIPLOC,STATUS)

      END DO

*    History file entry
      CALL HIST_ADD( FLOC, VERSION, STATUS )

*    Exit
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
