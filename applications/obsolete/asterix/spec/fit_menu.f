*+  FIT_MENU - Displays menu of available primitive spectral models
      SUBROUTINE FIT_MENU(GENUS,NCIMP,MENU,STATUS)
*
*    Description :
*
*     Retrieves keywords and names of the supported primitive models from model
*     menu file (e.g. SPEC_MENU) and displays these as a menu on the user's
*     terminal.
*     No.of models implemented and an array of the model keywords are returned
*     to the calling routine.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*
*     14 Jan 85 : Original (SPMENU)
*     29 Jun 87 : FIT_MENU (TJP)
*      9 Nov 87 : Menu 'GENUS'_MENU accessed (TJP)
*      3 Dec 87 : Read access for menu (TJP)
*     20 Jun 89 : ASTERIX88 version, minor tidying (TJP)
*     10 May 91 : Minor bug fix in error reporting (TJP)
*     14 Sep 92 : Use FIT_MEN_OPEN to open menu file, and FIO for menu file
*                 access (DJA)
*     18 Aug 93 : Handles keys of any length, and does i/o properly (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Import :
*
	CHARACTER*4 GENUS			! Model genus
*
*    Export :
*
	INTEGER NCIMP				! No of models supported
	CHARACTER*(*) MENU(*)			! Menu of model keys
*
*    Status :
*
	INTEGER STATUS
*
*    Local variables :
*
	CHARACTER*79 LINE			! Line from menu file
	CHARACTER*80 STRING			! String from LINE
	CHARACTER*(MAXKEYLEN) KEY		! Model keyword
	CHARACTER*40 NAME			! Model name
	CHARACTER*14 TYPE			! Model type

      INTEGER        MFD                        ! Menu file descriptor
      INTEGER        N

      LOGICAL        FINISHED                   ! Finished reading model file?
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Access menu file
      CALL FIT_MEN_OPEN( GENUS, MFD, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      NCIMP = 0

*    Write menu prologue
      CALL MSG_BLNK()
      CALL MSG_PRNT( 'A composite model can be synthesised using'/
     :               /' + - * ( ) and' )
      CALL MSG_PRNT( 'any of the following primitive models:' )
      CALL MSG_BLNK()

*    Read line from menu file, leave loop if end of menu
      FINISHED = .FALSE.
      DO WHILE ( (STATUS .EQ. SAI__OK) .AND. .NOT. FINISHED )

	CALL FIO_READF( MFD, LINE, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN

          CALL MSG_SETC( 'GENUS', GENUS )
	  CALL ERR_REP( ' ', 'Unexpected end of file in ^GENUS '/
     :                                     /'menu file', STATUS )

	ELSE IF ( INDEX(LINE,'endmenu') .GT. 0 ) THEN
          FINISHED = .TRUE.

        ELSE IF ( INDEX(LINE,'  model') .GT. 0 ) THEN

*        Header found - read model key, name & type
	  CALL FIO_READF( MFD, LINE, STATUS )
	  N=INDEX(LINE,'key:')
	  IF(N.EQ.0)THEN
	    STATUS=SAI__ERROR
	    CALL ERR_REP('BADMNU','Error in menu file',STATUS)
	    GOTO 99
	  END IF

*        Strip out keyword
	  STRING=LINE(N+4:)
	  CALL CHR_LDBLK(STRING)
	  IF(STATUS.NE.SAI__OK) GO TO 99
	  KEY=STRING

	  CALL FIO_READF( MFD, LINE, STATUS )
	  N=INDEX(LINE,'mname:')
	  IF(N.EQ.0)THEN
	    STATUS=SAI__ERROR
	    CALL ERR_REP('BADMNU','Error in SPECFIT.MNU',STATUS)
	    GOTO 99
	  END IF

*        Strip out model name
	  STRING=LINE(N+6:)
	  CALL CHR_LDBLK(STRING)
	  IF(STATUS.NE.SAI__OK) GO TO 99
	  NAME=STRING(1:40)

	  CALL FIO_READF( MFD, LINE, STATUS )
	  N=INDEX(LINE,'type:')
	  IF(N.EQ.0)THEN
	    STATUS=SAI__ERROR
	    CALL ERR_REP('BADMNU','Error in SPECFIT.MNU',STATUS)
	    GOTO 99
	  END IF

*        Strip out model type
	  STRING=LINE(N+5:)
	  CALL CHR_LDBLK(STRING)
	  IF(STATUS.NE.SAI__OK) GO TO 99
	  TYPE=STRING(1:14)

*        If model's type is properly defined then add it to menu
	  IF(TYPE.EQ.'additive'.OR.TYPE.EQ.'multiplicative')THEN
	    NCIMP=NCIMP+1
	    MENU(NCIMP)=KEY
	    WRITE(LINE,50)KEY,NAME,TYPE
 50	    FORMAT(3X,A5,2X,A40,5X,A14)
            CALL MSG_PRNT( LINE )
	  ENDIF

        END IF

      END DO

*    Sign off
      CALL MSG_BLNK()

*    Close menu file
      CALL FIT_MEN_CLOSE( MFD, STATUS )

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MENU', STATUS )
      END IF

      END
