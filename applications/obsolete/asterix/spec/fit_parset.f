*+  FIT_PARSET - Sets up parameters for a composite fit model
      SUBROUTINE FIT_PARSET( GENUS, NCOMP, MODL, FLOC, STATUS )
*
*    Description :
*
*     Sets up a set of descriptors and parameters for each primitive component
*     model (pmodel) of a composite fit model (cmodel) in the fit_model data
*     object pointed into by the locator FLOC. The information required to do
*     this is obtained from the appropriate (according to GENUS) model menu
*     file.
*
*    Method :
*    Deficiencies :
*
*     Only spectral models supported at present.
*
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*
*     15 Jan 85 : Original (PARSET) (TJP)
*     12 Feb 85 : FREEZE flag incorporated (TJP)
*     29 Jun 87 : Renamed to FIT_PARSET, GENUS imported (TJP)
*      9 Nov 87 : Now accesses 'GENUS'_MENU (TJP)
*     14 Jun 88 : Menu accessed READONLY (TJP)
*     20 Jun 89 : ASTERIX88 version, some tidying of code (TJP)
*     15 Sep 92 : Uses FIO for menu i/o (DJA)
*      6 Nov 92 : Cater for "frozen" flags in menu file (TJP)
*     23 Aug 93 : Generalised to pmodel keys of any length (DJA)
*     29 Mar 94 : Modularised to make FIT_PARSET_SUB more useful to external
*                 software. FIT_PARSET now simply handles the composite
*                 nature of a cmodel. (DJA)
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
*
*    Import :
*
      CHARACTER*(*)		GENUS			! Model genus
      INTEGER                	NCOMP			! No of pmodels in cmodel
      CHARACTER*(*)  	     	MODL(NCOMP)		! Keywords of the pmodels
      CHARACTER*(DAT__SZLOC) 	FLOC			! Locator to fit_model obj.
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) 	MLOC			! Locator to pmodels
      CHARACTER*(DAT__SZLOC) 	MILOC			! Locator to pmodel I

      INTEGER			I			! Loop over pmodels
      INTEGER 			MFD                     ! Menu file descriptor
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Locate pmodels in fit_model data object
      CALL DAT_FIND( FLOC, 'PMODEL', MLOC, STATUS )

*    Access menu file
      CALL FIT_MEN_OPEN( GENUS, MFD, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 9000

*    Loop through pmodels
      DO I = 1, NCOMP

*      Locate I'th cell of the model structure
	CALL DAT_CELL( MLOC, 1, I, MILOC, STATUS )

*      Set up this pmodel
        CALL FIT_PARSET_SUB( MFD, MODL(I), MILOC, STATUS )

*      Flush errors for user
	IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_FLUSH( STATUS )
        END IF

*      Free the cell
        CALL DAT_ANNUL( MILOC, STATUS )

*      Rewind menu file unless last component
        IF ( I .LT. NCOMP ) THEN
          CALL FIO_RWIND( MFD, STATUS )
        END IF

      END DO

*    Release PMODEL object
      CALL DAT_ANNUL( MLOC, STATUS )

*    Close menu file
      CALL FIT_MEN_CLOSE( MFD, STATUS )

*    Exit
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_PARSET', STATUS )
      END IF

      END



*+  FIT_PARSET_SUB - Sets up parameters for a pmodel
      SUBROUTINE FIT_PARSET_SUB( MFD, MODL, PCELL, STATUS )
*
*    Description :
*
*     Sets up a set of descriptors and parameters for each primitive component
*     model (pmodel) of a composite fit model (cmodel) in the fit_model data
*     object pointed into by the locator FLOC. The information required to do
*     this is obtained from the appropriate (according to GENUS) model menu
*     file.
*
*    Method :
*    Deficiencies :
*
*     Only spectral models supported at present.
*
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*
*     15 Jan 85 : Original (PARSET) (TJP)
*     12 Feb 85 : FREEZE flag incorporated (TJP)
*     29 Jun 87 : Renamed to FIT_PARSET, GENUS imported (TJP)
*      9 Nov 87 : Now accesses 'GENUS'_MENU (TJP)
*     14 Jun 88 : Menu accessed READONLY (TJP)
*     20 Jun 89 : ASTERIX88 version, some tidying of code (TJP)
*     15 Sep 92 : Uses FIO for menu i/o (DJA)
*      6 Nov 92 : Cater for "frozen" flags in menu file (TJP)
*     23 Aug 93 : Generalised to pmodel keys of any length (DJA)
*     29 Mar 94 : Derived from old FIT_PARSET. Checks to see if components
*                 exist before they are created. Means that that this routine
*                 can be used to update a existing model. (DJA)
*     11 Nov 94 : Use AIO to do menu file input (DJA)
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
*
*    Import :
*
      INTEGER			MFD			! Menu file descriptor
      CHARACTER*(*)		MODL			! Keyword of pmodel
      CHARACTER*(DAT__SZLOC) 	PCELL			! Locator to PMODEL cell
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local constants :
*
      CHARACTER*30        ERRMEN
        PARAMETER         ( ERRMEN = 'Error in menu file' )
*
*    Local variables :
*
      CHARACTER*(MAXKEYLEN) 	KEY			! Pmodel keyword
      CHARACTER*80 		LINE			! Line from menu file
      CHARACTER*(DAT__SZLOC) 	MIPLOC			! Locator to parameters
							! of this pmodel
      CHARACTER*(DAT__SZLOC) 	MIPJLOC			! Locator to param J of
							! this pmodel
      CHARACTER*40 		NAME			! Pmodel name
      CHARACTER*30 		PNAME			! Parameter name
      CHARACTER*80 		STRING			! String from LINE
      CHARACTER*14 		TYPE			! Pmodel type
      CHARACTER*25 		UNITS			! Parameter units

      REAL 			VAL			! Parameter value
      REAL 			LOW			! Parameter lower limit
      REAL 			HI			! Parameter upper limit

      INTEGER			NCOMP			! No object components
      INTEGER 			NPAR			! No of parameters in pmodel
      INTEGER 			I,J,N

      LOGICAL			FROZEN			! Frozen in menu file
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Look for appropriate pmodel (by keyword)
 10   CALL AIO_READF( MFD, LINE, STATUS )
      IF ( INDEX(LINE,'endmenu') .GT. 0 ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( 'BADMOD', 'Model not found in fit_model file',
     :                STATUS )
	GOTO 9000
      END IF
      N = INDEX(LINE,'key:')
      IF ( N .EQ. 0 ) GOTO 10

*    Extract keyword and compare with MODL
      STRING=LINE(N+4:80)
      CALL CHR_LDBLK(STRING)
      IF(STATUS.NE.SAI__OK) GOTO 9000
      KEY = STRING(1:MAXKEYLEN)
      IF ( KEY .NE. MODL ) GOTO 10

*    Remove existing components of PCELL
      CALL DAT_NCOMP( PCELL, NCOMP, STATUS )
      IF ( NCOMP .GT. 0 ) THEN
        CALL HDX_ERASE( PCELL, STATUS )
      END IF

*    Correct pmodel found - copy attributes from menu to fit_model object
      CALL DAT_NEWC( PCELL, 'KEY', MAXKEYLEN, 0, 0, STATUS )
      CALL CMP_PUT0C( PCELL, 'KEY', KEY, STATUS )
      IF(STATUS.NE.SAI__OK) GOTO 9000

*    Pmodel name
      CALL AIO_READF( MFD, LINE, STATUS )
      N = INDEX(LINE,'mname:')
      IF ( N .EQ. 0 ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( 'BADMNU', ERRMEN, STATUS )
      ELSE
	STRING=LINE(N+6:80)
	CALL CHR_LDBLK(STRING)
	NAME=STRING(1:40)
	CALL DAT_NEWC( PCELL,'NAME',40,0,0,STATUS)
	CALL CMP_PUT0C( PCELL,'NAME',NAME,STATUS)
      END IF
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH(STATUS)

*    Pmodel type (additive/multiplicative)
      CALL AIO_READF( MFD, LINE, STATUS )
      N = INDEX(LINE,'type:')
      IF ( N .EQ. 0 ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( 'BADMNU', ERRMEN, STATUS )
      ELSE
	STRING=LINE(N+5:80)
	CALL CHR_LDBLK(STRING)
	TYPE=STRING(1:14)
	CALL DAT_NEWC( PCELL,'TYPE',14,0,0,STATUS)
	CALL CMP_PUT0C( PCELL,'TYPE',TYPE,STATUS)
      END IF
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH(STATUS)

*    Number of parameters in pmodel
 30   CALL AIO_READF( MFD, LINE, STATUS )
      N = INDEX(LINE,'npar:')
      IF(INDEX(LINE,'endmodel').GT.0)THEN
	STATUS=SAI__ERROR
	CALL ERR_REP( 'BADMNU', ERRMEN, STATUS )
	GOTO 9000
      ELSE IF(N.GT.0)THEN
	STRING=LINE(N+5:80)
	CALL CHR_CTOI(STRING,NPAR,STATUS)
	CALL DAT_NEW( PCELL,'NPAR','_INTEGER',0,0,STATUS)
	CALL CMP_PUT0I( PCELL,'NPAR',NPAR,STATUS)
      ELSE
	GOTO 30			! Read next line for current model
      END IF
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH(STATUS)

*    Create parameter structure, & access
      CALL DAT_NEW( PCELL, 'PAR', 'SPEC_PARAMETER', 1, NPAR, STATUS )
      CALL DAT_FIND( PCELL, 'PAR', MIPLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 9000

*    Copy across all parameters for current pmodel
      DO J = 1, NPAR

*      Look for parameter start
 50	CALL AIO_READF( MFD, LINE, STATUS )
	IF(INDEX(LINE,' parameter').EQ.0)THEN
	  IF(INDEX(LINE,'endmodel').GT.0)THEN
	    STATUS=SAI__ERROR
	    CALL ERR_REP( 'BADMNU', ERRMEN, STATUS )
	    GOTO 9000
	  ELSE
	    GOTO 50		! Read next menu line
	  END IF
	END IF

*      Parameter name
	CALL DAT_CELL(MIPLOC,1,J,MIPJLOC,STATUS)
	CALL AIO_READF( MFD, LINE, STATUS )
	N = INDEX(LINE,'pname:')
	IF(N.EQ.0)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP( 'BADMNU', ERRMEN, STATUS )
	ELSE
	  STRING=LINE(N+6:80)
	  CALL CHR_LDBLK(STRING)
	  PNAME=STRING(1:30)
	  CALL DAT_NEWC(MIPJLOC,'NAME',30,0,0,STATUS)
	  CALL CMP_PUT0C(MIPJLOC,'NAME',PNAME,STATUS)
	END IF
        IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH(STATUS)

*      Parameter units
	CALL AIO_READF( MFD, LINE, STATUS )
	N=INDEX(LINE,'units:')
	IF(N.EQ.0)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP( 'BADMNU', ERRMEN, STATUS )
	ELSE
	  STRING=LINE(N+6:80)
	  CALL CHR_LDBLK(STRING)
	  UNITS=STRING(1:25)
	  CALL DAT_NEWC(MIPJLOC,'UNITS',25,0,0,STATUS)
	  CALL CMP_PUT0C(MIPJLOC,'UNITS',UNITS,STATUS)
	END IF
        IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH(STATUS)

*      Parameter value
	CALL AIO_READF( MFD, LINE, STATUS )
	N=INDEX(LINE,'val:')
	IF(N.EQ.0)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP( 'BADMNU', ERRMEN, STATUS )
	ELSE
	  STRING=LINE(N+4:80)
	  CALL CHR_CTOR(STRING,VAL,STATUS)
	  CALL DAT_NEW(MIPJLOC,'VAL','_REAL',0,0,STATUS)
	  CALL CMP_PUT0R(MIPJLOC,'VAL',VAL,STATUS)
	END IF
        IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH(STATUS)

*      Frozen flag (menu may contain "frozen" flag after param value)
	CALL AIO_READF( MFD, LINE, STATUS )
	N=INDEX(LINE,'frozen')
        FROZEN = (N.NE.0)
	CALL DAT_NEW(MIPJLOC,'FROZEN','_LOGICAL',0,0,STATUS)
	CALL CMP_PUT0L(MIPJLOC,'FROZEN',FROZEN,STATUS)

*      Parameter lower bound
	IF(FROZEN)THEN			! Otherwise line has been
	  CALL AIO_READF( MFD, LINE, STATUS ) ! read already
	ENDIF
	N=INDEX(LINE,'low:')
	IF(N.EQ.0)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP( 'BADMNU', ERRMEN, STATUS )
	ELSE
	  STRING=LINE(N+4:80)
	  CALL CHR_CTOR(STRING,LOW,STATUS)
	  CALL DAT_NEW(MIPJLOC,'LOW','_REAL',0,0,STATUS)
	  CALL CMP_PUT0R(MIPJLOC,'LOW',LOW,STATUS)
	END IF
        IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH(STATUS)

*      Parameter upper bound
	CALL AIO_READF( MFD, LINE, STATUS )
	N=INDEX(LINE,'hi:')
	IF(N.EQ.0)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP( 'BADMNU', ERRMEN, STATUS )
	ELSE
	  STRING=LINE(N+3:80)
	  CALL CHR_CTOR(STRING,HI,STATUS)
	  CALL DAT_NEW(MIPJLOC,'HI','_REAL',0,0,STATUS)
	  CALL CMP_PUT0R(MIPJLOC,'HI',HI,STATUS)
	END IF
        IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH(STATUS)

*      Free parameter
	CALL DAT_ANNUL( MIPJLOC, STATUS )

*    Next parameter
      END DO

*    Annul PAR object
      CALL DAT_ANNUL( MIPLOC, STATUS )

*    Exit
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_PARSET_SUB', STATUS )
      END IF

      END
