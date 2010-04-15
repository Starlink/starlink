*+RPS              Form filling program for Remote Proposals
      PROGRAM RPS

********************************************************************************
*  History
*     1987 May	M.D.C.Harris RAL	1st Version, POP_MANAGER
*     1988 	M Ricketts,		RPS version (Remote Proposal Submission)
*     		M Bush, D Selby
*     1991      M Ricketts		Small mods for AO2 release
*     1992 Mar  M Duesterhaus		remove VAX specific stuff
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1993 June         P. Brisco       Deleted SMG stuff.
*     1993 July		P. Brisco	Changed the hard coded helplib to look
*					for the help file where the logical is
*					defined.
*     1994 Jan		M Ricketts	Back with RAL again!
*     1995 Feb          M Ricketts      AO6 Parameters
*     1996 Mar          M Ricketts      AO7 Parameters, tidy filenames
*     1997 Apr             ::           AO8 Parameters
*     1997 May             ::           AO9 Parameters
********************************************************************************

      IMPLICIT NONE

* Global Variables

      INCLUDE 'com_form_files.inc'
      DATA FORM/'rps_form'/

      CHARACTER*3 RPS_VERSION
      COMMON /VERSION/ RPS_VERSION
      DATA RPS_VERSION/'5.6'/

      INTEGER NCHAR_DEF/1/
      CHARACTER*128 DEFAULT_FILE
      CHARACTER*3  FILL_TYPE/'NEW'/
      COMMON /DEFILE/ NCHAR_DEF, DEFAULT_FILE, FILL_TYPE

      DOUBLE PRECISION AO_MJD0 /51162.0D0/    ! Dec 15th, 1998
      DOUBLE PRECISION AO_PERIOD /365.0D0/
      DOUBLE PRECISION AO_DELAY /1.0D0/
      DOUBLE PRECISION SUN_MAX_OFF_DEG /14.0D0/
      COMMON /AO_SPECS/ AO_MJD0,AO_PERIOD,AO_DELAY,SUN_MAX_OFF_DEG

      CHARACTER*8 HELPLIB /'rps_help'/
      COMMON / HELP_LIB_NAME / HELPLIB

      CHARACTER*64 END_MESSAGE/'  '/
      COMMON /EXITEXT/END_MESSAGE

      INTEGER      WIDTH/132/, START_WIDTH/80/
      LOGICAL COLS132
      COMMON /WIDTH_KEEP/ WIDTH,COLS132,START_WIDTH

      INCLUDE 'com_form_mtext.inc'		! Text for smg_menu
      INCLUDE 'com_form_ao.inc'			! AO number
      INCLUDE 'com_form_qual.inc'			! AO number

*  Functions
      INTEGER     POP_MENU
*-
*  Local Variables
      INTEGER      STATUS, INIT_CURS/1/, ISTAT
      INTEGER      OUT /1/
      INTEGER*4    lbuf,lret
      LOGICAL      RETRY /.FALSE./
      CHARACTER*60 cbuf,cret
      DOUBLE PRECISION MJD_NOW

*    Screen Text for Main Selection
      INTEGER NOPT, NOPT1
      PARAMETER (NOPT = 8, NOPT1 = NOPT+1)					! Number of options
      CHARACTER*31 HEADING / '   Remote Proposal Submission   '/
      CHARACTER*20 OPTIONS(NOPT) /  'Create new file',
     1     'Edit old file','Create/Edit abstract','Print file',
     2     'Submit proposal','Summarise File','Check Target','Help'/
*  __________________________ Executable Code __________________________________
	QUAL_COVER = .TRUE.
	QUAL_GEN = .TRUE.

* replace trlog with sys_ routine
	cbuf = 'RPS_AUX'
	lbuf = 7
	CALL SYS_GETENV( cbuf(:lbuf), cret, lret, istat)
	dscfrps_data = cret(:lret)
	len_dscfrps = lret

	REF_FORM=0
	REF_TARGET=0

        AO_NUMBER = 9
        AO_CHAR  = '9'

      WRITE(*,'(A,I1)' )  '         RPS - Rosat Remote Proposal Submission vsn '// RPS_VERSION //' for AO',AO_NUMBER

      CALL GETMJD(MJD_NOW)
      IF (MJD_NOW .GT. AO_MJD0 - 30.0D0 ) WRITE(*,'(//5x,a//)' ) ' ** RPS is not initialised for the correct AO! **'

      CALL FORM_INTRO
      CALL FORM_FILENAME

      DO WHILE ( OUT .NE. NOPT1)

         OUT = POP_MENU(OPTIONS, NOPT , HEADING,INIT_CURS ,MTEXT) 	! Call menu and get program to run.

	 IF (out .EQ. 102) THEN
		out = 8
	  ELSE IF (out .EQ. 106) THEN
		CALL key_help (112)
	  ELSE IF (out .GT. 100) THEN
		WRITE (*,*), '** This option not available for the '//
	1		'main menu. **'
	 ENDIF

         INIT_CURS = NOPT1							! so <ret> exits

         IF (out .EQ. 1) THEN   ! Form filler
            CALL form_fill(width,status)
            IF (status .NE. 0) GOTO 99

          ELSE IF (out .EQ. 2) THEN
            CALL form_edit(status)
            IF (status .LT. 0) GOTO 99

          ELSE IF (out .EQ. 3) THEN
            CALL abstract_edit

          ELSE IF (out .EQ. 4) THEN ! Print form
            CALL form_print(status)

          ELSE IF (out .EQ. 5) THEN ! Submit proposal
            CALL form_submit

          ELSE IF (out .EQ. 6) THEN
            CALL do_summary

          ELSE IF (out .EQ. 7) THEN
            CALL target_check

          ELSE IF (out .EQ. 8) THEN
            CALL mdh_help(helplib,'  ')
         END IF

      END DO

99    CONTINUE
      CALL FORM_CLOSE
      IF (END_MESSAGE(:2).NE.' ') WRITE(*,'(A)') ' '//END_MESSAGE

      END
