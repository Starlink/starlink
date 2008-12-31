*+FORM_FILL        Provides a form filling facility for the package.
********************************************************************************
*  History
*     1987 Aug	Mark Harris	1st Version (UPG_FORM)
*     1988 Oct	M Ricketts	Mods to try varying page no. cols.
*     				Also to get defaults from another file
*     1989 Jan    ::		Separate Cover and Target Files
* 	Libraries required	DBSLIB, SMGLIB, MDHLIB, POP_FORM
*	Called by		RPS or POP_MANAGER.
*     1992 Apr	M. Duesterhaus  remove VAX RTL calls
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1993 June         P. Brisco       Got rid of SMG junque.
*     1994 Jan		M Ricketts	RAL version
*     1996 Mar		M Ricketts	Tidy filenames
********************************************************************************
      SUBROUTINE FORM_FILL( WIDTH, STATUS)

      IMPLICIT NONE

*  Calling Arguments
      INTEGER WIDTH		! In	Screen Width
      INTEGER STATUS		! 	Exit status, 0 = OK
 
*-
*  Global Variables
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_points.inc'
      INCLUDE 'com_form_qual.inc'
      INCLUDE 'com_form_mtext.inc'
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_iof.inc'
  
      INTEGER NCHAR_DEF
      CHARACTER*128 DEFAULT_FILE
      CHARACTER*3  FILL_TYPE
      COMMON /DEFILE/ NCHAR_DEF, DEFAULT_FILE, FILL_TYPE
      CHARACTER*64 END_MESSAGE/' '/
      INTEGER CHECKSUM
      COMMON /KEEPCHECK/ CHECKSUM
      CHARACTER*8 HELPLIB
      COMMON / HELP_LIB_NAME / HELPLIB
 
*  Functions
      INTEGER MDH_ENDWORD
      CHARACTER*17 DBS_INFOC	! Gets character information about the record.
	CHARACTER*80 form_getc	! Gets default file info.
      LOGICAL      DBS_GETL	! Gets a logical value from the record.
     & ,           MDH_GETL	! Gets logical input.
 
* Local Variables
      CHARACTER*3  CNUM		! Character version of target number.
      CHARACTER*1  RMFILE
      INTEGER      HDID		! Header ID.
     & ,           I,J		! Loop variable
     & ,           IERR		! Error indicator.
     & ,           MESID	! Display area for messages.
      INTEGER NTARG, RECN
      CHARACTER*1 XTYPE		! Returned by FORM_VALS - 'Q' terminates targets

      LOGICAL MORE_TARGETS	! True until XTYPE returns 'Q'
      LOGICAL FILE_EXIST

      CHARACTER*128  TEST_FILE(MAX_FILE_COUNT)

*  Titla Data
      CHARACTER*19 TITLE(4)	! Titles of separate forms.
      DATA TITLE  / '    Cover Page'  , '   General Page',
     &              ' Target Record',   '    Constraints'/

* _____________________________ Executable Code _______________________________
c
      rmfile = 'N'
      test_file(1) = form(1:len_form_file)
      test_file(2) = form(1:len_form_file)//'_target'

      DO i=1,max_file_count
         INQUIRE (file=test_file(i),exist=file_exist)

         IF (file_exist) THEN

            IF (rmfile .eq. 'N') THEN
               WRITE(*,'(XA$)')' Warning - files exist. Overwrite? '//
     &              '(Y/N,Default is N): '
               READ (*,'(A)') rmfile
            END IF

            IF (CHAR(ICHAR(rmfile) .AND. 95) .EQ. 'Y') THEN
               CALL DELFILE(test_file(i)(1:MDH_ENDWORD(test_file(i))))
             ELSE
               CALL form_filename
	       RETURN
            END IF

         END IF           ! file_exist

      END DO              ! i = 1,max_file_count

*  Clear text space and put in info

      status = 0
 30	default_file = FORM_GETC('Cover page defaults file name? ','None')

	IF (default_file .EQ. 'None') THEN
		fill_type = 'NEW'
        ELSE IF (default_file .EQ. '?') THEN
		CALL MDH_HELP(HELPLIB,'COVER_DEF')
		GOTO 30
	 ELSE
		nchar_def = MDH_ENDWORD(default_file)
		fill_type = 'DEF'
	ENDIF

      CALL FORM_OPEN( 'C', STATUS )						! Open both Cover and Target files
      IF (STATUS .NE. 0 ) GOTO 99						! and default file if required

      CALL FORM_INIT( 'COVER') ! Initialise data collection routines.
      CALL FORM_INIT( 'GEN')
      CALL FORM_INIT( 'TARGET')
      CALL FORM_INIT( 'CONSTR')


10    CONTINUE									! Return here if more forms
      CALL FORM_VALS( 'COVER' , FILL_TYPE , MESID , HDID , TITLE(1), XTYPE )	!   Get 'Cover' page' data
      CALL FORM_VALS( 'GEN'   , FILL_TYPE , MESID , HDID , TITLE(2), XTYPE )	!   Get 'General Page' data
 
      NTARG = 0
      MESID = -99
      MORE_TARGETS = .TRUE.
      DO WHILE (MORE_TARGETS)							!   Do for each target.

         NTARG = NTARG + 1
         QTARGET = NTARG							!   Pass to con check , etc.
         WRITE( CNUM , '(I3)' ) NTARG
         CALL DBS_PUTC( REF_TARGET, FLD_TARG_NUMBER , CNUM, IERR )			!    Write Target  No. to record

15       CONTINUE								! Do again if 2 failures on constraints form
C 	 IF (NTARG.EQ.1) THEN
C	    MESID = -99
C         ELSE 
C         END IF
         CALL FORM_VALS( 'TARGET', FILL_TYPE, MESID, HDID, TITLE(3)(:15)//CNUM, XTYPE)	!    Get target form values.
         IF (MESID .EQ. -99) THEN
            MESID = 0
         END IF
         IF (XTYPE .EQ. 'Q') THEN
            MORE_TARGETS = .FALSE.
            NTARG = NTARG - 1							! Actual no. of targets
            GOTO 20
         ELSE
            MORE_TARGETS = .TRUE.
         END IF

         IF ( DBS_GETL( REF_TARGET, FLD_CONSTRAINTS ) ) THEN			!    If constraints then.
            CALL FORM_VALS( 'CONSTR', FILL_TYPE, MESID, HDID, TITLE(4), XTYPE)	!     Get constraints values.
            IF (XTYPE .EQ. 'T' ) GOTO 15					! go back to the target form

         ELSE									!    Else -
            DO J = FLD_LIMS_CONS( 1 ) , FLD_LIMS_CONS( 2 )				!     Put in default values.
               CALL DBS_PUTC(REF_TARGET,J, DBS_INFOC(REF_TARGET,J,'NULVALUE'), IERR)
            END DO
         END IF
         CALL FORM_WRITE( REF_TARGET, NTARG , IERR )				!   Write the record
         MORE_TARGETS = MDH_GETL(' Another Target', .FALSE.)
 
20       CONTINUE
      END DO									! End inputting targets

      WRITE( CNUM , '(I3)' ) NTARG
      CALL DBS_PUTC( REF_FORM , FLD_NTARGETS , CNUM , IERR )				!    Write No.Targets to Cover record

      RECN = 1
      CALL FORM_WRITE( REF_FORM , RECN , IERR )					!   Write the record for Cover

      WRITE(MTEXT(LOC_MSTATUS:), '(A,I3,A)') 'Files created with', NTARG, ' targets'
      GOTO 100

99    CONTINUE
      WRITE(MTEXT(LOC_MSTATUS:), '(A)') ' Error in file creation'

100   CONTINUE
      END
