*+DBS_OPEN       Opens and reads dscf, opens file
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*---------------------------------------------------------------------------
      SUBROUTINE DBS_OPEN( REF_NO , FILE , USE )

*  History
*     1987 Aug	M Harris	1st version
*     1988 Oct	M Ricketts	Change access to dscf to 'readonly'
*     1989 Jan	   ::		Add Open_type, shows how file was opened (re-edit!)
*     1992 Apr  M. Dueterhaus   Remove VAX RTL calls
*     1996 Mar  m Ricketts      tidy filenames

*DESCRIPTION:

*     This subroutine opens the files and then gets the field information for the
*     database and loads it into common blocks for access by the rest of the
*     program. It also provides a backup file in case of errors.

* Caliing Arguments
      CHARACTER*(*) FILE	! Name of database.
     & ,            USE		! Type of access required.
				! 'C' - Create, 'W' - Write (makes copy)
				! 'D' - Reads just the DSCF
				!  None of the above; access for reading?
      INTEGER       REF_NO	! Reference number of data set.

*  Global Variables
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_field.inc'
      INCLUDE 'com_dbs_chars.inc'
      INCLUDE 'com_dbs_iof.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_form_files.inc'

      CHARACTER*5   FILESYM(MAX_FILE_COUNT)		! Symbolic name of file.
      COMMON / DBS_FILES / FILESYM

*  Local Variables
      INTEGER MAXFILES_PLUS1
      PARAMETER(MAXFILES_PLUS1=MAX_FILE_COUNT + 1)
      CHARACTER*128 NONUMNAME,NONUMNAME2	! Above with no version number.
     & ,           TEMPNAME 	! Temporary storage for above.
      INTEGER          I            ! Temporary value of REFDB.
     & ,       J 		! Loop variables.
     & ,           IDSCF	! Logical unit number for DSCF file.
     & ,           IERR        	! Error indicator.
     & ,           POINTER	! Indicates data set with required info.
     & ,	   LEN_TEMP_FILE
      INTEGER      LUNNUM

*  Functions
      INTEGER  MDH_ENDWORD, nchars

*  ________________________  Executable Code  ______________________________
	j = MDH_ENDWORD(FILE)

      IF ( REF_NO .GT. 0 .AND. ARRAY( REF_NO ) .NE. 2 ) THEN

        I = REF_NO

        IF ( ARRAY( I ) .EQ. 1 ) THEN

          POINTER = I

        ELSE

          POINTER = 0

        END IF

      ELSE

        IF ( REF_NO .LE. 0 ) THEN

          POINTER = 0

        ELSE

          POINTER = REF_NO

        END IF

        J = 1

        DO WHILE( J .LE. MAX_FILE_COUNT .AND. ARRAY( J ) .EQ. 2 )		! Do until space found.

          J = J + 1								!  Increment pointer.

        END DO									! End do.

        I = J

        DO WHILE( I .LE. MAX_FILE_COUNT .AND. ARRAY( I ) .NE. 0 )		! Do until space found.

          I = I + 1								!  Increment pointer.

        END DO									! End do.

        IF ( I .EQ. MAXFILES_PLUS1 .AND. J .NE. MAXFILES_PLUS1 ) I = J

      END IF

      IF ( I .EQ. MAXFILES_PLUS1 ) THEN						! If pointer too big.

        REF_NO = -4								!  Return error message.

      ELSE

        FILENAME( I ) =' '
        IF ( POINTER .LE. 0 ) THEN
            CALL GETLUN( IDSCF )						!  Get logical unit number.

	  IF (I.EQ.1) THEN
	    TEMPNAME= dscfrps_data(:len_dscfrps)//'dscfrps_form'
	  ELSE IF (I.EQ.2) THEN
	    TEMPNAME= dscfrps_data(:len_dscfrps)//'dscfrps_form_target'
          else
            WRITE(*,*) ' Error - dbs_open, i=',I
            stop
	  END IF
          OPEN( IDSCF , FILE = TEMPNAME, STATUS = 'OLD'
     &       ,    IOSTAT = IERR )  					!  Open descriptor file if it exists.

          IF ( IERR .NE. 0 .AND. POINTER .LE. 0 ) THEN				!  If no descriptor file.

	    WRITE(*,*) ' Error - dbs_open failed to open descriptor file'
            REF_NO = -2								!   Set error message.
            CALL FRELUN( IDSCF )

          ELSE

            REF_NO = 0

          END IF

        END IF

        IF ( USE(:1) .NE. 'D' .AND. USE(:1) .NE. 'C' ) THEN			! If write open required.

          IF ( USE(:1) .EQ. 'W' ) THEN						! If write open required.
										!  To make a backup file.
	      LEN_TEMP_FILE= MDH_ENDWORD(FILE)
	      TEMPNAME = FILE(1:LEN_TEMP_FILE)
	      NONUMNAME = TEMPNAME
              NCHARS = MDH_ENDWORD(NONUMNAME)
              NONUMNAME2 = NONUMNAME(1:NCHARS)//'2'

              CALL FILE_COPY(NONUMNAME(:NCHARS),NONUMNAME2(:NCHARS+1), IERR)		! make a copy
              CALL GETLUN( LUNNUM )
              OPEN( LUNNUM, FILE = NONUMNAME, STATUS = 'OLD',
     &          ACCESS='DIRECT',RECL = 80, IOSTAT= IERR )
              FILENAME( I ) = NONUMNAME						!   Set the filename vector to store the filename.

          ELSE

            CALL GETLUN( LUNNUM )
            LEN_TEMP_FILE= MDH_ENDWORD(FILE)
            TEMPNAME = FILE(1:LEN_TEMP_FILE)
            OPEN( LUNNUM , FILE = TEMPNAME ,
	1	STATUS = 'OLD' , ACCESS = 'DIRECT'
	2       , RECL = 80,    IOSTAT = IERR )
          END IF

          IF ( IERR .NE. 0 ) THEN

            IF ( REF_NO .EQ. -2 ) THEN						!    If already an error.

              REF_NO = -3							!     Return error message for both.

            ELSE								!    Else if only error.

              REF_NO = -1							!     Return error message for this.
              CLOSE( IDSCF )
              CALL FRELUN( IDSCF )

            END IF								!    End if.

            CALL FRELUN( LUNNUM )

          ELSE

            LNDB( I ) = LUNNUM
          END IF								!  File search over.

        END IF									! End if.

      END IF

      IF ( REF_NO .GE. 0 ) THEN

        REF_NO = I								!   Set up reference number.
        FILESYM( I ) = FILE
        OPEN_TYPE ( I ) = USE(:1)

        CALL DBS_READ_DSCF(REF_NO, IDSCF, POINTER )

        IF ( USE(:1) .EQ. 'C' ) THEN						! If file is to be re-opened.
          CALL GETLUN( LNDB( I ) )
	  LEN_TEMP_FILE= MDH_ENDWORD(FILE)
          OPEN (LNDB(I),FILE = FILE(1:LEN_TEMP_FILE),STATUS='NEW'			!  Open the file.
	1     ,     RECL = 80 , ACCESS = 'DIRECT'		!  Set record size and access.
	2     ,     IOSTAT = IERR)			!  Set organisation.
          ARRAY( I ) = 2

        ELSE IF ( USE(:1) .EQ. 'D' ) THEN
          ARRAY( I ) = 1

        ELSE
          ARRAY( I ) = 2
        END IF
      END IF									! End if.

      END									! The file is ready for use.
