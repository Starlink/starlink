*+FORM_LATEX       Outputs a Latex File for printing source forms
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1993 Jun          P. Brisco       Removed SMG junque.
*---------------------------------------------------------------------------
      SUBROUTINE FORM_LATEX(FTYPE,STATUS)
 
*  Type Declaration
      IMPLICIT NONE
 
*  Calling Arguments
      CHARACTER*(*) FTYPE		! 'ALL', 'SELECT', 'BLANK', or 'SUBLTX'
      INTEGER STATUS			! Status, 0 = OK
 
*  Global Variables
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_latex.inc'
      INCLUDE 'com_form_mtext.inc'
 
	CHARACTER       DATE_TIME*18

*******************************************************************************
*  History
*     1988 October	M Ricketts	1st Version
*     1989 Jan	Dave Ewart: Symbol RPS_TEX defined to local LATEX command
*       		Added EXTN to distinguish concurrent batch jobs
*     1991 Oct  MJR	Create multiple .tex files if more than 8 targets etc.
* 			to avoid Latex crashing
*     1992 Aug  M. Duesterhaus	Remove VAX specific code 
*  Method
*     The Descriptor file for the forms contains strings in the comment fields
*     delimited by ':'s to describe the page(s) and location(s) on which each
*     field is to be put. The strings will be written with:
*     \put(location) {\makebox(0,0)[bl]{String}}
*     						(emphasis may be put in later)
*     The location is given as :P/X/Y: where -
*     	P Defines the page number(s)	Values allowed are 1,2,3,4,A
*     					'A' means put in the rest of the pages
 
*     	X Defines the x coordinate	Can be real or integer, max 6 characters
 
*     	Y Defines the Y coordinate,	As for X, the position and decrement
*     	  and any step for arrays	can be up to 6 chars. each, separated
*     					by a '-', ie the step must be a 
*     					decrement. The decrement MUST be given
*     					for 'G' - type fields, ie arrays OR
*     					only the first value will be output
 
*     	There can be several output locations defined for each field, but only
*     	one per page. The two fields:
*     		NUMBER.OF.TARGETS
*     		SUBJECT.CATEGORY
*     	are treated as special cases, so the code will need changing if these
*     	names are changes. Also the Abstract box is fixed in LTX_ABSTRACT
*       Otherwise, it should be possible to make any changes required by 
*	editing the dscf file.
*  ALSO Contraints page ...
*    	The files containing the basic data for each page have the names
*     	<file>_I_TEX, where <file> is the name of the file for which the dscf
*     	file applies, and I is the page number.
*     	The output is to file <file>.tex

*******************************************************************************
 
*-
 
*  Functions
      INTEGER MDH_ENDWORD
      INTEGER DBS_GETI, DBS_FIELDNO
      LOGICAL DBS_GETL
      CHARACTER*60 DBS_GETC      
*  Local Parameters
      INTEGER NEND/2/
      CHARACTER*14 END_STRING(2)
      DATA END_STRING/'end{document}','end'/
 
      CHARACTER*20 HEADING /' Process LaTeX File '/
 
*  Local Variables
      INTEGER NF, LUN_OUT, I, N, IOSTATUS, LUN_IN, NCHAR
      INTEGER IERR, NC_TEX/0/, SUB_PAGE
      CHARACTER*50 tmp
      CHARACTER*128  FILEIN, TEXFILE, FILE_NAME
      CHARACTER*256  LINE
      LOGICAL time_crit, targ_constr_page, targ_rem_page, do_page, file_exist
      CHARACTER*1  RMFILE
 
*  Executable Code
 
*     Have to assign the \  character to a variable so the SUN won't
*     interpret it as an escape character

      BSLASH= CHAR(92)

      TEXFILE = FORM_FILE(:LEN_FORM_FILE)		! needed for resubmission, so do 1st
      NC_TEX = MDH_ENDWORD( TEXFILE)

      IF (FTYPE .EQ. 'SUBLTX' ) GOTO 99			! Resubmit LaTeX job
 
      IF (.NOT.DSCF_PRINT_GOT) THEN			! Get print control
        CALL LTX_DSCF_INFO(STATUS)
        IF (STATUS.NE.0) GOTO 99
      END IF
 
      IF (FTYPE .NE. 'BLANK' ) THEN			! Get Params
 
         NF = DBS_FIELDNO(REF_FORM,'NUMBER.OF.TARGETS')
         IF (NF.GT.NFIELD_FORM) THEN
            NTARGET = 0
         ELSE
            NTARGET = DBS_GETI(REF_FORM,NF)
         END IF
 
         NF = DBS_FIELDNO(REF_FORM,'SUBJECT.CATEGORY')
         IF (NF.GT.NFIELD_FORM) THEN
            CATEGORY = 0
         ELSE
            CATEGORY = DBS_GETI(REF_FORM,NF)
         END IF
      ELSE
         NTARGET = 1
      END IF
 
*  Now go thru pages outputting to the .tex file
 
      CALL GETLUN(LUN_OUT)

      FILE_NAME = TEXFILE(:NC_TEX)//'.tex'
      RMFILE = 'N'
      NCHAR= MDH_ENDWORD(FILE_NAME)
      INQUIRE (FILE=FILE_NAME,EXIST=FILE_EXIST)
      IF (FILE_EXIST) THEN
        IF (RMFILE.EQ.'N') THEN
          WRITE(*,'(a,$)')' Warning - file '//FILE_NAME(1:NCHAR)//
     &      ' exists. Overwrite? (Y/N,Default is N):'
          READ (*, '(A)') RMFILE
        END IF

        IF (CHAR(ICHAR(rmfile) .AND. 95) .EQ. 'Y') THEN
          CALL DELFILE(FILE_NAME(1:NCHAR))
        ELSE
          GOTO 99
        END IF
      END IF

      OPEN(UNIT=LUN_OUT,FILE=FILE_NAME(1:NCHAR),
	1  CARRIAGECONTROL='list', STATUS='NEW', IOSTAT=IOSTATUS)
      IF (IOSTATUS.NE.0) THEN
         CALL FORM_ERR('Error Opening LaTex o/p File')
         STATUS = -1
         GOTO 99
      END IF
 
*  Get header stuff
      CALL GETLUN(LUN_IN)
      FILEIN  = 'rps_form_0.tex'
      OPEN(LUN_IN,FILE=dscfrps_data(:len_dscfrps)//FILEIN,
	1	STATUS='OLD',IOSTAT=IOSTATUS)
      IF (IOSTATUS.NE.0) THEN
         CALL FORM_ERR('No LaTeX i/p File ?')
         STATUS = -1
         GOTO 99
      END IF
      CALL GETDAT(DATE_TIME)
      WRITE(LUN_OUT,'(A/A/A)' ) '%', '% RPS Latex file ' // TEXFILE(:NC_TEX) // ' written ' // DATE_TIME, '%'
 
      DO WHILE (.TRUE.)				! Read whole of file
         READ(LUN_IN,'(A)',END=20) LINE
         NCHAR= MDH_ENDWORD (LINE)
         IF(NCHAR.GT.0) WRITE(LUN_OUT,'(A)') LINE(:NCHAR)
      END DO
20    CONTINUE
      CLOSE(LUN_IN)
      CALL FRELUN(LUN_IN)
 
      CALL LATEX_PAGE(1,LUN_OUT,FORM,FTYPE,STATUS)	! Cover page
      CALL LATEX_PAGE(2,LUN_OUT,FORM,FTYPE,STATUS)	! General form

	IF (ntarget .GE. 1) THEN

                CALL LATEX_PAGE(5,LUN_OUT,FORM,FTYPE,STATUS)	! target Summary
		do_page = .FALSE.
		targ_constr_page = .FALSE.
		targ_rem_page = .FALSE.
		n = 1

		DO WHILE (n .LE. ntarget .AND. .NOT. do_page)

			IF (FTYPE.NE.'BLANK') THEN
				CALL FORM_READ(REF_TARGET,n,IERR)
				IF(IERR.NE.0) GOTO 99
			ELSE
				DO_PAGE = .TRUE.
			END IF

			IF (FTYPE .NE. 'BLANK') THEN
				nf = DBS_FIELDNO(REF_TARGET,'TIME.CRITICAL')
				time_crit = .FALSE.
				time_crit = DBS_GETL(REF_TARGET,NF)
				IF (time_crit) targ_constr_page = .TRUE.
				nf = DBS_FIELDNO(REF_TARGET,'REMARKS(1)')
				tmp = ' '
				tmp = DBS_GETC(REF_TARGET, NF)

				DO i = 1,50
					IF (tmp(i:i) .GT. ' ') targ_rem_page
	1					= .TRUE.
				ENDDO

			END IF

			n = n + 1
		ENDDO

		IF (DO_PAGE) THEN

			DO sub_page = 3,4
				CALL LATEX_PAGE(SUB_PAGE,LUN_OUT,FORM,FTYPE,
	1				STATUS)

				IF (STATUS .EQ. 1) THEN
					STATUS = 0
					GOTO 50
				END IF

			ENDDO

		END IF

	ENDIF

50    CONTINUE
      IF (targ_constr_page) CALL LATEX_PAGE(3,LUN_OUT,FORM,FTYPE,STATUS)
      IF (targ_rem_page) CALL LATEX_PAGE(4,LUN_OUT,FORM,FTYPE,STATUS)

      IF (FTYPE .EQ. 'BLANK') THEN
	MTEXT(LOC_MSTATUS:) = ' Blank forms only created          '
      ELSE
        NCHAR= MDH_ENDWORD(FILE_NAME)
        MTEXT(LOC_MSTATUS:) = 'Latex forms on file '//FILE_NAME(1:NCHAR)
      END IF

      DO I=1,NEND
         WRITE(LUN_OUT,'(A)') BSLASH//END_STRING(I)
      END DO
      CLOSE(LUN_OUT)
      CALL FRELUN(LUN_OUT)
 
      STATUS = 0
 
 
99    CONTINUE
 
      END
