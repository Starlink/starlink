*+LTX_DSCF_INFO    Reads in latex info from dscf file
*---------------------------------------------------------------------------
      SUBROUTINE LTX_DSCF_INFO(STATUS)
 
*  Type Declaration
      IMPLICIT NONE
 
*  Calling Arguments
      INTEGER STATUS			! Status Flag, 0 = OK
 
*  Global Variables
      INCLUDE 'com_form_latex.inc'
      INCLUDE 'com_form_files.inc'
 
*******************************************************************************
*  History
*     1988 October	M Ricketts	1st Version
*     1989 Jan		::		Mods for split form
*     1992 Aug		M. Duesterhaus  remove VAX specific calls
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1993 Sep		P. Brisco	Redefined the line variable from 132
*					to 200 characters.
*     1994 Jan		M Ricketts	RAL version
*     1996 Mar		M Ricketts	Tidy Filenames
****************************************************************************** 
*-
*  Functions
      INTEGER DBS_INFOI
      INTEGER MDH_CTOI, MDH_ENDWORD
 
*  Local Variables
      INTEGER IDSCF, SIZE, NFIELD_READ, P1, P2, IERR, SSTART
      INTEGER SIZE_LAST_ADDED, IFILE, NCHAR, NFIELD, NEXTCOLON
      CHARACTER*132 DSCF_FILE
	character*200 line
 
*  Executable Code
 
      NFIELD_FORM = DBS_INFOI(REF_FORM,1,'NFIELDS')
      NFIELD = NFIELD_FORM
      NFIELD_TARGET = DBS_INFOI(REF_TARGET,1,'NFIELDS')
 
      CALL GETLUN (IDSCF)
 
      DSCF_FILE = 'dscfrps_form'
 
* Loop for cover, target files
 

      DO IFILE = 1,2
         OPEN(UNIT=IDSCF,FILE=dscfrps_data(:len_dscfrps)//DSCF_FILE,
	1	STATUS='OLD',IOSTAT=IERR)
         IF (IERR.NE.0) THEN
            STATUS = -1
            GOTO 99
         END IF
 
         LINE = '  '
         NFIELD_READ = 1

         DO WHILE (LINE(2:2) .NE. 'E')
            READ(IDSCF,'(A)') LINE
            SSTART = INDEX(LINE,':') + 1
            NEXTCOLON = INDEX( LINE(SSTART:),':')
            IF ( NEXTCOLON .LE. 4) THEN
               SSTART = SSTART + NEXTCOLON
            END IF
            IF (LINE(2:2) .EQ.'F') THEN
               SIZE = 1
            ELSE IF (LINE(2:2) .EQ. 'G') THEN
               P1 = INDEX(LINE,'(') + 1
               P2 = INDEX(LINE,')') - 1
               SIZE = MDH_CTOI(LINE(P1:P2))
            ELSE
               SIZE = 0
            END IF
 
            IF (SIZE.GT.0) THEN
               SIZE_LAST_ADDED = SIZE
               CALL LTX_DECODE_DSCF(LINE(SSTART:),SIZE,NFIELD_READ,IFILE)
            END IF
 
         END DO
         CLOSE (IDSCF)
 
         NFIELD_READ = NFIELD_READ - 1
         IF (NFIELD.NE.NFIELD_READ) THEN
            CALL FORM_ERR('Error reading DSCF')
            STATUS = -1
            GOTO 99
         END IF
 
         NCHAR = MDH_ENDWORD(DSCF_FILE)
         NFIELD = NFIELD_TARGET
         DSCF_FILE = 'dscfrps_form_target'
      END DO					! End loop for each file
      DSCF_PRINT_GOT = .TRUE.			! Flag to say we got info
 
      STATUS = 0
99    CONTINUE
      END
