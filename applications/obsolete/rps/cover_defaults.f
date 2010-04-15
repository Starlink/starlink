*+COVER_DEFAULTS   Gets a 'default' record from another file
*******************************************************************************
*  History
*     1988 October	M Ricketts	1st Version
*     1992 April	M. Duesterhaus  remove VAX RTL calls
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1993 October	P. Brisco	Changed read statement to jive with
*					the correct open statement (80 bytes).
*     1996 Mar          M Ricketts      tidy filenames
*******************************************************************************
      SUBROUTINE COVER_DEFAULTS(DEFAULT_FILE,STATUS)

*  Type Declaration
      IMPLICIT NONE

*  Calling Arguments
      CHARACTER*(*) DEFAULT_FILE
      INTEGER STATUS

*  Global Variables
      INCLUDE 'com_form_files.inc'		! Holds File ref nos.
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_rec.inc'		! Holds Record

*-
*  Functions
      INTEGER DBS_FIELDNO, DBS_INFOI

*  Local Variables
      INTEGER LUN, nbytes, field_no, i, j, rectmp

*  Executable Code
      CALL GETLUN(LUN)				! Get a lun

      OPEN(LUN,FILE=DEFAULT_FILE, STATUS='OLD',			! Open the file
     &   ACCESS='DIRECT',RECL = 80, IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
         CALL FORM_ERR('Error opening Defaults File')
         GOTO 10
      END IF

      FIELD_NO = DBS_FIELDNO(REF_FORM,'NUMBER.OF.TARGETS')		! Locate end useful defaults
      NBYTES = DBS_INFOI(REF_FORM,FIELD_NO,'START') - 1
	rectmp = 1
	i = 1

	DO WHILE (i .LT. nbytes)

		IF (i + 79 .GT. nbytes) THEN
			j = nbytes
		 ELSE
			j = i + 79
		END IF

		READ(LUN,REC=rectmp,IOSTAT=STATUS) RECORD(REF_FORM)(i:j)
		i = j + 1
		rectmp = rectmp + 1
	END DO

      IF (STATUS.NE.0) THEN
         CALL FORM_ERR('Error reading Defaults File')
         GOTO 10
      END IF
      CLOSE(LUN)					! clear up
      CALL FRELUN(LUN)
      IF (STATUS.EQ.0) GOTO 20

10    CONTINUE
      STATUS = -1
20    CONTINUE

      END
