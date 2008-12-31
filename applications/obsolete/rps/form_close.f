*+FORM_CLOSE       Closes database files
*  History
*       1989 Jan 	M Ricketts		1st vsn 
*	8 Apr 1992	M. Duesterhaus (GSFC)	port to UNIX
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1993 Jun          P. Brisco               Removed SMG junque.
*     1994 Jan		M Ricketts		RAL again!
**************************************************************************
      SUBROUTINE FORM_CLOSE
      IMPLICIT NONE

 
*  Global Variables
      INCLUDE 'com_form_files.inc'
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_iof.inc'
      INTEGER NCHAR_DEF
      CHARACTER*128 DEFAULT_FILE, FILL_TYPE*3
      COMMON /DEFILE/ NCHAR_DEF,DEFAULT_FILE, FILL_TYPE

*  Function
      INTEGER POP_MENU
*-
*  Local Variables
      INTEGER LOPT
      CHARACTER*16  HEADING/'RPS - file close'/
      CHARACTER*16 CLOSE_QUERY/'Delete new files'/
      CHARACTER*1 CLOSE_TYPE		! 'E' if to delete old, 'Q' for new
      CHARACTER*110 MESSAGE		!  Screen message
     &   /'Default is to delete the old files, keep new ones               Option 1 deletes the new files, keeps old ones'/
 
* _____________________________ Executable Code _______________________________


      IF (REF_FORM .LE.0 .AND. REF_TARGET .LE.0) GOTO 10			! No files open

      IF (OPEN_TYPE(REF_FORM).NE. OPEN_TYPE(REF_TARGET) ) THEN
         CALL FORM_ERR('Files screwed up?')
         CLOSE_TYPE = 'E'
      ELSE IF (OPEN_TYPE(REF_FORM) .EQ. 'D') THEN					! Descriptor files only used
         GOTO 10
      ELSE IF (OPEN_TYPE(REF_FORM) .EQ. 'R') THEN
         CLOSE_TYPE = 'R'
      ELSE IF ( OPEN_TYPE(REF_FORM) .EQ.'C') THEN
         CLOSE_TYPE = 'E'
      ELSE
         LOPT = POP_MENU(CLOSE_QUERY, 1, HEADING, -2, MESSAGE )
         IF (LOPT.EQ.2) THEN
            CLOSE_TYPE = 'E'
         ELSE
            CLOSE_TYPE = 'Q'
         END IF
      END IF
      CALL DBS_CLOSE(REF_FORM, CLOSE_TYPE)
      CALL DBS_CLOSE(REF_TARGET, CLOSE_TYPE)

10    CONTINUE
      END
