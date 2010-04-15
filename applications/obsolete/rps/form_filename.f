*+FORM_FILENAME    Gets Form Generic Filename
*	DATE		AUTHOR			DESCRIPTION
*	???		RAL			original
*	8 Apr 1992	M. Duesterhaus (GSFC)	remove VAX RTL calls
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1993 Jun          P. Brisco               Removed SMG junque.
*     1996 Mar          M Ricketts              Tidy filenames
*************************************************************************
      SUBROUTINE FORM_FILENAME

      IMPLICIT NONE

*  Global Variables
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_mtext.inc'			! Text for main menu

*  Functions
      INTEGER MDH_ENDWORD
      CHARACTER*128 FORM_GETC

*  Local Variables
      INTEGER DOT, BRAC, I, LOC, LENTARG

*  _______________________Executable Code ______________________________________


      FORM_FILE(1:1) = '?'
      DO WHILE (FORM_FILE(:1) .EQ. '?' )
         FORM_FILE = FORM_GETC(
     & ' Enter Proposal Filename', FORM)
         IF (FORM_FILE(:1) .EQ. '?' ) THEN
            WRITE(*,*) ' This is the <propname> referred to in the user-guide section 1.3 -'
            WRITE(*,*) ' if you only want to check a target, just enter <return>.'
         END IF
      END DO
      LEN_FORM_FILE = MDH_ENDWORD(FORM_FILE)

      FORM = FORM_FILE(1:LEN_FORM_FILE)
      FORM_TARGET = FORM_FILE(1:LEN_FORM_FILE)//'_target'
      LENTARG = MDH_ENDWORD(FORM_TARGET)

*  Clear text space and put in info
      DO I=1,256						! use text as 4 lines of 64 chars, but filename line can overflow
         MTEXT(I:I) = ' '
      END DO
      WRITE(MTEXT(129:), '(A)') 'Files  '//
     &   FORM_FILE(:LEN_FORM_FILE)//',  '//
     &   FORM_TARGET(:LENTARG)
      LOC = MDH_ENDWORD(MTEXT) + 3
      IF (LOC.LT.193 ) THEN
         WRITE( MTEXT(193:), '(A)' ) 'Status     Not open yet.'
         LOC_MSTATUS = 203
      ELSE
         WRITE( MTEXT(LOC:), '(A)' ) 'Status     Not open yet.'
         LOC_MSTATUS = LOC + 10
      END IF
 110  END
