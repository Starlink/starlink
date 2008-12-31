*+FORM_INTRO       Writes intro message to terminal, from file RPS_INTRO
*	DATE		AUTHOR			DESCRIPTION
*	???		RAL			original
*	8 Apr 1992	M. Duesterhaus		remove VAX RTL calls
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
************************************************************************
      SUBROUTINE FORM_INTRO

      IMPLICIT NONE

      INCLUDE 'com_form_files.inc'

* Local Variables
      INTEGER INTRO, LEN, IERR
      CHARACTER*80 LINE

* Functions
      INTEGER MDH_ENDWORD

* _____ Executable code ________________________________________________________
      CALL GETLUN(INTRO)
         OPEN(UNIT=INTRO,FILE=DSCFRPS_DATA(:LEN_DSCFRPS)//'rps_intro.txt',
	1	TYPE='OLD',IOSTAT=IERR)
         IF (IERR.NE.0) THEN
            WRITE(*,*) ' Error - RPS not installed properly :'
         ELSE
            DO WHILE (.TRUE.)
               READ(INTRO,'(A)',END=10 ) LINE
               LEN = MDH_ENDWORD (LINE)
               WRITE(*,'(1X,A)' ) LINE(:LEN)
            END DO
10          CONTINUE

            CLOSE(UNIT=INTRO)
	    CALL FRELUN(INTRO)
         END IF
 
      END
