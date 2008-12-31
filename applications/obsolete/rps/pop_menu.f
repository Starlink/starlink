*+POP_MENU         Gives choice of Screen or Listed MENU
********************************************************************************
*  History
*     1987 May	Mark Harris	1st version
*     1988 Oct	M Ricketts	Put in Initial Cursor setting
*     1989 Jan	   ::		Change line mode code
*     1992 Apr  M. Duesterhaus	remove VAX RTL calls
*     1993 Jun	P. Brisco	Recompile with new com_form_files.inc
*     1993 Jul	P. Brisco	Called FRELUN so we don't get core dump.
********************************************************************************
      INTEGER FUNCTION POP_MENU(TITLE,NUM,HEADING,INIT_CURS,MTEXT)
      IMPLICIT NONE

*INPUT:

      CHARACTER*(*) TITLE(*)			! Option names
      INTEGER       NUM				! Number of Options
      CHARACTER*(*) HEADING			! Header
      INTEGER       INIT_CURS			! Initial Cursor Option
      CHARACTER*(*) MTEXT			! Text for adding to menu


* Local Variables
      INTEGER  I, NUMOPTS, STATUS, START, END_TEXT, LOC, NLINES, LEN
 
*  Functions
      INTEGER MDH_ENDWORD

*  Executable Code
 
                        
        NUMOPTS = NUM + 1
        WRITE( * , '( / 5X,A)' ) HEADING

        END_TEXT = MDH_ENDWORD ( MTEXT)
        IF (END_TEXT .GT.0) THEN
         START = 1
         NLINES = ( END_TEXT - 1 ) / 64
         DO I=1, NLINES
            LOC = START + 63
	    LEN = MDH_ENDWORD(MTEXT(START:LOC))
            IF (LEN.GT.0) WRITE(*,'(3X,A)') MTEXT(START:LOC)
            START = START + 64
         END DO
         WRITE(*,'(3X,A)') MTEXT(START:END_TEXT)
        END IF

        WRITE( *, * )
        WRITE( * , '(4X,I2,A)' ) ( I , ' - '//TITLE( I ) , I = 1 , NUM ), NUMOPTS , ' - Return/Exit'

10       CONTINUE
         CALL H_GETI('Which option',.TRUE., NUMOPTS, POP_MENU, STATUS)

	 IF (status .GT. 0) THEN
	    POP_MENU = status + 100
         ELSE IF (STATUS .NE. 0) THEN
            POP_MENU = NUMOPTS
         ELSE
            IF (POP_MENU.LE.0 .OR. POP_MENU.GT.NUMOPTS) GOTO 10
         END IF

      END
