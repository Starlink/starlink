*+LTX_ABSTRACT     Gets the Abstract File into a box
*  Aug 1992	M. Duesterhaus	Remove VAX specific code
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*****************************************************************
      SUBROUTINE LTX_ABSTRACT(LUN_OUT,STATUS)
      IMPLICIT NONE

*  Calling Arguments
      INTEGER LUN_OUT		!	Unit to write tex stuff
      INTEGER STATUS		! Out	0 = OK, else -1

*  Global Variables
      INCLUDE 'com_form_latex.inc'
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_mtext.inc'

*  Functions
      INTEGER MDH_ENDWORD

*  Local Variables
      INTEGER IOSTATUS, NCHAR, LUN, LEN1, NCHABS, NLEFT
      CHARACTER*16 NOFILE/'No Abstract File'/
      CHARACTER*128 LINE
      CHARACTER*800 ABSTRACT

*  Open File
      CALL GETLUN(LUN)
      NCHAR = MDH_ENDWORD(FORM_FILE)
      OPEN(LUN,FILE=FORM_FILE(:NCHAR)//'.ABSTRACT',STATUS='OLD',IOSTAT=IOSTATUS)

      WRITE(LUN_OUT,'(A)') BSLASH//'put(30,55) {'//BSLASH//'parbox[t] {150mm}{'
      IF (IOSTATUS.NE.0) THEN
         WRITE(LUN_OUT,'(A)') NOFILE
      ELSE

      READ(LUN,'(A)',END=20,ERR=10) LINE				! read 1st line
      LEN1 = MDH_ENDWORD (LINE)
      IF(NCHAR.GT.0) WRITE(LUN_OUT,'(A)') LINE(:LEN1)

      NCHABS = 0
      NLEFT = 800 - NCHABS
      DO WHILE ( NLEFT .GT. LEN1 )
         ABSTRACT(NCHABS+1:) = LINE(:LEN1)
         NCHABS = NCHABS + LEN1
         NLEFT = 801 - NCHABS						! Allow for not putting space on last line
         READ(LUN,'(A)',END = 10, ERR=10) LINE				! read 1st line
         LEN1 = MDH_ENDWORD (LINE)
         IF(NCHAR.GT.0) WRITE(LUN_OUT,'(A)') LINE(:LEN1)
      END DO									! Copying lines into abstract
      IF (NLEFT.GT.0) THEN
         ABSTRACT(NCHABS+1:) = LINE(:LEN1)
      END IF
*  Put out error message with last line of abstract
      CALL FORM_NOTIFY('Abstract too long','Ends with:'//ABSTRACT(750:800) )
      MTEXT(LOC_MSTATUS:) = 'Error reading abstract                                           '

c         DO WHILE (.TRUE.)
c            READ(LUN,'(A)',END=10) LINE
c            NCHAR = MDH_ENDWORD(LINE)
c            IF(NCHAR.GT.0) WRITE(LUN_OUT,'(A)') LINE(:NCHAR)
c         END DO
10       CONTINUE
      END IF

      WRITE(LUN_OUT,'(A)') '} }'
      CLOSE(LUN)
      CALL FRELUN(LUN)

      STATUS = 0

20    CONTINUE

      END
