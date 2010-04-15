*+LTX_WRITE_RADEC  Writes RA and DEC to .tex file.
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*---------------------------------------------------------------------------
      SUBROUTINE LTX_WRITE_RADEC(IFILE,FIELD,LUN_OUT,I)

*  Type Declaration
      IMPLICIT NONE

*  Calling Arguments
      INTEGER IFILE
      CHARACTER*(*) FIELD
      INTEGER LUN_OUT
      INTEGER I							!Field number.

*  Global Variables
      INCLUDE 'com_form_latex.inc'
      INCLUDE 'com_form_files.inc'

      INTEGER CHECKSUM
      COMMON /KEEPCHECK/ CHECKSUM
*******************************************************************************
*  History
*     1988 November	M Bush		1st Version
*     1989 Jan		M Ricketts	Mods for split form
*******************************************************************************
*-
*  Functions
      INTEGER MDH_ENDWORD
      INTEGER MDH_CTOI
      CHARACTER*60 DBS_GETC                            ! Retrieves the value of a character field

*  Local Parameters
      CHARACTER*5  S1/'put('/
      CHARACTER*24 S2/'makebox(0,0)[bl]{'/
      CHARACTER*2  S3/'}}'/

*  Local Variables
      INTEGER IP, ISX, LENGTH
      INTEGER SIGNST, START, LSTATUS, FINISH
      CHARACTER*11 TARG
      DOUBLE PRECISION RADS
      REAL R2D
      CHARACTER*128 LINE
      CHARACTER*6 SX
      CHARACTER*1 SIGN

      PARAMETER(R2D=180.0/3.1415926536)

*  Executable Code

      TARG = DBS_GETC(REF_TARGET,I)

      IF(FIELD.EQ.'TARGET.DEC')THEN					!Write sign to LATEX file.
         CALL DEC_CONVERT(TARG, RADS, LSTATUS)
         IF (RADS.LT.0) THEN
           SIGN = '-'
         ELSE
           SIGN = '+'
         END IF

         ISX = MDH_CTOI(XSTART(I,1,IFILE))
         ISX=ISX-22
         WRITE(SX,'(I3)')ISX
         LINE=BSLASH//S1//SX//','//YSTART(I,1,IFILE)//
     &    '){'//BSLASH//S2//BSLASH//'tt '//SIGN//S3
         LENGTH=MDH_ENDWORD(LINE)
         WRITE(LUN_OUT,'(A)')LINE(:LENGTH)
         SIGNST = 1
      ELSE
         SIGNST = 0
      END IF

      START = 1 + SIGNST
      FINISH = START +1
      DO IP=1,3							!Write angle to LATEX file.
         LINE=BSLASH//S1//XSTART(I,IP,IFILE)//','//YSTART(I,IP,IFILE)//
     &    '){'//BSLASH//S2//BSLASH//'tt '//TARG(START:FINISH)//S3
         LENGTH=MDH_ENDWORD(LINE)
         WRITE(LUN_OUT,'(A)')LINE(:LENGTH)
         START = START +3
         IF (IP.EQ.2) THEN
           FINISH = START+3
         ELSE
           FINISH = START + 1
         END IF
      END DO


      END
