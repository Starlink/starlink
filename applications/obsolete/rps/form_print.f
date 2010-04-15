*+FORM_PRINT       Prints Rosat Proposals
*   Aug 1992	M. Duesterhaus	Remove VAX specific code
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1993 June         P. Brisco       Got rid of SMG stuff.
***********************************************************************
      SUBROUTINE FORM_PRINT(STATUS)
      IMPLICIT NONE

*  Calling Arguments
      INTEGER STATUS			! Exit Status

*  Global Variables
      INCLUDE 'com_form_points.inc'
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_mtext.inc'
      INCLUDE 'com_form_latex.inc'

*  Function

* Local Variables
      CHARACTER*1 OPEN_REQ

*  Print Selection
      CHARACTER*23 PHEADING /'      Print Forms      '/
      CHARACTER*20 POPTIONS(3) /'All Forms, LaTeX', 'Selected Pages,LaTeX',
     &'Blank Forms, LaTeX'/

c	'Resubmit LaTeX',

*  Error Info
      CHARACTER*21 LHEADING /' LaTeX not Available '/
      CHARACTER*20 LOPTIONS /'Continue'/
      CHARACTER*23 ERRMESS/'Record read error      '/

      INTEGER     POP_MENU, LPRINT


*  Executable Code

      STATUS = 0
      DSCF_PRINT_GOT= .FALSE.
      LPRINT = POP_MENU(POPTIONS, 3,PHEADING,-1,' ')

      IF (REF_FORM .LE.0 ) THEN
         IF (LPRINT .EQ.3) THEN
            OPEN_REQ = 'D'
         ELSE
            OPEN_REQ = 'R'
         END IF
         CALL FORM_OPEN(OPEN_REQ, STATUS)
         IF (STATUS.NE.0) THEN
            GOTO 90
         END IF
         IF (LPRINT .NE. 3) THEN
            CALL FORM_READ(REF_FORM,1,STATUS)
            IF (STATUS.NE.0) THEN
               MTEXT(LOC_MSTATUS:) = 'Error reading files                                    '
               GOTO 90
            END IF
         END IF
      END IF

      IF (LPRINT .EQ.1) THEN			! Print all Pages, LaTeX

         CALL FORM_LATEX( 'ALL' , STATUS)

      ELSE IF (LPRINT.EQ.2) THEN		! Select

         CALL FORM_LATEX( 'SELECT', STATUS)

      ELSE IF (LPRINT.EQ.3) THEN		! Blank Forms

         CALL FORM_LATEX( 'BLANK', STATUS)

      END IF

90    CONTINUE

      END
