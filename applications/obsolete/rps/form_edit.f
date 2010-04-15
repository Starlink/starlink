*+FORM_EDIT        Does Form editing, deleting, etc.
      SUBROUTINE FORM_EDIT(STATUS)

*  Type Declaration
      IMPLICIT NONE

*  Calling Arguments
      INTEGER STATUS			! Exit status, 0 = OK

*  Global Variables
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_points.inc'
      INCLUDE 'com_form_qual.inc'
      INCLUDE 'com_form_mtext.inc'

*******************************************************************************
*  History
*     1988 December	M Ricketts	1st Version
*     1989 Jan		   ::		Revised for split Form
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1993 June         P. Brisco       Got rid of SMG stuff.
*     1994 Jan		M Ricketts	RAL version
*******************************************************************************
*-
*  Functions
      CHARACTER*128 FORM_GETC
      INTEGER POP_MENU
      INTEGER DBS_GETI

*  Local Variables
      INTEGER NTARGET, KTARGET, IERR, EDTYPE, ISTART
      CHARACTER*6 TEXT, ETHEAD*27
      CHARACTER*3 CNUM
      INTEGER NOPT, MENU_CURS, RECN
      PARAMETER(NOPT=5)
      CHARACTER*20 OPTIONS(NOPT)/'Edit cover page',
     &     'Edit general page', 'Edit target data', 'Add target',
     &     'Review targets'/
      CHARACTER*26 EHEADING /'RPS - Edit Forms,   target'/

* ___________________________ Executable Code __________________________________

      STATUS = 0

      CALL FORM_OPEN('W',STATUS)					! Open file if necessary, find how many targets
      IF (STATUS .NE.0 ) GOTO 99

      RECN=1
      CALL FORM_READ(REF_FORM,RECN,IERR)					! This also sets up Qual_cover
      IF (IERR.NE.0) GOTO 99
      MTEXT(LOC_MSTATUS:) = ' Backup made, files opened         '
      NTARGET = DBS_GETI(REF_FORM,FLD_NTARGETS)
      MENU_CURS = 1
10    CONTINUE								! Come back here if an error occurs
      WRITE(EHEADING(18:19),'(I2)') NTARGET
      IF (NTARGET .EQ.1) THEN
         ETHEAD = EHEADING//' '
      ELSE
         ETHEAD = EHEADING//'s'
      END IF
      EDTYPE = POP_MENU(OPTIONS,NOPT,ETHEAD,-MENU_CURS,MTEXT)		! '-' puts 'return' instead of 'exit'
      MENU_CURS = NOPT + 1
      IF (EDTYPE.GT.NOPT) GOTO 99

      IF (EDTYPE.EQ.1) THEN			! Edit cover page

         CALL FORM_REFILL('COVER',0,STATUS)
         IF (STATUS.NE.0) GOTO 99
         RECN = 1
         CALL FORM_WRITE(REF_FORM,RECN,IERR)
         MTEXT(LOC_MSTATUS:) = ' Cover edited                     '
         IF (IERR.EQ.0) GOTO 10

      ELSE IF (EDTYPE.EQ.2) THEN		! Edit General Page

         CALL FORM_REFILL('GEN',0,STATUS)
         IF (STATUS.NE.0) GOTO 99
         RECN = 1
         CALL FORM_WRITE(REF_FORM,RECN,IERR)
         MTEXT(LOC_MSTATUS:) = ' General page edited                  '
         IF (IERR.EQ.0) GOTO 10

      ELSE IF (EDTYPE.EQ.3) THEN		! Edit Target

         IF (NTARGET.GT.1) THEN
            KTARGET = NTARGET + 1
            DO WHILE (KTARGET.LE.0 .OR. KTARGET .GT. NTARGET)
               TEXT = FORM_GETC('Target Record No.',' ')//',    '
               ISTART = 1
               CALL SLA_INTIN(TEXT,ISTART,KTARGET,IERR)
               IF (KTARGET .GT. NTARGET .OR. KTARGET.EQ.0 .OR. IERR.NE.0) THEN
                  TEXT = FORM_GETC('Not allowed! <ret> to continue',' ')
               END IF
            END DO
         ELSE IF (NTARGET.EQ.0) THEN
	    TEXT = FORM_GETC('No Targets - Must add target first, <ret> to continue',
     c		' ')
	    GOTO 10
         ELSE
            KTARGET = 1
         END IF
         QTARGET = KTARGET
         CALL FORM_REFILL('TARGET',KTARGET,STATUS)
         IF (STATUS.NE.0) GOTO 99
         CALL FORM_WRITE( REF_TARGET, KTARGET, IERR)
         WRITE( MTEXT(LOC_MSTATUS:) ,'(A,I2,A)' ) 'Target ',KTARGET, ' edited         '
         IF (IERR.EQ.0) GOTO 10

      ELSE IF (EDTYPE.EQ.4) THEN		! Add Target

         KTARGET = NTARGET + 1
         QTARGET = KTARGET
         CALL FORM_REFILL('TARGET',-KTARGET,STATUS)
         IF (STATUS .NE. 0) GOTO 99
         NTARGET = NTARGET + 1
         WRITE(CNUM,'(I3)')NTARGET
         CALL DBS_PUTC(REF_FORM,FLD_NTARGETS,CNUM,IERR)
         RECN = 1
         CALL FORM_WRITE(REF_FORM,RECN,IERR)
         IF (IERR .NE.0) GOTO 99

         CALL FORM_WRITE(REF_TARGET,KTARGET,IERR)
         WRITE( MTEXT(LOC_MSTATUS:) ,'(A,I2,A)' ) 'Target ',KTARGET, ' added          '
         IF (IERR.EQ.0) GOTO 10

      ELSE IF (EDTYPE.EQ.5) THEN		! Review Target List

         CALL TARG_REVIEW
         MTEXT(LOC_MSTATUS:) = '                                                            '
         NTARGET = DBS_GETI(REF_FORM,FLD_NTARGETS)
         GOTO 10

      END IF

99    CONTINUE
      IF (IERR.NE.0) MTEXT(LOC_MSTATUS:) = 'Error editing                                  '

      END
