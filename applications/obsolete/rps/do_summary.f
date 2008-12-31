*+DO_SUMMARY       Does a summary of the Proposal
*   Aug 1992	M. Duesterhaus	Remove VAX specific code
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*******************************************************************
      SUBROUTINE DO_SUMMARY
 
      IMPLICIT NONE
 
*  Global Variables
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_mtext.inc'
 
      LOGICAL SMG			! True if smg mode
      COMMON /SMG_KEEP/ SMG
      INTEGER PBID			! Pasteboard Ident
      COMMON / SMG_PANDK / PBID
 
      INTEGER WIDTH
      COMMON /WIDTH_KEEP/ WIDTH
 
*  Functions
      INTEGER MDH_ENDWORD
      LOGICAL MDH_GETL
 
*  Local Variables
      INTEGER LENGTH
      INTEGER LUN
      LOGICAL ANS
      CHARACTER*128 FILE_NAME
      LOGICAL FILE_EXIST
      CHARACTER*1 RMFILE

*  _______________________Executable Code ______________________________________
  
      ANS = MDH_GETL('List here (else creates file)','Yes')
      IF (ANS) THEN
            LUN = 0
            CALL SUMMARISE(ANS,LUN)
         MTEXT(LOC_MSTATUS:) = 'Summary listed.                             '
      ELSE

	 CALL GETLUN (LUN)

         FILE_NAME = FORM_FILE(:LEN_FORM_FILE) //'.lis'
         RMFILE = 'N'
         LENGTH= MDH_ENDWORD(FILE_NAME)
         INQUIRE (FILE=FILE_NAME,EXIST=FILE_EXIST)
         IF (FILE_EXIST) THEN
           IF (RMFILE.EQ.'N') THEN
             WRITE(*,*)' Warning - file '//FILE_NAME(1:LENGTH)//
     &      ' exists. Overwrite? (Y/N,Default is N):'
             READ (*, '(A)') RMFILE
           END IF

         IF (CHAR(ICHAR(rmfile) .AND. 95) .EQ. 'Y') THEN
             CALL DELFILE(FILE_NAME(1:LENGTH))
           ELSE
             GOTO 100
           END IF
         END IF

         OPEN(UNIT=LUN, FILE= FILE_NAME(:LEN_FORM_FILE+4), FORM='FORMATTED',
     &      STATUS = 'NEW', ERR=90 )

         CALL SUMMARISE(ANS,LUN)
         CLOSE(LUN)
	 CALL FRELUN(LUN)
         MTEXT(LOC_MSTATUS:) = 'Summary written to '//FILE_NAME(:LEN_FORM_FILE+4) 
     &      // '                                  '
 
      END IF
      GOTO 100
90    CONTINUE
      MTEXT(LOC_MSTATUS:) = 'Error opening summary file                     '
100   CONTINUE
  
      END
