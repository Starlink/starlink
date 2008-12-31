*+LATEX_PAGE       Outputs one copy of the given page
      SUBROUTINE LATEX_PAGE(IPAGE,LUN_OUT,FILE,FTYPE,STATUS)
 
*  Type Declaration
      IMPLICIT NONE

	real x
	integer*4 y 
*  Calling Arguments
      INTEGER IPAGE
      INTEGER TARGET
      INTEGER LUN_OUT
      CHARACTER*(*) FILE		! Output file name - use for i/p file
      CHARACTER*(*) FTYPE
      INTEGER STATUS			! Status, 0 = OK
 
*  Global Variables
      INCLUDE 'com_form_latex.inc'
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_points.inc'
      INCLUDE 'com_form_ao.inc'
 
*******************************************************************************
*  History
*     1988 October	M Ricketts	1st Version
*     1989 Jan				Version for split form
*     1991 Jan				Put AO number on each page
*     1991 Oct				Add target summary
*     1992 Aug		M. Duesterhaus  Remove VAX specific code
*     1992 Dec		M. Duesterhaus  Modify for Astro-D
*     1994 Jan		M Ricketts	RAL version
*******************************************************************************
*-
*  Functions
      CHARACTER*1 FORM_GETC
      INTEGER PROP_CHECK
      CHARACTER*8 MDH_ITOC
      INTEGER MDH_ENDWORD
      CHARACTER*60 DBS_GETC
      INTEGER DBS_FIELDNO
      INTEGER DBS_GETL
 
*  Local Variables
      INTEGER LUN_IN, IOSTATUS, NCHAR, FIELD_NO, ntargs
      CHARACTER*128 FILE_IN
      CHARACTER*132 LINE
      CHARACTER*2 PAGE
      CHARACTER*8 CHECKSUM
      CHARACTER*18 CDATE
      CHARACTER*7 TOT_TIME
      CHARACTER*1 DO_PAGE/'Y'/
      SAVE DO_PAGE
      CHARACTER*13 END_PICTURE/'end{picture}'/
      CHARACTER*17 ERROPT/'<ret> to continue'/
      LOGICAL MORE, MORE_SUMMARY
      INTEGER CON_TARG,REM_TARG, CON_COUNT, REM_COUNT
      SAVE CON_COUNT, REM_COUNT

*  Executable Code

      IF (STATUS.NE.0) GOTO 99
 
      IF (FTYPE.EQ.'SELECT' ) THEN
         IF (IPAGE.EQ.1) THEN
            DO_PAGE = FORM_GETC('Do you want Cover page printed','Y')
            WRITE(*,*)'        '
         ELSE IF (IPAGE.EQ.2) THEN
            DO_PAGE = FORM_GETC('Do you want General page printed','Y')
            WRITE(*,*)'        '
         ELSE IF (IPAGE.EQ.5) THEN
            DO_PAGE = FORM_GETC('Do you want Target summary printed','Y')
            WRITE(*,*)'        '
         ELSE IF (IPAGE.EQ.3) THEN
            DO_PAGE = FORM_GETC('Do you want Target constraints printed','Y')
            WRITE(*,*)'        '
         ELSE IF (IPAGE.EQ.4) THEN
            DO_PAGE = FORM_GETC('Do you want Target remarks printed','Y')
            WRITE(*,*)'        '
         END IF
         
         IF (DO_PAGE.EQ.'N' .OR. DO_PAGE.EQ.'n' .OR. DO_PAGE.EQ.'F' .OR. DO_PAGE.EQ.'f') GOTO 99
         IF (DO_PAGE.EQ.'E' .OR. DO_PAGE.EQ. 'e') THEN
            STATUS = 1
            GOTO 99
         END IF

      END IF					! 'SELECT' type
 
      MORE = .TRUE.
      CON_TARG = 1
      REM_TARG = 1
      IF ((IPAGE.EQ.3).AND.(CON_COUNT.EQ.0).AND.(FTYPE.EQ.'ALL')) THEN
	MORE = .FALSE.				! if 'select' will not have done count, so only if 'all'
      ELSE IF ((IPAGE.EQ.4).AND.(REM_COUNT.EQ.0).AND.(FTYPE.EQ.'ALL')) THEN
	MORE = .FALSE.
      END IF

      DO WHILE (MORE)						! ?
      CALL GETLUN (LUN_IN)

      WRITE(PAGE,'(A,I1)') '_', IPAGE
      NCHAR = MDH_ENDWORD (dscfrps_data)

      FILE_IN  = dscfrps_data(:NCHAR)//'rps_form'//PAGE//'.tex'
 
      OPEN(LUN_IN,FILE=FILE_IN,STATUS='OLD',IOSTAT=IOSTATUS)
      IF (IOSTATUS.NE.0) THEN
         CALL FORM_ERR('Error opening LaTeX source')
         STATUS = -1
         GOTO 99
      END IF
      DO WHILE (.TRUE.)				! Read whole of file
         READ(LUN_IN,'(A)',END=20) LINE
         NCHAR = MDH_ENDWORD (LINE)
         IF (NCHAR.GT.0) WRITE(LUN_OUT,*) LINE(:NCHAR)
      END DO

20    CONTINUE
 
C      IF ((IPAGE.EQ.1).OR.(IPAGE.EQ.2).OR.(IPAGE.EQ.6).OR.(IPAGE.EQ.7)) THEN
        LINE = BSLASH//'put(170,251){'//BSLASH//'framebox(20,10){'//
     &        BSLASH//'Large '//BSLASH//
     &        'bf AO-'//AO_CHAR//'}}'
C      ELSE IF ((IPAGE.EQ.5).OR.(IPAGE.EQ.3).OR.(IPAGE.EQ.4)) THEN
C        LINE = BSLASH//'put(160,246){'//BSLASH//'framebox(20,10){'//
C     &        BSLASH//'Large '//BSLASH//
C     &        'bf AO-'//AO_CHAR//'}}'
C      END IF

      NCHAR = MDH_ENDWORD(LINE)
      WRITE(LUN_OUT,*)LINE(:NCHAR)
      IF (FTYPE.NE. 'BLANK' )THEN
         CALL LATEX_DBS(IPAGE,LUN_OUT)
 
         IF (IPAGE.EQ.1) THEN
	    CALL DBS_GETTARG( NTARGET, TOT_TIME)
	    WRITE(LUN_OUT,'(A)') BSLASH//'put(150,73){'//BSLASH//
     &        'makebox(0,0)[bl]{'//TOT_TIME//'}}'
            WRITE(LUN_OUT,'(A)') BSLASH//'put(20,11){'//BSLASH//'line(1,0){170}}'
            WRITE(LUN_OUT,'(A)') BSLASH//'put(23,5){'//BSLASH//'makebox(0,0)[bl]{Proposal number}}'
            WRITE(LUN_OUT,'(A)') BSLASH//'put(90,5){'//BSLASH//'makebox(0,0)[bl]{Date}}'
            WRITE(LUN_OUT,'(A)') BSLASH//'put(125,5){'//BSLASH//'makebox(0,0)[bl]{File Checksum}}'
            WRITE(LUN_OUT,'(A)') BSLASH//'put(163,2.5){'//BSLASH//
     &        'makebox(0,0)[bl]{'//BSLASH//
     &        'footnotesize{Admin. use only}}}'
           CALL GETDAT(CDATE)
           WRITE(LUN_OUT,'(A)') BSLASH//'put(100,5){'//BSLASH//
     &       'makebox(0,0)[bl]{'//CDATE(11:18)//'}}'
            CHECKSUM = MDH_ITOC(PROP_CHECK())
            WRITE(LUN_OUT,'(A)') BSLASH//'put(153,5){'//BSLASH//
     &         'makebox(0,0)[bl]{' // CHECKSUM // '}}'
         END IF

         IF (IPAGE.EQ.5) CALL TARGET_SUMMARY(LUN_OUT, BSLASH, MORE_SUMMARY, CON_COUNT, REM_COUNT)
         IF (IPAGE.EQ.3) CALL CONSTRAINT_SUMMARY(LUN_OUT, BSLASH, MORE_SUMMARY)
         IF (IPAGE.EQ.4) CALL REMARKs_SUMMARY(LUN_OUT, BSLASH, MORE_SUMMARY)

      ELSE								! Blank page
         WRITE(LUN_OUT,'(A)') BSLASH//'put(40,227.5){'//BSLASH//
     &       'makebox(0,0)[tl]{'//BSLASH//
     &       'footnotesize{Title}}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(65,227.5){'//BSLASH//
     &        'makebox(0,0)[tl]{'//BSLASH//
     &        'footnotesize{First name}}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(100,227.5){'//BSLASH//
     &        'makebox(0,0)[tl]{'//BSLASH//
     &        'footnotesize{Middle name}}}'
         WRITE(LUN_OUT,'(A)') BSLASH//'put(125,227.5){'//BSLASH//
     &        'makebox(0,0)[tl]{'//BSLASH//
     &        'footnotesize{Last name}}}'

         IF (IPAGE.EQ.1) WRITE(LUN_OUT,'(A)') BSLASH//'put(50,60){'//
     &        BSLASH//'makebox(0,0)[bl]{(800 characters maximum)}}'
 
         IF (IPAGE .EQ.2) THEN
            WRITE(LUN_OUT,'(A)') BSLASH//'put(130,63){'//BSLASH//
     &         'makebox(0,0)[tl]{(delete TWO agencies)}}'
            WRITE(LUN_OUT,'(A)') BSLASH//'put(40,64){'//BSLASH//
     &         'makebox(0,0)[tl]{'//BSLASH//'large{NASA'//
     &          BSLASH//'hspace{10mm}BMFT'//BSLASH//'hspace{10mm}PPARC}}}'
         END IF
         IF (IPAGE .EQ. 5) THEN
c            WRITE(LUN_OUT,'(A)') BSLASH//'multiput(20,33)(0,10){11}{'//
c     &          BSLASH//'line(1,0){170}}'
         END IF

      END IF

      WRITE(LUN_OUT,'(A)') BSLASH//END_PICTURE
      STATUS = 0
      CLOSE(LUN_IN)
      CALL FRELUN (LUN_IN)
      IF ((IPAGE.EQ.1).OR.(IPAGE.EQ.2).OR. (FTYPE.EQ.'BLANK')) THEN
        MORE = .FALSE.
      ELSE IF ((.not. MORE_SUMMARY).AND.(IPAGE.EQ.5 .OR. IPAGE.EQ.4 .OR. IPAGE .EQ.3)) THEN
        MORE = .FALSE.
      ELSE IF ((NTARGET.LT.CON_TARG).AND.(IPAGE.EQ.3)) THEN
        MORE = .FALSE.
      ELSE
        MORE = .TRUE.
      END IF

      END DO 
99    CONTINUE
 
      END
