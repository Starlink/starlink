*+FORM_SUBMIT      Creates Proposal form submittable file, submits to UKDC.
      SUBROUTINE FORM_SUBMIT
 
*   Method : Codes the binary POP form file into ASCII characters. The 
*            reformatted file is then transmitted to a directory at the
*            UKDC across the JANET network.
 
*  History:
*     1988 Oct	M Ricketts	1st version
*          Dec	M.Bush 		Put in RPS
*     1989 Jan  M Ricketts	Add test mail output, when REF_FORM = 0
*          Jan  D Ewart         Cope with MAIL and POST syntax
*     1992 Jul  M Duesterhaus   remove VAX specific calls
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1993 Jun          P. Brisco       Removed SMG junque.
*     1994 JAN		M RICKETTS	RAL VERSION
*     1996 Mar          M Ricketts      Tidy Filenames
*************************************************************************
 
      IMPLICIT NONE

*   Global Variables:
 
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_rec.inc'
      INCLUDE 'com_dbs_iof.inc'
 
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_mtext.inc'
      INCLUDE 'com_form_ao.inc'
 
      INCLUDE 'com_form_points.inc'
      INCLUDE 'com_dbs_field.inc'
      INCLUDE 'com_dbs_bytes.inc'

      CHARACTER*3 RPS_VERSION
      COMMON /VERSION/ RPS_VERSION

*  Functions
      INTEGER DBS_FIELDNO
      INTEGER DBS_GETI, FIND_FIRST
      CHARACTER*60 DBS_GETC
      CHARACTER*128 FORM_GETC        
      INTEGER MDH_ENDWORD					! Gets length of string
      INTEGER POP_MENU
      INTEGER PROP_CHECK

*   Local Variables :
      CHARACTER*128 FILE_NAME					!Full file name
      INTEGER P							!Position of end character.                    
      INTEGER NCHAR						!Maximum number of characters on a line of the submittable file.
      INTEGER NREC						!Number of record being read in.
      INTEGER LN1,LN2						!Logical unit numbers.
      INTEGER J							!Number of character on line OUTREC being written.
      PARAMETER (NCHAR=80)
      CHARACTER*(NCHAR) OUTREC, STARLINE, BLANK			!Line written to output file.
      DATA BLANK/'**                                                                            **'/
      DATA STARLINE/'********************************************************************************'/

      CHARACTER*60 VALUE  
      CHARACTER*50 ADDRESS 					!Address for posting to.
      INTEGER LIMIT
      INTEGER LENGTH						!Length of address string.
      INTEGER CHECKSUM						!Checksum (equal to summation of data bytes).
      INTEGER REF_NO,ADDR
      INTEGER I						!Array counters.
      INTEGER STATUS						!Status flag on opening binary file.
      INTEGER FIELDNO						!Field number.
      INTEGER NO_TARGETS					!Number of targets.
      INTEGER END_BYTE						!Last byte in a record to be read
      INTEGER IERR						!Error flag
      CHARACTER*132 LINE					!Line of abstract file
      LOGICAL FILE_EXIST
      CHARACTER*1 RMFILE,QUOTE
      CHARACTER*18 CDATE
      CHARACTER*1 DUMMY
      CHARACTER*4 SUBMIT_TYPE
      INTEGER SUBMIT_STATUS, LENS,SSTART,ITEST,SUBINDEX,LOC	! Indicates if file can be sent
      INTEGER LEN1, LEN2, NCHABS, ICHAR, NLEFT, LOC_MTEXT, IOPT, TLEN
      INTEGER TARG_NOS(50), TNFIELD, char_count, char_sum, NTEXT, ITEXT
      CHARACTER*2 CHARTARG, CHARTARGNOS
      LOGICAL TESTSUBMIT /.FALSE./				! True for test transmission only
      CHARACTER*800 ABSTRACT, ABSLEN*64, HTEXT*192, AABS*3
      CHARACTER*12 PHEAD/' RPS submit '/
      CHARACTER*15 POPTION /'Submit proposal'/
      CHARACTER*60 TITLE, outext*100
 
* __________________________________________ Executable Code ________________________________________

      QUOTE = CHAR(39)
      LOC_MTEXT = INDEX(MTEXT, 'Status') + 10
      SUBMIT_STATUS = 0
      IF (REF_FORM .EQ. 0 .AND. FORM_FILE .EQ.'TESTMAIL') THEN	! See if test submission wanted; if not exit
         I = POP_MENU('Submission Test',1,'RPS Submit Form',-1,' ')
         IF (I.EQ.1) THEN
            TESTSUBMIT = .TRUE.
            P = 8
            GOTO 20						! Skip file access if Test
         END IF
      END IF
 
      CALL FORM_OPEN('R', IERR)
      IF (IERR .NE. 0) GOTO 99
      CALL DBS_READ(REF_FORM,1,IERR)				! Read cover page
      CHECKSUM = PROP_CHECK()					! Get checks

      IF (CHECKSUM .LE. 0 ) THEN
         CALL FORM_ERR('Checksum indicates form error')
         MTEXT(LOC_MTEXT:) = 'Checksum error                                           '
         GOTO 200
      END IF

      STATUS = 1						! See that abstract available and not too long
      CALL GETLUN(LN2)
      P = MDH_ENDWORD(FORM_FILE)
      OPEN(UNIT=LN2, FILE=FORM_FILE(:P)//'.abstract',
     &     STATUS='OLD', ACCESS='SEQUENTIAL', IOSTAT=STATUS)

      IF(STATUS .NE. 0) THEN
         CALL FORM_ERR( 'Abstract file '//FORM_FILE(:P)//'.abstract not found. ')
         SUBMIT_STATUS = -1
         MTEXT(LOC_MTEXT:) = 'Error opening abstract                                           '
         GOTO 200
      END IF

20    CONTINUE
      FILE_NAME = FORM_FILE
      FILE_NAME(P+1:P+5) = '.post'
      RMFILE = 'N'
      LENGTH= MDH_ENDWORD(FILE_NAME)
      INQUIRE (FILE=FILE_NAME,EXIST=FILE_EXIST)
      IF (FILE_EXIST) THEN
        IF (RMFILE.EQ.'N') THEN
          WRITE(*,'(XA$)')' Warning - file '//FILE_NAME(1:LENGTH)//
     &      ' exists. Overwrite? (Y/N,Default is N): '
          READ (*, '(A)') RMFILE
        END IF

        IF (CHAR(ICHAR(rmfile) .AND. 95) .EQ. 'Y') THEN
          CALL DELFILE(FILE_NAME(1:LENGTH))
        ELSE
          GOTO 200
        END IF
      END IF

      CALL GETLUN(LN1)
      
      OPEN(UNIT=LN1,FILE=FILE_NAME,STATUS='NEW')
 
      OUTREC = BLANK
      WRITE(OUTREC(18:62),'(A,I1)' ) 'Rosat (unix) Proposal Submission V'//RPS_VERSION//' for AO', AO_NUMBER
      WRITE(LN1,'(A/A/A)') STARLINE, BLANK,OUTREC
      CALL GETDAT(CDATE)
      OUTREC = BLANK
      OUTREC(3:3) = '!'
      write(OUTREC(17:62),'(A,I8)') 'Created '//CDATE//', Checksum =',CHECKSUM
      WRITE(LN1,'(A/A/A)') OUTREC, BLANK, STARLINE
 
      IF (TESTSUBMIT) THEN		 			! Line to indicate Test submission
         OUTREC = BLANK
         OUTREC(21:61) = 'Test Submission File - Please acknowledge'
         WRITE(LN1,'(A/A)') OUTREC,BLANK
         GOTO 60
      END IF
 
      FIELDNO = DBS_FIELDNO(REF_FORM,'NUMBER.OF.TARGETS')
      NO_TARGETS = DBS_GETI(REF_FORM,FIELDNO)

      TNFIELD = DBS_FIELDNO(REF_TARGET,'TARGET.NUMBER')
      DO I=1,NO_TARGETS
         TARG_NOS(I) = 0
      END DO
      END_BYTE = RECSIZE(1)
      REF_NO = REF_FORM
      DO NREC = 0, NO_TARGETS
         IF(NREC .GE. 1) THEN					! Read target
            CALL FORM_READ(REF_TARGET,NREC,IERR)
            IF (IERR .NE. 0) GOTO 99
            IF (NREC .EQ.1) THEN				! Switch to Targets from cover
               REF_NO = REF_TARGET
               END_BYTE = RECSIZE ( REF_TARGET )
            END IF

            I = DBS_GETI(REF_TARGET, TNFIELD )			! Check that target nos are sequential
            IF (I.GT.0 .AND. I.LE.NO_TARGETS ) THEN
               TARG_NOS(I) = TARG_NOS(I) + 1
            ELSE
               WRITE(CHARTARG, '(I2)' ) NREC
               CALL FORM_ERR('Error, target rec '//CHARTARG)
               MTEXT(LOC_MTEXT:) = 'Error in target rec '// CHARTARG// '                        '
               GOTO 200
            END IF
 
         END IF
 
         I=1
         IF (REF_NO.EQ.REF_FORM) THEN
           LIMIT = FLD_LIMS_GEN(2)
         ELSE
           LIMIT = FLD_LIMS_CONS(2)
         END IF
    
         DO WHILE (I.LE.LIMIT)
           IF (INDEX(FIELD(I,REF_NO),'$').EQ.1) THEN
             J=2
           ELSE
             J=1
           END IF
           FIELDNO = DBS_FIELDNO(REF_NO, FIELD(I,REF_NO)(J:))
           VALUE = DBS_GETC(REF_NO,FIELDNO)
           IF (NULFORMAT(I,REF_NO)(1:1).EQ.'A') THEN
              SSTART = 1
              ITEST = 1
              LENS = LENTH(I,REF_NO)
              DO WHILE (ITEST.NE.0)
                 ITEST = FIND_FIRST(VALUE(SSTART:LENS),QUOTE,SUBINDEX)
                 IF (ITEST .NE. 0) THEN
                   LOC = SSTART + ITEST -1
                   VALUE = VALUE(:LOC-1) //QUOTE//VALUE(LOC:LENS)
                   LENS = LENS +1
                   SSTART = LOC +2
                 END IF
              END DO
           END IF
           LEN1 = MDH_ENDWORD(FIELD(I,REF_NO))
	   LEN2 = MDH_ENDWORD(NULFORMAT(I,REF_NO))
           WRITE(outext,'(A)')' '//FIELD(I,REF_NO)(J:LEN1)//' ['//NULFORMAT(I,REF_NO)(1:LEN2)//']='
     &       //VALUE(1:LENTH(I,REF_NO))
           ntext = len1-j+6 +len2 +lenth(i,ref_no)
           WRITE(LN1,'(A)') OUTEXT(1:NTEXT)					! Output
           ntext = MDH_ENDWORD(OUTEXT)
           DO ITEXT=2,NTEXT							! useful characters
              CHAR_COUNT=CHAR_COUNT + 1
              CHAR_SUM = CHAR_SUM + ICHAR( OUTEXT(ITEXT:ITEXT))
           END DO
           I = I+1
         END DO
         WRITE(LN1,'(A)') STARLINE
      END DO
      OUTREC = BLANK								! checksum
      WRITE(OUTREC(25:53), '(I5,A,I8)') CHAR_COUNT, ' chars.   Sum = ', CHAR_SUM
      WRITE(LN1,'(A)') OUTREC

      DO NREC = 1, NO_TARGETS
         IF (TARG_NOS (NREC) .NE. 1 ) THEN
               WRITE(CHARTARG, '(I2)' ) NREC
               WRITE(CHARTARGNOS, '(I2)' ) TARG_NOS(NREC)
               CALL FORM_ERR('Error, target number(s) '//CHARTARG)
               MTEXT(LOC_MTEXT:) = 'Error, '//CHARTARGNOS//' targets numbered '// CHARTARG// '                        '
               GOTO 200
         END IF
      END DO
 
      WRITE(LN1,'(A)') STARLINE							       ! marks end records
      OUTREC = BLANK
      OUTREC(4:12) = 'Abstract:'
      WRITE(LN1,'(A)') OUTREC

      LEN1 = 0
      NCHABS = 0
      NLEFT = 800 - NCHABS
      DO WHILE ( NLEFT .GT. LEN1 )
         NCHABS = NCHABS + LEN1
         NLEFT = 801 - NCHABS						! Allow for not putting space on last line
         READ(LN2,'(A)',END = 15, ERR=10) LINE				! read 1st line
         LEN1 = MDH_ENDWORD (LINE)
         SSTART = 1
         ITEST = 1
         DO WHILE (ITEST.NE.0)
            ITEST = FIND_FIRST(LINE(SSTART:LEN1),QUOTE,SUBINDEX)
            IF (ITEST .NE. 0) THEN
              LOC = SSTART + ITEST -1
              LINE = LINE(:LOC-1) //QUOTE//LINE(LOC:LEN1)
              LEN1 = LEN1 +1
              SSTART = LOC +2
            END IF
         END DO

         WRITE (LN1,'(A)')LINE(:LEN1)
      END DO									! Copying lines into abstract
      IF (NLEFT.GT.0) THEN
         ABSTRACT(NCHABS+1:) = LINE(:LEN1)
      END IF
      CLOSE(LN2)
      CALL FRELUN(LN2)
*  Put out error message with last line of abstract
      CALL FORM_NOTIFY('Abstract too long','Ends with:'//ABSTRACT(750:800) )
      MTEXT(LOC_MTEXT:) = 'Error reading abstract                                           '
      GOTO 200

10    CONTINUE
      CLOSE(LN2)
      CALL FRELUN(LN2)
      CALL FORM_ERR('Error reading Abstract')
      MTEXT(LOC_MTEXT:) = 'Error reading abstract                                           '
      goto 200

15    CONTINUE							! File ended within 800 chars

      WRITE(AABS, '(I3)' ) NCHABS
      ABSLEN = 'Your abstract has '// AABS // ' characters                                '
      CLOSE(LN2)
      CALL FRELUN(LN2)

* Why is next line in here? - c'out 15/4/97
*      CALL FORMAT_ABSTRACT (form_file(:p), .TRUE.)

      WRITE(LN1,'(A)') STARLINE
 
      OUTREC = BLANK
      OUTREC(32:48) = 'RPS - End of File'
      WRITE(LN1,'(A)') OUTREC
60    CONTINUE								! Come here if Test Submission File
      WRITE(LN1,'(A)') STARLINE

90    CONTINUE
      CLOSE(LN1)
      CALL FRELUN(LN1)

      CALL GETLUN(ADDR)      
      OPEN(UNIT=ADDR,FILE=DSCFRPS_DATA(:len_dscfrps)//
	1	'address.dis',TYPE='OLD',IOSTAT=IERR)
      IF (IERR.NE.0) THEN
        WRITE(*,*) ' RPS - address.dis file is not accessible '
      ELSE
        READ(ADDR,'(A)' ) ADDRESS
        READ(ADDR,'(A4)') SUBMIT_TYPE
      END IF

      CLOSE(UNIT=ADDR)
      CALL FRELUN(ADDR)

      LENGTH = MDH_ENDWORD(ADDRESS)

      HTEXT(:64)     = 'The proposal has been put into '// FILE_NAME(:P+5)//'                        '
      HTEXT(65:128)  = 'If you select submit it will be sent '
      HTEXT(129:192) = 'To '//ADDRESS //'                                                            '

      IOPT =  POP_MENU(POPTION, 1, PHEAD, -1, HTEXT //ABSLEN )
      IF (IOPT .EQ.1) THEN

!	Ed. Jan 1989 by D Ewart to cope with different 'POST' and 'MAIL' syntax.
!	POST <address> <filename>
!	MAIL <filename> <address>

        FIELDNO=DBS_FIELDNO(REF_FORM,'PROPOSAL.TITLE(1)')
        TITLE=DBS_GETC(REF_FORM,FIELDNO)
	TLEN = MDH_ENDWORD(TITLE)

	IF(SUBMIT_TYPE.EQ.'POST')THEN
	  CALL MDH_COMM('mail '//ADDRESS(1:LENGTH)//' < '//FILE_NAME(1:P+5) ,'SL')
        ELSE 
  	  CALL MDH_COMM('mail/subject="'//TITLE(1:60)//'" '//FILE_NAME(1:P+5)
     c 		//' "@'//DSCFRPS_DATA(:len_dscfrps)//'address.dis"','SL')
 	END IF

         DUMMY = FORM_GETC(' Form has been sent. Press return to continue.',' ')
      END IF
      goto 100
 
99    CONTINUE
         CALL FORM_ERR('Error reading Form File')
        MTEXT(LOC_MTEXT:) = 'Error reading files in Submit                          '
100   CONTINUE
        MTEXT(LOC_MTEXT:) = 'File '//FILE_NAME(:P+5)//' created                                       '

200   CONTINUE

      END
