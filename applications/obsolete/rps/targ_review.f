*+TARG_REVIEW      Displays Target summary, makes changes
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*     1994 Jan		M Ricketts	Ral version
*---------------------------------------------------------------------------
      SUBROUTINE TARG_REVIEW
      IMPLICIT NONE

*  Global Variables
      INCLUDE 'com_form_files.inc'		! Filenames, ref nos.
      INCLUDE 'com_form_points.inc'
      INCLUDE 'com_form_qual.inc'

      INCLUDE 'com_target.inc'
c      DATA TARG_NO/90*0/
      DATA NTARGET /0/

      LOGICAL SMG
      COMMON /SMG_KEEP/SMG
      INTEGER PBID,KPID
      COMMON /SMG_PANDK/ PBID, KPID

      INCLUDE 'aaa_dbs_params.inc'		!
      INCLUDE 'com_dbs_iof.inc'		! Holds ARRAY, OPEN_TYPE

      CHARACTER*64 END_MESSAGE
      COMMON /EXITEXT/ END_MESSAGE

*  Functions
      INTEGER FORM_GECHEK
      INTEGER MDH_ENDWORD

*  Local Variables
      INTEGER K,L,IERR, NCHAR, NCOM, IREC, ITARG
      INTEGER RECN
      LOGICAL STARTED_DISPLAY
      CHARACTER*10 TEXT
      CHARACTER*12 TT/'    Targets '/
      CHARACTER*25 MESSAGE/'You only have    Targets!'/
      CHARACTER*38 MESS_CHNGE/'Format: ''C<irec>,<itarget>'' - re-enter'/
      CHARACTER*34 STRING
      CHARACTER*32 BLANK/'                                '/
      CHARACTER*3 CTARG
*  Assume width is 80 chars

*  Executable Code

      DO K=1,90
        TARG_NO(K) = 0
      END DO

      IF (REF_FORM .LE.0) THEN
         CALL FORM_OPEN('R', IERR)
         IF (IERR .NE. 0 ) GOTO 99
         RECN = 1
         CALL FORM_READ(REF_FORM,RECN,IERR)			! Get Cover data
         IF (IERR .NE. 0 ) GOTO 99
      END IF

c      IF (FORM_GECHEK(REF_FORM) .LT. 0) THEN
c         QUAL_COVER = .FALSE.
c      ELSE
c         QUAL_COVER = .TRUE.
c      END IF

      STARTED_DISPLAY = .FALSE.
10    CONTINUE
      CALL TARG_VIEW(IERR)
      IF (IERR.NE.0) THEN
         END_MESSAGE = ' TARG REVIEW - Error reading File'
         GOTO 99
      END IF
         WRITE(*,'(A/A)') '  Target List',
     &      ' Rec  Tar No.     Name ( * after Rec indicates Target error)'
         DO L=1,90
            K=TARG_NO(L)
            IF (K.GT.0) THEN
               WRITE(STRING,'(I2,I8,4X,A)') L,K,TARG_NAME(L)
               IF (.NOT. QUAL_TARGET(L)) WRITE(STRING(4:4),'(A)') '*'
               WRITE(*,'(1X,A)') STRING
            END IF
         END DO

20    CONTINUE

         WRITE(*,'(/A)') ' Options - Dn      Delete record n'
         WRITE(*,'(A)') '           Cn,m    Rec n becomes Target m'
         WRITE(*,'(A)') '           R       Returns (or just <ret>)'
         WRITE(*,'(A,$)') 'Enter: '
         READ(*,'(A)') TEXT

      NCHAR = MDH_ENDWORD(TEXT)
      IF (NCHAR.EQ.0 .OR. TEXT(:1) .EQ. 'R' .OR. TEXT(:1).EQ.'r') THEN
         GOTO 90
      ELSE
         IF (TEXT(:1).EQ.'D' .OR. TEXT(:1).EQ.'d') THEN			! Want to delete a record
            READ(TEXT(2:NCHAR),'(I)',IOSTAT=IERR) IREC
            IF (IERR.NE.0) THEN
		WRITE (*,'(A)') ' Invalid Record Number'
		GOTO 20
	    END IF
            IF (NTARGET.EQ.1 .OR. IREC.GT.NTARGET) THEN
               WRITE(MESSAGE(15:16),'(I2)') NTARGET
                  WRITE(*,'(A)') ' '//MESSAGE
               GOTO 20
            ELSE
               CALL TARG_DELETE(IREC)					! Delete rec, update NTARGET
            END IF

         ELSE IF(TEXT(:1) .EQ. 'C' .OR. TEXT(:1) .EQ. 'c') THEN
            NCOM = INDEX(TEXT,',')
            IF (NCOM.GT.0.AND.NCOM.LT.NCHAR) THEN
               READ(TEXT(2:NCOM-1),'(I)',IOSTAT=IERR) IREC
            IF (IERR.NE.0) THEN
		WRITE (*,'(A)') ' Invalid Record Number'
		GOTO 20
	    END IF
               READ(TEXT(NCOM+1:NCHAR),'(I)',IOSTAT=IERR) ITARG
            IF (IERR.NE.0) THEN
		WRITE (*,'(A)') ' Invalid Record Number'
		GOTO 20
	    END IF
               CALL DBS_READ(REF_TARGET,IREC,IERR)
               WRITE(CTARG,'(I3)')ITARG
               CALL DBS_PUTC(REF_TARGET,FLD_TARG_NUMBER,CTARG,IERR)
               CALL FORM_WRITE(REF_TARGET,IREC,IERR)
            ELSE
                  WRITE(*,'(A)') ' '//MESS_CHNGE
               GOTO 20
            END IF								! Option to alter Target List
         END IF

         GOTO 10
      END IF

90    CONTINUE						! Exit point if screen set up
99    CONTINUE

      END
