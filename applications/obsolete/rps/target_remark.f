*+TARGET_REMARK        Produses a summary of the remarks for lineprinter o/p
*  Dec 1992 	M. Duesterhaus	create original
*************************************************************************
      SUBROUTINE TARGET_REMARK(LUN_OUT,BSLASH,START_TARG)
      IMPLICIT NONE

*   Input :
      INTEGER LUN_OUT,START_TARG
      CHARACTER*1 BSLASH

*  Global Variables
      INCLUDE 'com_form_qual.inc'
      INCLUDE 'com_form_points.inc'		! Gives Constraints field
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_target.inc'
      INCLUDE 'zpidata.inc'
      DOUBLE PRECISION DECLONG, DECLAT
      COMMON / ECL_COORD/ DECLONG, DECLAT	! Available from ROS_VIEW
      LOGICAL SMG
      COMMON / SMG_KEEP / SMG

*   Functions :
      INTEGER DBS_FIELDNO					!Gets field number from the database.
      INTEGER DBS_GETI						!Gets integer value from the database.
      CHARACTER*60 DBS_GETC					!Gets character value from the database.
      INTEGER FIND_FIRST				!Used to check for blank character variables.
*-
*   Local :
      INTEGER FIELD_NO, I, IERR
      INTEGER ITARGET, PTARGET, NTARGS, JTARGET, TARG_NUM, COUNT_REM
      INTEGER LENS,START,ITEST, SUBINDEX,LOC
      CHARACTER*20 YTEXT*3, YTEXT2*3
      CHARACTER*20 NUM*2

      CHARACTER*1 INCR,QUOTE
      CHARACTER*50 REQ(4)

*  __________________________ Executable Code __________________________________
      QUOTE = CHAR(39)
      PTARGET = 1
      COUNT_REM = 0

      IF (REF_FORM .LE.0 ) THEN							! Open files if necessary
         CALL FORM_OPEN( 'R', IERR)
         IF (IERR .NE. 0) GOTO 90
         CALL FORM_READ(REF_FORM,1,IERR)
         IF (IERR .NE. 0) GOTO 90
      END IF

      NTARGS = DBS_GETI(REF_FORM,FLD_NTARGETS)
C      IF ((NTARGS-START_TARG).GE.20) THEN
C        NTARGS = START_TARG+19
C      END IF
      DO ITARGET = START_TARG,NTARGS

         DO JTARGET = 1,NTARGS
           CALL TARG_READ(JTARGET,IERR)
           IF (IERR.NE.0) GOTO 90
           TARG_NUM = DBS_GETI(REF_TARGET,1)
           IF (TARG_NUM.EQ.ITARGET) THEN
             GOTO 60
           END IF
         END DO
         GOTO 80

 60      FIELD_NO=DBS_FIELDNO(REF_TARGET,'REMARKS(1)')
         REQ(1) = DBS_GETC(REF_TARGET, FIELD_NO)
         IF (REQ(1)(1:5) .NE. '     ') THEN
C            IF (ITARGET.EQ.1) THEN
C
C               FOOTER = .TRUE.
C               WRITE(LUN,'(A)' ) '1'
CC               DO I=1,14
C                 WRITE(LUN,'(A)') LINE(I)
C               END DO
C            END IF

            IF (COUNT_REM.EQ.20) THEN
		START_TARG = ITARGET
		GOTO 90
            ELSE
                COUNT_REM = COUNT_REM +1
            END IF
            DO I=2,4
              WRITE(INCR,'(I1)')I
              FIELD_NO=DBS_FIELDNO(REF_TARGET,'REMARKS('//INCR//')')
              REQ(I) = DBS_GETC(REF_TARGET, FIELD_NO)
            END DO

            DO I=1,4
	       LENS = 50
               START = 1
               ITEST = 1
               DO WHILE (ITEST .NE. 0)
                  ITEST = FIND_FIRST(REQ(I)(START:LENS),
     &               '$&%#_{}',SUBINDEX  )
                  IF (ITEST .NE. 0) THEN
                     LOC = START + ITEST - 1
                     REQ(I) = REQ(I)(:LOC-1) // BSLASH // REQ(I)(LOC:LENS)
                     LENS = LENS + 1
                     START = LOC + 2
                  END IF
                  ITEST = FIND_FIRST(REQ(I)(START:LENS),
     &               QUOTE,SUBINDEX  )
                  IF (ITEST .NE. 0) THEN
                     LOC = START + ITEST - 1
                     REQ(I) = REQ(I)(:LOC-1) // QUOTE // REQ(I)(LOC:LENS)
                     LENS = LENS + 1
                     START = LOC + 2
                  END IF
               END DO
            END DO

            WRITE(NUM ,'(I2)') TARG_NO(JTARGET)

* write to latex file

c      WRITE(LUN_OUT,'(A)') BSLASH//'put(20,195){'//BSLASH//'line(1,0){170}}'
      If (ptarget .GT. 20) ptarget = 1
      WRITE(YTEXT,'(I3)' ) 200 - PTARGET*8
      WRITE(YTEXT2,'(I3)' ) 196 - PTARGET*8
      WRITE(LUN_OUT,'(A)') BSLASH//'put(25,'// YTEXT //'){'//BSLASH//
     &    'makebox(0,0)[tr]{'//BSLASH//'tt '//bslash//'small '//
     &     NUM //'}}'
      WRITE(LUN_OUT,'(A)') BSLASH//'put(35,'// YTEXT //'){'//BSLASH//
     &    'makebox(0,0)[tl]{'//BSLASH//'tt '//bslash//'small '//
     &     REQ(1) //'}}'
      WRITE(LUN_OUT,'(A)') BSLASH//'put(110,'// YTEXT //'){'//BSLASH//
     &    'makebox(0,0)[tl]{'//BSLASH//'tt '//bslash//'small '//
     &     REQ(2) //'}}'
      WRITE(LUN_OUT,'(A)') BSLASH//'put(35,'// YTEXT2//'){'//BSLASH//
     &    'makebox(0,0)[tl]{'//BSLASH//'tt '//bslash//'small '//
     &     REQ(3) //'}}'
      WRITE(LUN_OUT,'(A)') BSLASH//'put(110,'// YTEXT2 //'){'//BSLASH//
     &    'makebox(0,0)[tl]{'//BSLASH//'tt '//bslash//'small '//
     &     REQ(4) //'}}'

       PTARGET = PTARGET + 1

         END IF

 80   END DO							! End loop for each target
      START_TARG = ITARGET
90    CONTINUE
      END
