*+REMARKS_SUMMARY        Produces a print of any remarks
      SUBROUTINE REMARKS_SUMMARY(LUN_OUT, BSLASH, MORE_SUMMARY)
      IMPLICIT NONE

*   Input :
      INTEGER LUN_OUT
      CHARACTER*1 BSLASH
*   Out:
      LOGICAL MORE_SUMMARY

* History
*     Feb 94 M Ricketts	  Modified from vax vsn

*  Global Variables
      INCLUDE 'com_form_qual.inc'
      INCLUDE 'com_form_points.inc'		! Gives Constraints field
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_target.inc'
      INCLUDE 'zpidata.inc'
      REAL*8 DECLONG, DECLAT
      COMMON / ECL_COORD/ DECLONG, DECLAT	! Available from ROS_VIEW

*   Functions :
      INTEGER DBS_FIELDNO					!Gets field number from the database.
      INTEGER DBS_GETI						!Gets integer value from the database.
      CHARACTER*60 DBS_GETC					!Gets character value from the database.
      INTEGER MDH_ENDWORD					!Used to check for blank character variables.
*-
*   Local :
      INTEGER FIELD_NO, IERR, LINELOC, NREM, IREM, LEN, LENR(4)
      INTEGER ITARGET, NTARGS, NS, FIRST_TARGET, NEXT_TARGET
      SAVE NEXT_TARGET
      LOGICAL REMARKS
      CHARACTER*50 REMSTRING(4), FLD*20, NUM*2, YTEXT*3

*  __________________________ Executable Code __________________________________


      IF (REF_FORM .LE.0 ) THEN							! Open files if necessary
         CALL FORM_OPEN( 'R', IERR)
         IF (IERR .NE. 0) GOTO 90
         CALL FORM_READ(REF_FORM,1,IERR)
         IF (IERR .NE. 0) GOTO 90
      END IF

      FIELD_NO = DBS_FIELDNO(REF_FORM,'PROPOSAL.TITLE(1)' )

      NTARGS = DBS_GETI(REF_FORM,FLD_NTARGETS)
      LINELOC = 198
      IF (MORE_SUMMARY) THEN								! on subsequent page
         FIRST_TARGET = NEXT_TARGET
      ELSE
         FIRST_TARGET = 1
      END IF

      DO ITARGET = FIRST_TARGET, NTARGS

         CALL TARG_READ(ITARGET,IERR)
         IF (IERR.NE.0) GOTO 90
         REMARKS = .TRUE.
         NREM = 0
         NS = 1
         DO WHILE ((NS.LE.4) .AND. REMARKS)
            WRITE(FLD,'(A,I1,A)') 'REMARKS(', NS, ')'
            FIELD_NO=DBS_FIELDNO(REF_TARGET,FLD)
            REMSTRING(NS) = DBS_GETC(REF_TARGET, FIELD_NO)

            LEN = MDH_ENDWORD(REMSTRING(NS))
            IF (LEN .EQ.0) THEN
               REMARKS = .FALSE.
            ELSE
               NREM = NS
               LENR(NREM) = LEN
            END IF
            NS = NS + 1
         END DO

         IF (LINELOC - NREM*6 .LT. 10) THEN						!end this page if not enough space
            NEXT_TARGET = ITARGET
            MORE_SUMMARY = .TRUE.
            GOTO 99
         END IF

         IF (NREM .GT. 0) THEN
            WRITE(NUM ,'(I2)') TARG_NO(ITARGET)

            LINELOC = LINELOC - 6
            WRITE(YTEXT,'(I3)' ) LINELOC

            WRITE(LUN_OUT,'(A)') BSLASH//'put(25,'// YTEXT //'){'//BSLASH//'makebox(0,0)[tl]{'//BSLASH//'tt '// NUM //'}}'
            WRITE(LUN_OUT,'(A)') BSLASH//'put(35,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// REMSTRING(1)(:LENR(1)) //'}}'

            DO IREM = 2,NREM
               LINELOC = LINELOC - 6
               WRITE(YTEXT,'(I3)' ) LINELOC
               WRITE(LUN_OUT,'(A)') BSLASH//'put(35,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// REMSTRING(IREM)(:LENR(IREM)) //'}}'
            END DO
         END IF

      END DO							! End loop for each target
90    CONTINUE

      MORE_SUMMARY = .FALSE.
99    CONTINUE
      END
