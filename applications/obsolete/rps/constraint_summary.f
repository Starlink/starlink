*+CONSTRAINT_SUMMARY        Produces a summary of any constraints
      SUBROUTINE CONSTRAINT_SUMMARY(LUN_OUT, BSLASH, MORE_SUMMARY)
      IMPLICIT NONE

*   Input :
      INTEGER LUN_OUT
      CHARACTER*1 BSLASH
*   Out:
      LOGICAL MORE_SUMMARY

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
      LOGICAL DBS_GETL
*-
*   Local :
      INTEGER FIELD_NO, IERR, LINELOC, ITARGET, NTARGS, FIRST_TARGET, NEXT_TARGET
      SAVE NEXT_TARGET
      LOGICAL LCOORD,LMON,LCONTG,LPHASE				!Set to TRUE on selection of a given constraint.
      CHARACTER*10 YR1, MO1, DA1, HR1, MN1, YR2, MO2, DA2, HR2, MN2
      CHARACTER*12 CON_INT, MON_INT, PHAS_EPOCH, PHAS_PERIOD, YTEXT*3, NUM*2

*  __________________________ Executable Code __________________________________

      IF (REF_FORM .LE.0 ) THEN							! Open files if necessary
         CALL FORM_OPEN( 'R', IERR)
         IF (IERR .NE. 0) GOTO 90
         CALL FORM_READ(REF_FORM,1,IERR)
         IF (IERR .NE. 0) GOTO 90
      END IF

      FIELD_NO = DBS_FIELDNO(REF_FORM,'PROPOSAL.TITLE(1)' )

      NTARGS = DBS_GETI(REF_FORM,FLD_NTARGETS)
      LINELOC = 197
      IF (MORE_SUMMARY) THEN								! on subsequent page
         FIRST_TARGET = NEXT_TARGET
      ELSE
         FIRST_TARGET = 1
      END IF

      DO ITARGET = FIRST_TARGET, NTARGS

         CALL TARG_READ(ITARGET,IERR)
         IF (IERR.NE.0) GOTO 90


         IF (DBS_GETL(REF_TARGET, FLD_CONSTRAINTS) ) THEN			! Time constraint set

            FIELD_NO=DBS_FIELDNO(REF_TARGET,'COORD.OBSERVATION')
            LCOORD=DBS_GETL(REF_TARGET,FIELD_NO)

            FIELD_NO=DBS_FIELDNO(REF_TARGET,'MONITOR')
            LMON=DBS_GETL(REF_TARGET,FIELD_NO)

            FIELD_NO=DBS_FIELDNO(REF_TARGET,'CONTIGUOUS.OBS')
            LCONTG=DBS_GETL(REF_TARGET,FIELD_NO)

            FIELD_NO=DBS_FIELDNO(REF_TARGET,'PHASE.DEPENDENT')
            LPHASE=DBS_GETL(REF_TARGET,FIELD_NO)

            IF (LCOORD) THEN

               FIELD_NO=DBS_FIELDNO(REF_TARGET,'START.YEAR')
               YR1=DBS_GETC(REF_TARGET,FIELD_NO)
               FIELD_NO=DBS_FIELDNO(REF_TARGET,'START.MONTH')
               MO1=DBS_GETC(REF_TARGET,FIELD_NO)
               FIELD_NO=DBS_FIELDNO(REF_TARGET,'START.DAY')
               DA1=DBS_GETC(REF_TARGET,FIELD_NO)
               FIELD_NO=DBS_FIELDNO(REF_TARGET,'START.HOUR')
               HR1=DBS_GETC(REF_TARGET,FIELD_NO)
              FIELD_NO=DBS_FIELDNO(REF_TARGET,'START.MINUTE')
               MN1=DBS_GETC(REF_TARGET,FIELD_NO)

               FIELD_NO=DBS_FIELDNO(REF_TARGET,'END.YEAR')
               YR2=DBS_GETC(REF_TARGET,FIELD_NO)
               FIELD_NO=DBS_FIELDNO(REF_TARGET,'END.MONTH')
               MO2=DBS_GETC(REF_TARGET,FIELD_NO)
               FIELD_NO=DBS_FIELDNO(REF_TARGET,'END.DAY')
               DA2=DBS_GETC(REF_TARGET,FIELD_NO)
               FIELD_NO=DBS_FIELDNO(REF_TARGET,'END.HOUR')
               HR2=DBS_GETC(REF_TARGET,FIELD_NO)
               FIELD_NO=DBS_FIELDNO(REF_TARGET,'END.MINUTE')
               MN2=DBS_GETC(REF_TARGET,FIELD_NO)

            ELSE IF (LMON) THEN
               FIELD_NO=DBS_FIELDNO(REF_TARGET,'TIME.INTERVAL')
               MON_INT=DBS_GETC(REF_TARGET,FIELD_NO)

            ELSE IF (LCONTG) THEN
               FIELD_NO=DBS_FIELDNO(REF_TARGET,'NUMBER.INTERVALS')
               CON_INT=DBS_GETC(REF_TARGET,FIELD_NO)

            ELSE IF (LPHASE) THEN
               FIELD_NO=DBS_FIELDNO(REF_TARGET,'EPOCH')
               PHAS_EPOCH=DBS_GETC(REF_TARGET,FIELD_NO)
               FIELD_NO=DBS_FIELDNO(REF_TARGET,'PERIOD')
               PHAS_PERIOD=DBS_GETC(REF_TARGET,FIELD_NO)
            END IF

            IF (LINELOC - 6 .LT. 10) THEN						!end this page if not enough space
               NEXT_TARGET = ITARGET
               MORE_SUMMARY = .TRUE.
               GOTO 99
            END IF

            WRITE(NUM ,'(I2)') TARG_NO(ITARGET)

* write to latex file

            LINELOC = LINELOC - 6
            WRITE(YTEXT,'(I3)' ) LINELOC
            WRITE(LUN_OUT,'(A)') BSLASH//'put(21,'// YTEXT //'){'//BSLASH//'makebox(0,0)[tl]{'//BSLASH//'tt '// NUM //'}}'
            IF (LCOORD) THEN
               WRITE(LUN_OUT,'(A)') BSLASH//'put(27,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt Y}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(32.5,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// YR1(:MDH_ENDWORD(YR1)) //'}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(42,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// MO1(:MDH_ENDWORD(MO1)) //'}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(48,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// DA1(:MDH_ENDWORD(DA1)) //'}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(55,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// HR1(:MDH_ENDWORD(HR1)) //'}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(61,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// MN1(:2) //'}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(71.5,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// YR2(:MDH_ENDWORD(YR2)) //'}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(81,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// MO2(:MDH_ENDWORD(MO2)) //'}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(87,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// DA2(:MDH_ENDWORD(DA2)) //'}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(94,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// HR2(:MDH_ENDWORD(HR2)) //'}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(100,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '//MN2(:2) //'}}'
            ELSE
               WRITE(LUN_OUT,'(A)') BSLASH//'put(27,'// YTEXT //'){'//BSLASH//'makebox(0,0)[tl]{'//BSLASH//'tt N}}'
            END IF

            IF (LMON) THEN
               WRITE(LUN_OUT,'(A)') BSLASH//'put(112,'// YTEXT //'){'//BSLASH//'makebox(0,0)[tl]{'//BSLASH//'tt Y}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(117,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// MON_INT(:5) //'}}'
            ELSE
               WRITE(LUN_OUT,'(A)') BSLASH//'put(112,'// YTEXT //'){'//BSLASH//'makebox(0,0)[tl]{'//BSLASH//'tt N}}'
            END IF

            IF (LCONTG) THEN
               WRITE(LUN_OUT,'(A)') BSLASH//'put(172,'// YTEXT //'){'//BSLASH//'makebox(0,0)[tl]{'//BSLASH//'tt Y}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(180,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// CON_INT(:3) //'}}'
            ELSE
               WRITE(LUN_OUT,'(A)') BSLASH//'put(172,'// YTEXT //'){'//BSLASH//'makebox(0,0)[tl]{'//BSLASH//'tt N}}'
            END IF

            IF (LPHASE) THEN
               WRITE(LUN_OUT,'(A)') BSLASH//'put(130,'// YTEXT //'){'//BSLASH//'makebox(0,0)[tl]{'//BSLASH//'tt Y}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(136,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// PHAS_EPOCH(:8) //'}}'
               WRITE(LUN_OUT,'(A)') BSLASH//'put(155,'// YTEXT //'){'//BSLASH//
     &		'makebox(0,0)[tl]{'//BSLASH//'tt '// PHAS_PERIOD(:7) //'}}'
            ELSE
               WRITE(LUN_OUT,'(A)') BSLASH//'put(130,'// YTEXT //'){'//BSLASH//'makebox(0,0)[tl]{'//BSLASH//'tt N}}'
            END IF

         END IF								! constraaint set
      END DO							! End loop for each target
90    CONTINUE

      MORE_SUMMARY = .FALSE.
99    CONTINUE
      END
