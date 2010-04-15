*+CON_CHECK_GEN    Consistency check for Rosat Proposal Form  General Page
*	DATE		AUTHOR			DESCRIPTION
*	???		RAL			ORIGINAL
*	10 APR 1992	M. DUESTERHAUS		PORT TO UNIX
*     1994 Jan		M Ricketts		RAL again, remove distrib. medium
*     1996 Mar          MR                      furthe coi checks
***********************************************************************
      SUBROUTINE CON_CHECK_GEN(REF_NO,MESID,SCREEN,XTYPE,FIELD_NO)
      IMPLICIT NONE

*   Input :
      INTEGER REF_NO		! Database reference number.
      INTEGER MESID		! Screen Number
      LOGICAL SCREEN		! Set to TRUE if in screen mode.

*   Output :
      CHARACTER*1 XTYPE
      INTEGER FIELD_NO		! Field number Where error detected, else 1

*  Global Variables
      INCLUDE 'com_form_qual.inc'

*-
*   Functions
      INTEGER DBS_FIELDNO					!Gets field numberfrom the database.
      CHARACTER*20 DBS_GETC					!Gets character value from the database.
      INTEGER MDH_ENDWORD					!Finds length of word.
      CHARACTER*1 CON_CHECK_ERR

* Local Variables
      CHARACTER*7 CVAL
      INTEGER I							!Array counter.

      INTEGER COI_NFIELD, COI_IFIELD, COI_CFIELD
      INTEGER NAME_CHECK, INST_CHECK, CNTRY_CHECK
      CHARACTER*20 MESSAGE*40, FLD_NAME, FLD_INST, FLD_CNTRY
      CHARACTER*32 COI_NAME, COI_INST, COI_CNTRY

* _____________________________________Executable Code _________________________________

      DO I=1,6
         WRITE(FLD_NAME,'(A,I1,A)') 'COI.NAME(', I, ')'
         COI_NFIELD = DBS_FIELDNO(REF_NO,FLD_NAME)
         COI_NAME  = DBS_GETC(REF_NO, COI_NFIELD)

         WRITE(FLD_INST,'(A,I1,A)') 'COI.INSTITUTE(', I, ')'
         COI_IFIELD = DBS_FIELDNO(REF_NO,FLD_INST)
         COI_INST  = DBS_GETC(REF_NO, COI_IFIELD)

         NAME_CHECK = MDH_ENDWORD(COI_NAME)
         INST_CHECK = MDH_ENDWORD(COI_INST)
         IF (NAME_CHECK.GT.0 .AND. INST_CHECK.EQ.0) THEN
            MESSAGE = 'COI Institute needed / remove name'
            FIELD_NO = COI_IFIELD
            GOTO 10
         ELSE IF (NAME_CHECK.EQ.0 .AND. INST_CHECK.GT.0) THEN
            MESSAGE = 'COI Name needed / remove Inst.'
            FIELD_NO = COI_NFIELD
            GOTO 10
         END IF

* name / inst. compatible, now check country

         WRITE(FLD_CNTRY,'(A,I1,A)') 'COI.CNTRY(', I, ')'
         COI_CFIELD = DBS_FIELDNO(REF_NO,FLD_CNTRY)
         COI_CNTRY  = DBS_GETC(REF_NO, COI_CFIELD)
         CNTRY_CHECK = MDH_ENDWORD(COI_CNTRY)

         IF (NAME_CHECK.GT.0 .AND. CNTRY_CHECK.EQ.0) THEN
            MESSAGE = 'COI Country needed or remove name'
            FIELD_NO = COI_CFIELD
            GOTO 10
         ELSE IF (NAME_CHECK.EQ.0 .AND. CNTRY_CHECK.GT.0) THEN
            MESSAGE = 'COI Name needed or remove Country'
            FIELD_NO = COI_CFIELD
            GOTO 10
         END IF

      END DO

      FIELD_NO=DBS_FIELDNO(REF_NO,'AGENCY')
      CVAL=DBS_GETC(REF_NO,FIELD_NO)
      IF (CVAL .NE. 'PPARC' .AND. CVAL.NE.'NASA' .AND. CVAL .NE.'BMFT') THEN
         MESSAGE = 'Agency'
         GOTO10
      END IF


*  No errors found
      FIELD_NO = 1
      QUAL_GEN = .TRUE.
      GOTO 20

10    CONTINUE
      XTYPE = CON_CHECK_ERR(MESID,MESSAGE)
      QUAL_GEN = .FALSE.
20    CONTINUE

      END
