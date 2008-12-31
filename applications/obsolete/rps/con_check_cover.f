*+CON_CHECK_COVER  Consistency check for Rosat Proposal Form  Cover Page
*	DATE		AUTHOR			DESCRIPTION
*	????		RAL			ORIGINAL
*	10 APR 1992	M. DUESTERHAUS (GSFC)	PORT TO UNIX
************************************************************************
      SUBROUTINE CON_CHECK_COVER(REF_NO,MESID,SCREEN,XTYPE,FIELD_NO)
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
      INTEGER DBS_GETI						!Gets integer value from the database.
      CHARACTER*20 DBS_GETC					!Gets character value from the database.
      INTEGER MDH_ENDWORD					!Finds length of word.
      CHARACTER*1 CON_CHECK_ERR
 
* Local Variables
      INTEGER IVAL
      CHARACTER*20 CVAL

      CHARACTER*20 MESSAGE

* _____________________________________Executable Code _________________________________

      FIELD_NO=DBS_FIELDNO(REF_NO,'LAST.NAME')
      CVAL=DBS_GETC(REF_NO,FIELD_NO)
      IF(MDH_ENDWORD(CVAL).EQ.0) THEN
         MESSAGE = 'Last Name'
         GOTO10
      END IF

      FIELD_NO=DBS_FIELDNO(REF_NO,'FIRST.NAME')
      CVAL=DBS_GETC(REF_NO,FIELD_NO)
      IF(MDH_ENDWORD(CVAL).EQ.0) THEN
         MESSAGE = 'First Name'
         GOTO10
      END IF

      FIELD_NO=DBS_FIELDNO(REF_NO,'INSTITUTE')
      CVAL=DBS_GETC(REF_NO,FIELD_NO)
      IF(MDH_ENDWORD(CVAL).EQ.0) THEN
         MESSAGE = 'Institute'
         GOTO10
      END IF

      FIELD_NO=DBS_FIELDNO(REF_NO,'POSTAL.CODE')
      CVAL=DBS_GETC(REF_NO,FIELD_NO)
      IF(MDH_ENDWORD(CVAL).EQ.0) THEN
         MESSAGE = 'Post Code'
         GOTO10
      END IF

      FIELD_NO=DBS_FIELDNO(REF_NO,'TELEPHONE.NUMBER')
      CVAL=DBS_GETC(REF_NO,FIELD_NO)
      IF(MDH_ENDWORD(CVAL).EQ.0) THEN
         MESSAGE = 'Telephone No.'
         GOTO10
      END IF

      FIELD_NO=DBS_FIELDNO(REF_NO,'PROPOSAL.TITLE(1)')
      CVAL=DBS_GETC(REF_NO,FIELD_NO)
      IF(MDH_ENDWORD(CVAL).EQ.0) THEN
         MESSAGE = 'Proposal Title'
         GOTO10
      END IF

      FIELD_NO=DBS_FIELDNO(REF_NO,'SUBJECT.CATEGORY')
      IVAL=DBS_GETI(REF_NO,FIELD_NO)
      IF(IVAL.LT.1.OR.IVAL.GT.10) THEN
         MESSAGE = 'Subject category'
         GOTO10
      END IF


*  No errors found
      FIELD_NO = 1
      QUAL_COVER = .TRUE.
      GOTO 20
 
10    CONTINUE
      XTYPE = CON_CHECK_ERR(MESID,MESSAGE)
      QUAL_COVER = .FALSE.
20    CONTINUE

      END
