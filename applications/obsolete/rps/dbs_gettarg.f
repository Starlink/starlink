*+  DBS_GETTARG
*	12 Nov 1992	M. Duesterhaus (GSFC)	ORIGINAL
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
**********************************************************************
      SUBROUTINE DBS_GETTARG ( NTARG, TOTAL_TIME )
*
      INTEGER	   NTARG
      CHARACTER*7  TOTAL_TIME

*  Global Variables
      INCLUDE 'com_form_files.inc'

*  functions

      INTEGER DBS_FIELDNO
      REAL DBS_GETR
      CHARACTER*7 MDH_RTOC

*  local
      INTEGER TARGET_NO, IERR, FIELDNO
      REAL TOT_TIME

      TOT_TIME = 0.0
      DO TARGET_NO=1,NTARG
        CALL FORM_READ(REF_TARGET,TARGET_NO, IERR)
        FIELDNO = DBS_FIELDNO(REF_TARGET,'TOTAL.OBS.TIME')
        TOT_TIME = TOT_TIME + DBS_GETR(REF_TARGET,FIELDNO)
      END DO

      TOTAL_TIME = MDH_RTOC(TOT_TIME)

      END

