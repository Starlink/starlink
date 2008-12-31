*+COM_FORM_FILES   Include file, holds logical equivalence, DBS units
*	7/30/93	P. Brisco		Added rps_data common
*       18/1/94 MJR			RAL version
 
      INTEGER REF_FORM, REF_TARGET	! Database ref nos.
      INTEGER LEN_FORM_FILE		! Length, FORM_FILE
      LOGICAL LOGICAL_NAME		! True if Logical equivalence set
      CHARACTER*35 FORM			! Logical Name, Cover form
      CHARACTER*35 FORM_TARGET		!   ::		Target form
      CHARACTER*128 FORM_FILE		! Generic File name
 
      COMMON /FORM_FILES/ REF_FORM, REF_TARGET, LEN_FORM_FILE, 
     &          LOGICAL_NAME, FORM, FORM_TARGET, FORM_FILE

      INTEGER len_rpsdata, len_dscfrps
      CHARACTER*80 RPSDATA, RHDATA, DSCFRPS_DATA
      INTEGER RH_LEN
	COMMON /rps_data/rpsdata,len_rpsdata,dscfrps_data,len_dscfrps
      DATA  RHDATA / ' ' / 
      DATA  RH_LEN /1/
