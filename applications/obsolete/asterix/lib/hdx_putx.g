*+  HDX_PUT<T> - Puts <TYPE> variable/array on HDS file.
      SUBROUTINE HDX_PUT<T>( LOC, NAME, NELS, ARRAY, STATUS )
* Description :
*     <description of what the subroutine does - for user info>
* Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
* Method :
*     <description of how the subroutine works - for programmer info>
* Deficiencies :
*     <description of any deficiencies>
* Bugs :
*     <description of any "bugs" which have not been fixed>
* Authors :
*	Clive Page	1986 July 9
* History :
*      MAY 10 1988   ASTERIX88    (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
* Import :
	CHARACTER*(DAT__SZLOC) LOC !input: locator to structure.
	CHARACTER*(*) NAME	   !input: name of variable or array.
	INTEGER NELS		   !input: no of elements, =1 for scalar.
	<TYPE> ARRAY(NELS)	   !input: values to be written to HDS file.
* Import-Export :
*     <declarations and descriptions for imported/exported arguments>
* Export :
*     <declarations and descriptions for exported arguments>
* Status :
      INTEGER STATUS
*-

*    Check status
      IF (STATUS .NE. SAI__OK) RETURN

*    Write object
      CALL HDX_PUTN( LOC, NAME, '<HTYPE>', NELS, ARRAY, STATUS )

*    Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', ' from HDX_PUT<T>', STATUS )
      END IF

      END
