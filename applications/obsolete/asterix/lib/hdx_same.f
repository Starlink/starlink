*+  HDX_SAME - Do two locators point to the same object?
      SUBROUTINE HDX_SAME(LOC1,LOC2,SAME,STATUS)
*    Description :
*     If the locators LOC1 and LOC2 point to the same data object, then
*     the LOGICAL variable SAME is returned .TRUE., otherwise SAME is
*     returned .FALSE.
*    Invocation :
*     CALL HDX_SAME(LOC1,LOC2,SAME,STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*     LOC1=CHAR(READ)
*           locator to first object
*     LOC2=CHAR(READ)
*           locator to 2nd object
*     SAME=LOGICAL(WRITE)
*           objects are one and the same
*     STATUS=INTEGER(UPDATE)
*    Method :
*     Uses HDS_TRACE and compares the paths.
*    Deficiencies :
*     Search path and filenames limited to 100 characters
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     19 Nov 85 :  original (BHVAD::JCMP)
*     08 Dec 88 :  Renamed to HDX_SAME ( dja @ uk.ac.bham.sr.star )
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
	CHARACTER*(DAT__SZLOC) LOC1	! locator to first object
	CHARACTER*(DAT__SZLOC) LOC2	! locator to 2nd object
*    Import-Export :
*    Export :
	LOGICAL SAME			! objects are one and the same
*    Status :
	INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
	INTEGER NLEV			! # levels in path
	CHARACTER*(100) PATH1		! path of 1st object
	CHARACTER*(100) PATH2		! path of 2nd object
	CHARACTER*(100) FILE1		! container file of 1st object
	CHARACTER*(100) FILE2		! container file of 2nd object
*    Internal References :
*    Local data :
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Trace objects
	CALL HDS_TRACE(LOC1,NLEV,PATH1,FILE1,STATUS)
	CALL HDS_TRACE(LOC2,NLEV,PATH2,FILE2,STATUS)
	IF(STATUS.EQ.SAI__OK) THEN
	   SAME=PATH1.EQ.PATH2.AND.FILE1.EQ.FILE2
	ELSE
	   SAME=.FALSE.
	ENDIF

	END
