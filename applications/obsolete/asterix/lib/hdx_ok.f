*+  HDX_OK - Tests whether the named component is OK
      SUBROUTINE HDX_OK(GLOC,NAME,OK,STATUS)
*    Description :
*     Checks if component exists.  If it is primitive it then checks
*     whether it has values set or if a structure has components
*    Method :
*     The subroutine invokes DAT_THERE and DAT_STATE or DAT_NCOMP
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     13 Dec 83: original (BHVAD::JCMP)
*     23 Jan 84: deficiency fix - logical variables initialised (BHVAD::JCMP)
*     24 Jun 86: new tidy up (BHVAD::JCMP)
*      2 Sep 87: Checks to see if locator is to primative object. (pla)
*     24 Mar 88: Does something sensible with structures (BH750::RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
	CHARACTER*(DAT__SZLOC) GLOC	! object locator
	CHARACTER*(*) NAME		! name of component
*    Export :
	LOGICAL OK			! whether component is there and set
*    Status :
	INTEGER STATUS
*    Local variables :
	CHARACTER*(DAT__SZLOC) CLOC	! component locator

	LOGICAL THERE			! component there
	LOGICAL SET			! component has data
        LOGICAL PRIM                    ! locator is to primative object.

        INTEGER NCOMP
*-

*     Status check
	IF(STATUS.NE.SAI__OK) RETURN

*     Check that input locator is not primitive.
        CALL DAT_PRIM( GLOC, PRIM, STATUS )

        IF ( PRIM ) THEN
           OK = .FALSE.

        ELSE
*        Initialise logical variables
           THERE = .FALSE.
           SET   = .FALSE.

*        Check component
           CALL DAT_THERE(GLOC,NAME,THERE,STATUS)
           IF ( STATUS .EQ. SAI__OK ) THEN
              IF ( THERE ) THEN
*	       It's there: see if it's set
                 CALL DAT_FIND(GLOC,NAME,CLOC,STATUS)
                 CALL DAT_PRIM(CLOC,PRIM,STATUS)
                 IF (PRIM) THEN		! for primitive check for values
                   CALL DAT_STATE(CLOC,SET,STATUS)
                 ELSE
                   NCOMP=0		! for structure check for components
                   CALL DAT_NCOMP(CLOC,NCOMP,STATUS)
                   SET=(NCOMP.GT.0)
                 ENDIF
                 CALL DAT_ANNUL(CLOC,STATUS)

              END IF

              IF ( STATUS .NE. SAI__OK) THEN
                 CALL ERR_REP('SUB','from HDX_OK',STATUS)

              END IF
*           Set logical value
              OK = THERE .AND. SET

           END IF
        END IF
	END
