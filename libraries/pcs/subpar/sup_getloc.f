      SUBROUTINE SUBPAR_GETLOC ( NAMECODE, VALID, LOC, STATUS )
*+
*  Name:
*     SUBPAR_GETLOC

*  Purpose:
*     Gets the bottom-level HDS locator of a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_GETLOC ( NAMECODE, VALID, LOC, STATUS )

*  Description:
*     The storage for the locators of the indicated parameter is
*     inspected, and if a valid locator is present it is returned.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to the parameter
*     VALID=LOGICAL (returned)
*        .TRUE. => there is a valid HDS locator associated with the
*        parameter
*     LOC=CHARACTER*(DAT__SZLOC) (returned)
*        the value of the locator associated with the parameter. This
*        is the 'bottom-level' locator as opposed to the one
*        associated with the HDS container file.
*     STATUS=INTEGER

*  Algorithm:
*     NAMECODE indexes into arrays holding a logical flag for the
*     validity of the locator and the locator value. Note the actual
*     locator value is returned rather than a 'clone'.

*  Authors:
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1984 (BDK):
*        Original
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      INTEGER NAMECODE          ! pointer to the parameter


*  Arguments Returned:
      LOGICAL VALID             ! .TRUE. => there is a valid HDS
                                ! locator associated with the parameter

      CHARACTER*(DAT__SZLOC) LOC  ! the value of the locator associated
                                  ! with the parameter. This is the
                                  ! 'bottom-level' locator as opposed to
                                  ! the one associated with the HDS
                                  ! container file.


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Check that NAMECODE is in range, and return the values.
*
      IF ( ( NAMECODE .LE. PARPTR ) .AND. ( NAMECODE .GT. 0 ) ) THEN

         VALID = PARVALID(NAMECODE)
         LOC = PARLOC(2,NAMECODE)

      ELSE

         STATUS = SUBPAR__NOPAR
         VALID = .FALSE.

      ENDIF

      END
