      SUBROUTINE SUBPAR_PUTFLOC ( NAMECODE, LOC, STATUS )
*+
*  Name:
*     SUBPAR_PUTFLOC

*  Purpose:
*     Stores the top-level HDS locator of a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PUTFLOC ( NAMECODE, LOC, STATUS )

*  Description:
*     The given locator is stored in the internal storage for the
*     top-level locator associated with the indicated parameter.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to the parameter
*     LOC=CHARACTER*(DAT__SZLOC) (given)
*        the value of the locator to be associated with the parameter.
*        This is the 'top-level' locator associated with the HDS
*        container file.
*     STATUS=INTEGER

*  Algorithm:
*     NAMECODE indexes into the array for holding the locator value.

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

      CHARACTER*(DAT__SZLOC) LOC  ! the value of the locator to be
                                  ! associated with the parameter.
                                  ! This is the 'top-level' locator
                                  ! associated with the HDS container file.


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Check that NAMECODE is in range, and insert the value.
*
      IF ( ( NAMECODE .LE. PARPTR ) .AND. ( NAMECODE .GT. 0 ) ) THEN

         PARLOC(1,NAMECODE) = LOC

      ELSE

         STATUS = SUBPAR__NOPAR

      ENDIF

      END
