      SUBROUTINE TRN1_ERRL( ROUTIN, LOC, STATUS )
*+
*  Name:
*     TRN1_ERRL

*  Purpose:
*     report error about an object locator.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_ERRL( ROUTIN, LOC, STATUS )

*  Description:
*     The routine reports an error about an object.  It does this by
*     calling TRN1_ERROR with error text of the form:

*        <object>

*  Authors:
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
*     <any INCLUDE files containing global constant definitions>


*  Arguments Given:
      CHARACTER * ( * ) ROUTIN  ! Name of routine reporting the error
      CHARACTER * ( * ) LOC     ! Locator to object causing error


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
*     <declarations for local variables>


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Set a message token for the error report.
      CALL DAT_MSG( 'OBJECT', LOC )


*   Report the error message.
      CALL TRN1_ERROR( ROUTIN, '^OBJECT', STATUS )


*   Exit routine.
      END
