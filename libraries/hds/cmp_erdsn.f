      SUBROUTINE CMP_ERDSN(LOC,CMP,STATUS)
*+
*  Name:
*     CMP_ERDSN

*  Purpose:
*     Report error on structure component.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_ERDSN( [p]... )

*  Description:
*     This routine reports an error of the form:

*        <structure name>.<component name> status

*     where <structure name> is the name of the object located by LOC.
*           <component name> is CMP
*           status is the message text associated with the status value.

*  Arguments:
*     LOC=CHARACTER*(*)
*        Variable containing a locator associated with a structured
*        data object.
*     CMP=CHARACTER*(*)
*        Expression specifying the component name within the
*        structure
*     STATUS=INTEGER
*        Variable holding the status value.

*  Algorithm:
*     Calls the routine DAT_ERDSN which outputs appropriately for
*     the environment being used.

*  Authors:
*     A Chipperfield  (RAL::AJC)
*     {enter_new_authors_here}

*  History:
*     27-Mar-1987:  re-write to call DAT_ERDSN  (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
*    Import
      CHARACTER*(*) LOC			! structure locator
      CHARACTER*(*) CMP			! Component name
      INTEGER STATUS			! Status value

*  External References:

*  Global Variables:

*  Local Constants:

*  Local Variables:

*  Internal References:

*  Local Data:

*.


      CALL DAT_ERDSN( LOC, CMP, STATUS)

      END
