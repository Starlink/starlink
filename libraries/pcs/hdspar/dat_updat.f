      SUBROUTINE DAT_UPDAT ( PARAM, STATUS )
*+
*  Name:
*     DAT_UPDAT

*  Purpose:
*     Force HDS update.

*  Language:
*     Fortran 77

*  Invocation:
*     CALL DAT_UPDAT ( PARAM, STATUS )

*  Description:
*     If there is an HDS object associated with the parameter, force
*     its container file to be freed so that its memory cache coincides 
*     with the data on disk and the file is available for other programs
*     to use.

*  Arguments:
*     PARAM=CHARACTER*(*) (given)
*        Name of program parameter.
*     STATUS=INTEGER (given and returned)
*        Global status

*  Algorithm:
*     Use the SUBPAR routines to look-up the parameter and update the
*     HDS file on disk (SUBPAR_UPDAT calls HDS_FREE).

*  Authors:
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     18-APR-1985 (BDK)
*        Original
*     16-JUN-1998 (AJC)
*        Re-format prologue
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) PARAM      ! name of parameter associated with and
                               ! HDS structure

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NAMECODE         ! pointer to the parameter

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL SUBPAR_FINDPAR ( PARAM, NAMECODE, STATUS )
      CALL SUBPAR_UPDAT ( NAMECODE, STATUS )

      END
