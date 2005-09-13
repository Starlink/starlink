      SUBROUTINE KPG1_ASRGN( STATUS )
*+
*  Name:
*     KPG1_ASRGN

*  Purpose:
*     Register all non-graphical AST IntraMaps known by KAPPA.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASRGN( STATUS )

*  Description:

*     This routine registers all non-graphical AST IntraMaps known to
*     KAPPA. It should be called before any AST routine which may use an
*     IntraMap (such as a transformation routine, read/write routine, etc).

*  Arguments:
*     STATUS = INTEGER (Given)
*        Global status value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1998 (DSB):
*        Original version.
*     12-SEP-2005 (TIMJ):
*        Factor out from KPG1_ASREG
*     {enter_further_changes_here}

*  Notes:
*     Use KPG1_ASREG to register all IntraMaps (graphical and non-graphical).
*     Use KPG1_ASRGG to register just graphical IntraMaps.

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants 
      INCLUDE 'KPG_PAR'          ! KPG constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL KPG1_ASLOG

*  Local Variables:
      CHARACTER PURPOSE*80
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  "Log10" - Log base 10
      PURPOSE = 'Take log. (base 10) of each coordinate value'
      CALL AST_INTRAREG( 'Log10', AST__ANY, AST__ANY, KPG1_ASLOG, 
     :                   AST__SIMPIF + AST__SIMPFI, PURPOSE( : 44 ), 
     :                   KPG_AUTHOR, KPG_CONTACT, STATUS )

      END
