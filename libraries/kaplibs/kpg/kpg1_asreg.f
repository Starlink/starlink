      SUBROUTINE KPG1_ASREG( STATUS )
*+
*  Name:
*     KPG1_ASREG

*  Purpose:
*     Register all AST IntraMaps known by KAPPA.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASREG( STATUS )

*  Description:
*     This routine registers all AST IntraMaps known to KAPPA. It should 
*     be called before any AST routine which may use an IntraMap (such
*     as a transformation routine, plotting routine, read/write routine,
*     etc).

*  Arguments:
*     STATUS = INTEGER (Given)
*        Global status value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1998 (DSB):
*        Original version.
*     12-SEP-2005 (TIMJ):
*        CONTACT and AUTHOR now in KPG_PAR
*     {enter_further_changes_here}

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
      EXTERNAL KPG1_ASAGD
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

*  "ASAGD" - Encapsulates a TRANSFORM structure stored with an AGI database
*  picture. The transformation routine uses the TRANSFORM structure
*  associated with the current AGI picture.
      PURPOSE = 'Converts AGI World co-ords to AGI DATA co-ords'
      CALL AST_INTRAREG( 'ASAGD', 2, 2, KPG1_ASAGD, 
     :                   AST__SIMPIF + AST__SIMPFI, PURPOSE( : 46 ), 
     :                   KPG_AUTHOR, KPG_CONTACT, STATUS )

      END
