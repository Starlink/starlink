      SUBROUTINE NDG_ASSO1( PARAM, MODE, INDF, FIELDS, STATUS )
*+
*  Name:
*     NDG_ASSO1

*  Purpose:
*     Obtain an identifier for a single existing NDF using a specified
*     parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_ASSO1( PARAM, MODE, INDF, FIELDS, STATUS )

*  Description:
*     This routine is equivalent to NDF_ASSOC except that it allows the 
*     NDF to be specified using a GRP group expression (for instance, its
*     name may be given within a text file, etc). The first NDF in the 
*     group expression is returned. Any other names in the group
*     expression are ignored. Supplemental information describing the 
*     separate fields in the NDF specification are also returned.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     MODE = CHARACTER * ( * ) (Given)
*        Type of NDF access required: 'READ', 'UPDATE' or 'WRITE'.
*     INDF = INTEGER (Returned)
*        NDF identifier.
*     FIELDS( 6 ) = CHARACTER * ( * ) (Given)
*        Each element contains the following on exit:
*
*        1) NDF slice specifications
*        2) HDS paths
*        3) File types
*        4) Base file names
*        5) Directory paths
*        6) Full NDF specification (this is the returned group - IGRP)
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-AUG-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER MODE*(*)

*  Arguments Returned:
      INTEGER INDF
      CHARACTER FIELDS( 6 ) *(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRP               ! Group holding supplied NDF names
      INTEGER SIZE               ! No. of supplied NDF names
      LOGICAL FLAG               ! User wants to supply more NDFs?
*.

*  Set an initial value for the INDF argument.
      INDF = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a group of NDFs from the environment using the supplied parameter.
*  There is no need to loop if a group expression is given which is 
*  terminated by a flag character since we only want one NDF.
      IGRP = GRP__NOID
      CALL NDG_ASSOC( PARAM, IGRP, SIZE, FLAG, STATUS )

*  Get the supplemental fields for the first NDF in the group.
      CALL NDG_GTSUP( IGRP, 1, FIELDS, STATUS )

*  Get the first NDF from the group.         
      CALL NDG_NDFAS( IGRP, 1, MODE, INDF, STATUS )

*  Delete the group.
      CALL GRP_DELET( IGRP, STATUS )

*  If an error occurred, annul the NDF.
      IF( STATUS .NE. SAI__OK ) CALL NDF_ANNUL( INDF, STATUS )

      END
