      SUBROUTINE CTG_ASSO1( PARAM, MODE, CI, FIELDS, STATUS )
*+
*  Name:
*     CTG_ASSO1

*  Purpose:
*     Obtain an identifier for a single existing catalogue using a 
*     specified parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG_ASSO1( PARAM, MODE, CI, FIELDS, STATUS )

*  Description:
*     This routine is equivalent to CAT_ASSOC except that it allows the 
*     catalogue to be specified using a GRP group expression (for instance,
*     its name may be given within a text file, etc). The first catalogue in 
*     the group expression is returned. Any other names in the group
*     expression are ignored. Supplemental information describing the 
*     separate fields in the catalogue specification are also returned.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     MODE = CHARACTER * ( * ) (Given)
*        Type of catalogue access required: 'READ', 'UPDATE' or 'WRITE'.
*     CI = INTEGER (Returned)
*        catalogue identifier.
*     FIELDS( 5 ) = CHARACTER * ( * ) (Given)
*        Each element contains the following on exit:
*
*           1 - FITS extension specification (eg "{3}") if any
*           2 - File type
*           3 - Base file name
*           4 - Directory path
*           5 - Full catalogue specification 
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
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
      INCLUDE 'CAT_PAR'          ! catalogue constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER MODE*(*)

*  Arguments Returned:
      INTEGER CI
      CHARACTER FIELDS( 5 ) *(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRP               ! Group holding supplied catalogue names
      INTEGER SIZE               ! No. of supplied catalogue names
      LOGICAL FLAG               ! User wants to supply more catalogues?
*.

*  Set an initial value for the CI argument.
      CI = CAT__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a group of catalogues from the environment using the supplied parameter.
*  There is no need to loop if a group expression is given which is 
*  terminated by a flag character since we only want one catalogue.
      IGRP = GRP__NOID
      CALL CTG_ASSOC( PARAM, IGRP, SIZE, FLAG, STATUS )

*  Get the supplemental fields for the first catalogue in the group.
      CALL CTG_GTSUP( IGRP, 1, FIELDS, STATUS )

*  Get the first catalogue from the group.         
      CALL CTG_CATAS( IGRP, 1, MODE, CI, STATUS )

*  Delete the group.
      CALL GRP_DELET( IGRP, STATUS )

*  If an error occurred, annul the catalogue.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL CAT_TRLSE( CI, STATUS )
         CI = CAT__NOID
      END IF

      END
