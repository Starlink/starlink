      SUBROUTINE ATL1_GTGRP( PARAM, IGRP, STATUS )
*+
*  Name:
*     ATL1_GTGRP

*  Purpose:
*     Obtain lines of text from a parameter, and store them in a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_GTGRP( PARAM, IGRP, STATUS )

*  Description:
*     Currently this routine expects the parameter to be associated with
*     a text file, and the returned group contains the lines of the file.
*     In future it may be possible to add other ways of using the
*     parameter (i.e. by associating it with objects other than text
*     files).

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     IGRP = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FNAME*120
      CHARACTER GRPEXP*(GRP__SZGEX)
      INTEGER ADDED
      INTEGER IAT
      INTEGER IPAR
      INTEGER SIZE
      LOGICAL FLAG
*.

*  Initialise.
      IGRP = GRP__NOID

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the value of the parameter using SUBPAR to avoid interpretation of
*  the string by the parameter system.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, FNAME, STATUS )

*  Form a group expression containing an indirection element which will
*  cause GRP to read the specified file.
      GRPEXP = '^'
      IAT = 1
      CALL CHR_APPND( FNAME, GRPEXP, IAT )

*  Create a new group. 
      CALL GRP_NEW( ' ', IGRP, STATUS )

*  Switch off all control characters so that nothing gets interpreted by
*  GRP.
      CALL GRP_SETCC( IGRP, 'COM,DEL,NAM,SEP,OPEN_N,CLOSE_N,FL,'//
     :                'OPEN_K,CLOSE_K', '%%%%%%%%%', STATUS ) 

*  Read the file into the group.
      CALL GRP_GRPEX( GRPEXP( : IAT ), GRP__NOID, IGRP, SIZE, ADDED,
     :                FLAG, STATUS )     

*  Delete the group if an error occurred.
      IF( STATUS .NE. SAI__OK ) CALL GRP_DELET( IGRP, STATUS )

      END
