      SUBROUTINE POL1_CPCAT( CIIN, CIOUT, STATUS )
*+
*  Name:
*     POL1_CPCAT

*  Purpose:
*     Copy a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPCAT( CIIN, CIOUT, STATUS )

*  Description:
*     This routine copies an entire catalogue, excluding the textual
*     information.

*  Arguments:
*     CIIN = INTEGER (Given)
*        The CAT identifier for the input catalogue.
*     CIOUT = INTEGER (Given)
*        The CAT identifier for the output catalogue.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.

*  Authors:
*     DSB: David S. Berry (EAO)
*     ACD: A C Davenhall (Edinburgh)
**     {enter_new_authors_here}

*  History:
*     29-JUN-2017 (DSB):
*        Original version, based on cap_cpcat.f by ACD.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT constants

*  Arguments Given:
      INTEGER CIIN
      INTEGER CIOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER  CIP               ! Identifier for parent input catalogue
      INTEGER  FIIN(CAT__MXCOL)  ! Column identifiers for input  catalogue.
      INTEGER  FIOUT(CAT__MXCOL) !   "         "       "  output     "    .
      INTEGER  IDTYPE            ! Type of input catalogue identifier.
      INTEGER  NUMCOL            ! Number of columns in the input catalogue.
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the given input catalogue is not a genuine catalogue (for
*  example, it is a selection) then get the identifier for the
*  corresponding parent catalogue.
      CALL CAT_TIDTP( CIIN, IDTYPE, STATUS )
      IF (IDTYPE .NE. CAT__CITYP) THEN
         CALL CAT_TIDPR( CIIN, CIP, STATUS )
      ELSE
         CIP = CIIN
      END IF

*  Create the output catalogue.  First create columns corresponding
*  to the input catalogue, then copy the parameters, followed by
*  the table of values.
      CALL POL1_CPCOL( CIP, CIOUT, CAT__MXCOL, NUMCOL, FIIN, FIOUT,
     :                 STATUS)
      CALL POL1_CPPAR( CIP, CIOUT, STATUS )
      CALL POL1_CPTAB( CIIN, CIOUT, NUMCOL, FIIN, FIOUT, STATUS )

      END
