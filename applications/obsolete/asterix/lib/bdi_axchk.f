      SUBROUTINE BDI_AXCHK( ID, IAX, ITEMS, OKS, STATUS )
*+
*  Name:
*     BDI_AXCHK

*  Purpose:
*     Check that the named axis items exist and have valid data

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_AXCHK( ID, IAX, ITEMS, OKS, STATUS )

*  Description:
*     Checks the existance and integrity of the named axis items. Wrap up
*     of the BDI_CHK routine to enable numeric indexing of axis items.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     IAX = INTEGER (given)
*        Axis number of the items to be checked
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be checked
*     OKS[] = LOGICAL (returned)
*        Are named items ok?
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			ID, IAX
      CHARACTER*(*)		ITEMS

*  Arguments Returned:
      LOGICAL			OKS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*2		ASTR			! Axis number string

      INTEGER			C1, C2			! Character pointers
      INTEGER			IITEM			! Item counter
      INTEGER			NDIG			! # digits used in ASTR
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct axis number string
      CALL CHR_ITOC( IAX, ASTR, NDIG )

*  Loop over items while more of them and status is ok
      CALL UDI0_CREITI( ITEMS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Check the axis item
        CALL BDI_CHK( ID, 'Axis_'//ASTR(:NDIG)//'_'//ITEMS(C1:C2),
     :                OKS(IITEM), STATUS )

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_AXCHK', STATUS )

      END
