      SUBROUTINE BDI_LINK( CLASS, NDIM, DIMS, TYPE, FID, STATUS )
*+
*  Name:
*     BDI_LINK

*  Purpose:
*     Create new BinDS derived object and link to an existing file object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_LINK( CLASS, NDIM, DIMS, TYPE, FID, STATUS )

*  Description:
*     This routine is used to create new BinDS derived data objects and
*     link them to low level file objects. The supplied file identifier
*     is updated and should be used to erase the object chain after this
*     routine has been invoked.

*  Arguments:
*     CLASS = CHARACTER*(*) (given)
*        The class of the new interface object
*     NDIM = INTEGER (given)
*        The dimensionality of the new BinDS object
*     DIMS[NDIM] = INTEGER (given)
*        The dimensions of the new BinDS object
*     TYPE = CHARACTER*(*) (given)
*        The basic data type of the new BinDS object
*     FID = INTEGER (given and returned)
*        The low level file object to link to
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
*     4 Dec 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*)		CLASS, TYPE
      INTEGER			NDIM, DIMS(*)

*  Arguments Given and Returned:
      INTEGER			FID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			TID			! Temporary identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create new object
      CALL BDI_NEW( CLASS, NDIM, DIMS, TYPE, TID, STATUS )

*  Link it if creation went ok
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL ADI_SETLNK( TID, FID, STATUS )

*    Update file object if successful
        IF ( STATUS .EQ. SAI__OK ) THEN
          FID = TID
        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_LINK', STATUS )

      END
