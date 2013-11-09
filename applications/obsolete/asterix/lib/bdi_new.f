      SUBROUTINE BDI_NEW( CLASS, NDIM, DIMS, TYPE, ID, STATUS )
*+
*  Name:
*     BDI_NEW

*  Purpose:
*     Create a new object for manipulation by BDI, and initialise

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_NEW( CLASS, NDIM, DIMS, TYPE, ID, STATUS )

*  Description:
*     Create a new instance of a model data object for use by BDI, and
*     fill in the data members essential for use by BDI. Once dimensions
*     and basic data type have been defined BDI can usually create all
*     the data items it knows about without further information.

*  Arguments:
*     CLASS = CHARACTER*(*) (given)
*        The class of the model data object to be created
*     NDIM = INTEGER (given)
*        Dimensionality, 0 for scalar
*     DIMS[NDIM] = INTEGER (given)
*        Sizes of each of the NDIM dimensions of the object
*     TYPE = CHARACTER*(*) (given)
*        The basic data type of the numeric data in the dataset
*     ID = INTEGER (returned)
*        ADI identifier of the new data object
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
      INCLUDE 'AST_PKG'

*  Arguments Given:
      CHARACTER*(*)		CLASS, TYPE
      INTEGER			NDIM, DIMS(*)

*  Arguments Returned:
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( BDI__PKG ) ) CALL BDI0_INIT( STATUS )

*  Create the new object
      CALL ADI_NEW0( CLASS, ID, STATUS )

*  Define shape
      CALL BDI_SETSHP( ID, NDIM, DIMS, STATUS )

*  Define basic type
      CALL BDI_SETTYP( ID, TYPE, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_NEW', STATUS )

      END
