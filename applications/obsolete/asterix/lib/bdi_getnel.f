      SUBROUTINE BDI_GETNEL( ID, NELM, STATUS )
*+
*  Name:
*     BDI_GETNEL

*  Purpose:
*     Get total number of data elements of data model object supported by BDI

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_GETNEL( ID, NELM, STATUS )

*  Description:
*     Extracts the product of the dimensions of a data model object for
*     subsequent manipulation by BDI.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     NDIM = INTEGER (returned)
*        Product of dimensions of data object
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
      INTEGER			ID

*  Arguments Returned:
      INTEGER			NELM

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			DIMS(ADI__MXDIM)	! Dimensions
      INTEGER			NDIM			! Dimensionality
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get dimensions
      CALL BDI_GETSHP( ID, ADI__MXDIM, DIMS, NDIM, STATUS )

*  Find product
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_GETNEL', STATUS )

      END
