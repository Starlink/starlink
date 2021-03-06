      SUBROUTINE BDI_GETSHP( ID, MXNDIM, DIMS, NDIM, STATUS )
*+
*  Name:
*     BDI_GETSHP

*  Purpose:
*     Get dimensions of data model object supported by BDI

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_GETSHP( ID, MXNDIM, DIMS, NDIM, STATUS )

*  Description:
*     Extracts the dimensions of a data model object for subsequent
*     manipulation by BDI.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     MXNDIM = INTEGER (given)
*        Maximum number of dimensions to export
*     DIMS[NDIM] = INTEGER (returned)
*        Sizes of each of the NDIM dimensions of the object
*     NDIM = INTEGER (returned)
*        Dimensionality of object, zero for scalar
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
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			ID, MXNDIM

*  Arguments Returned:
      INTEGER			NDIM, DIMS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			IDIM			! Loop over dimensions

      LOGICAL			SCALAR			! Input is scalar?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( BDI__PKG ) ) CALL BDI0_INIT( STATUS )

*  Object is derived from Scalar?
      CALL ADI_DERVD( ID, 'Scalar', SCALAR, STATUS )
      IF ( SCALAR ) THEN

*    Set dimensions
        NDIM = 0
        DO IDIM = 1, MXNDIM
          DIMS(IDIM) = 0
        END DO

      ELSE

*    How big is dimensions array
        CALL ADI_CSIZE( ID, 'SHAPE', NDIM, STATUS )
        IF ( NDIM .GT. MXNDIM ) THEN
          CALL BDI0_DESCID( ID, 'ID', STATUS )
          CALL MSG_SETI( 'ND', NDIM )
          CALL MSG_SETI( 'NA', MXNDIM )
          STATUS = SAI__ERROR
          CALL ERR_REP( 'BDI_GETSHP_0', 'Input object ^ID has '/
     :                  /'^ND dimensions, whereas the calling '/
     :                  /'program can only cope with ^NA', STATUS )

        ELSE

*    Read dimensions
          CALL ADI_CGET1I( ID, 'SHAPE', MXNDIM, DIMS, NDIM, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'BDI_GETSHP_1', 'Unable to read data object'/
     :                  /' dimensions', STATUS )
          END IF
        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_GETSHP', STATUS )

      END
