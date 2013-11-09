      SUBROUTINE BDI1_ARYSHP( LOC, MAXNDIM, DIMS, NDIM, TYPE, STATUS )
*+
*  Name:
*     BDI1_ARYSHP

*  Purpose:
*     Determine dimension and type of primitive or ARRAY structure object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_ARYSHP( LOC, MAXNDIM, DIMS, NDIM, TYPE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     LOC = CHARACTER*(DAT__SZLOC) (given)
*        Locator to HDS object
*     MAXNDIM = INTEGER (given)
*        Maximum number of dimensions to export
*     DIMS[NDIM] = INTEGER (returned)
*        Sizes of each of the NDIM dimensions of the object
*     NDIM = INTEGER (returned)
*        Dimensionality of object, zero for scalar
*     TYPE = CHARACTER*(*) (returned)
*        The basic data type
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
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      CHARACTER*(DAT__SZLOC)	LOC
      INTEGER			MAXNDIM

*  Arguments Returned:
      INTEGER			NDIM, DIMS(*)
      CHARACTER*(*)		TYPE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*8               VARNT			! Array variant

      LOGICAL			PRIM			! Object is primitive?
      LOGICAL			THERE			! Object exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Object is primitive?
      CALL DAT_PRIM( LOC, PRIM, STATUS )

*  Easy if it is...
      IF ( PRIM ) THEN
        CALL DAT_SHAPE( LOC, MAXNDIM, DIMS, NDIM, STATUS )
        CALL DAT_TYPE( LOC, TYPE, STATUS )

*  Structure array object
      ELSE

*    Does VARIANT exist?
        CALL DAT_THERE( LOC, 'VARIANT', THERE, STATUS )
        IF ( THERE ) THEN
          CALL CMP_GET0C( LOC, 'VARIANT', VARNT, STATUS )
        ELSE
          CALL DAT_THERE( LOC, 'DATA', THERE, STATUS )
          IF ( THERE ) THEN
            VARNT = 'SIMPLE'
          ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Unable to derive array type'/
     :                    /' from object', STATUS )
          END IF

        END IF

*    Switch on variant. The simple case first
        IF ( VARNT .EQ. 'SIMPLE' ) THEN

*      Get dimensions from DATA component
          CALL CMP_SHAPE( LOC, 'DATA', MAXNDIM, DIMS, NDIM, STATUS )
          CALL CMP_TYPE( LOC, 'DATA', TYPE, STATUS )

*    Then spaced arrays
        ELSE IF ( VARNT .EQ. 'SPACED' ) THEN

*      Check for single DIMENSION
          CALL DAT_THERE( LOC, 'DIMENSION', THERE, STATUS )
          IF ( THERE ) THEN
            NDIM = 1
            CALL CMP_GET0I( LOC, 'DIMENSION', DIMS(1), STATUS )
          ELSE
            CALL CMP_GET1I( LOC, 'DIMENSIONS', MAXNDIM, DIMS, NDIM,
     :                      STATUS )
          END IF
          CALL CMP_TYPE( LOC, 'BASE', TYPE, STATUS )

*    And scaled arrays
        ELSE IF ( VARNT .EQ. 'SCALED' ) THEN

          CALL CMP_SHAPE( LOC, 'DATA', MAXNDIM, DIMS, NDIM, STATUS )
          CALL CMP_TYPE( LOC, 'ZERO', TYPE, STATUS )

        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Unrecognised or unsupported '/
     :                            /'array VARIANT', STATUS )

        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_ARYSHP', STATUS )

      END
