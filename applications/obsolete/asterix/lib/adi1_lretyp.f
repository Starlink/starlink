      SUBROUTINE ADI1_LRETYP( LHS, DEFTYP, RHS, STATUS )
*+
*  Name:
*     ADI1_LRETYP

*  Purpose:
*     Retype an HDS object based on LHS or default

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_UNLNK( LHS, DEFTYP, RHS, STATUS )

*  Description:
*     Performs tidying up prior to ADI file link between high level objects
*     EventDS and HDSfile being destroyed.

*  Arguments:
*     LHS = INTEGER (given)
*        ADI identifier of high level object
*     DEFTYP = CHARACTER*(*) (given)
*        The default HDS type to use if one isn't specified by the data model
*     RHS = INTEGER (given)
*        ADI identifier of low level object
*     STATUS = INTEGER (given)
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      2 Jan 1996 (DJA):
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
      INTEGER                   LHS, RHS
      CHARACTER*(*)		DEFTYP

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)    LOC                     ! Object locator
      CHARACTER*6               MODE                    ! Access mode
      CHARACTER*40              TYPE                    ! Data type

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get mode
      CALL ADI_CGET0C( RHS, 'MODE', MODE, STATUS )
      IF ( (STATUS .EQ. SAI__OK) .AND. (MODE(1:1).EQ.'W') ) THEN

*    Get locator and existing type
        CALL ADI1_GETLOC( RHS, LOC, STATUS )
        CALL DAT_TYPE( LOC, TYPE, STATUS )

*    Type is unknown?
        IF ( (STATUS .EQ. SAI__OK) .AND.
     :       (TYPE(1:7) .EQ. 'UNKNOWN') ) THEN
          CALL ADI_THERE( LHS, 'DatasetType', THERE, STATUS )
          IF ( THERE ) THEN
            CALL ADI_CGET0C( LHS, 'DatasetType', TYPE, STATUS )
          ELSE
            CALL ADI_TYPE( LHS, TYPE, STATUS )
          END IF
          IF ( TYPE .LE. ' ' ) TYPE = DEFTYP

*     Retype the object
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_RETYP( LOC, TYPE, STATUS )
          END IF

        END IF
      END IF

*  Always annul bad status
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

      END
