      SUBROUTINE ADI2_LOCHDU( FID, HDU, ID, STATUS )
*+
*  Name:
*     ADI2_LOCHDU

*  Purpose:
*     Locate HDU buffer structure in FITSfile object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_LOCHDU( FID, HDU, ID, STATUS )

*  Description:
*     Locates (and creates if necessary) the buffer structures for storing
*     FITS keyword data in FITSfile derived classes

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARACTER*(*) (given)
*        Logical HDU whose keyword this is. Blank for primary
*     ID = INTEGER (returned)
*        ADI identifier of FITSfile object
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private, FITS

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     2 Feb 1995 (DJA):
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
      INTEGER                   FID                     ! File identifier
      CHARACTER*(*)             HDU                     ! HDU name

*  Arguments Returned:
      INTEGER                   ID                      ! Structure identifier

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      LOGICAL			CREATED			! Did we create object?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the HDU
      CALL ADI2_LOCHDU1( FID, HDU, .TRUE., ID, CREATED, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_LOCHDU', STATUS )

      END



      SUBROUTINE ADI2_LOCHDU1( FID, HDU, CANCRE, ID, CREATED, STATUS )
*+
*  Name:
*     ADI2_LOCHDU1

*  Purpose:
*     Locate HDU buffer structure in FITSfile object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_LOCHDU1( FID, HDU, CANCRE, ID, CREATED, STATUS )

*  Description:
*     Locates (and creates if necessary) the buffer structures for storing
*     FITS keyword data in FITSfile derived classes

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARSCTER*(*) (given)
*        Logical HDU whose keyword this is. Blank for primary
*     ID = INTEGER (returned)
*        ADI identifier of FITSfile object
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private, FITS

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     2 Feb 1995 (DJA):
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
      INTEGER                   FID                     ! File identifier
      CHARACTER*(*)             HDU                     ! HDU name
      LOGICAL			CANCRE			! Can create?

*  Arguments Returned:
      INTEGER                   ID                      ! Structure identifier
      LOGICAL			CREATED			! Did we create object?

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*40		LHDU			! Local copy of HDU

      INTEGER			HCID			! HDU container
      INTEGER			HLEN			! Length of LHDU
      INTEGER			I			! Loop over LHDU

      LOGICAL			THERE			! Object exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      IF ( CANCRE ) CREATED = .FALSE.

*  Locate the approriate place depending on the HDU value. Blank means
*  primary HDU
      IF ( HDU(1:1) .EQ. ' ' ) THEN
        CALL ADI_FIND( FID, 'PRIMARY', ID, STATUS )
        LHDU = 'PRIMARY'
        HLEN = 7

*  Otherwise named HDU in the EXTENSIONS structure
      ELSE

*    Make local copy of name, and translate embedded spaces to underscores
        LHDU = HDU
        HLEN = CHR_LEN(LHDU)
        IF ( INDEX(LHDU(:HLEN),' ') .GT. 0 ) THEN
          DO I = 1, HLEN
            IF ( LHDU(I:I) .EQ. ' ' ) LHDU(I:I) = '_'
          END DO
        END IF

      END IF

*  Locate HDU container
      CALL ADI_FIND( FID, 'Hdus', HCID, STATUS )

*  Has HDU been created yet?
      CALL ADI_THERE( HCID, LHDU(:HLEN), THERE, STATUS )
      IF ( CANCRE .AND. .NOT. THERE ) THEN
        CALL ADI2_NEWHDU( FID, LHDU(:HLEN), HDU, STATUS )
        CREATED = .TRUE.
      END IF
      CALL ADI_FIND( HCID, LHDU(:HLEN), ID, STATUS )

*  Release HDU container
      CALL ADI_ERASE( HCID, STATUS )

      END
