      SUBROUTINE ADI2_ISHAPE( HDUID, MAXDIM, DIMS, NDIM, STATUS )
*+
*  Name:
*     ADI2_ISHAPE

*  Purpose:
*     Return dimensions of specified extension

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_ISHAPE( HDUID, MAXDIM, DIMS, NDIM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of FITShdu object
*     MAXDIM = INTEGER (given)
*        Maximum number of dimensions acceptable
*     DIMS[] = INTEGER (returned)
*        The dimensions of the image extension
*     NDIM = INTEGER (returned)
*        Dimensionality of image extension
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     26 Jul 1995 (DJA):
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
      INTEGER			HDUID, MAXDIM

*  Arguments Returned:
      INTEGER			DIMS(*), NDIM

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*72		CMNT			! (rb)
      INTEGER			IAX			! Loop over axes
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get dimensionality
      CALL ADI2_HGKYI( HDUID, 'NAXIS', NDIM, CMNT, STATUS )

*  Trap buffer overflow
      IF ( NDIM .GT. MAXDIM ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Dimensions list would overflow user buffer',
     :                STATUS )
      ELSE
        DO IAX = 1, NDIM
          CALL ADI2_HGKYII( HDUID, 'NAXIS', IAX, DIMS(IAX), CMNT,
     :                      STATUS )
        END DO

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_ISHAPE', STATUS )

      END
