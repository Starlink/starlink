      SUBROUTINE ADI2_MOVHDU( FID, HDU, ID, STATUS )
*+
*  Name:
*     ADI2_MOVHDU

*  Purpose:
*     Locate HDU and set FITSIO HDU cursor to that HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_MOVHDU( FID, HDU, ID, STATUS )

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
*     {routine_references}...

*  Keywords:
*     package:adi, usage:private, FITS

*  Copyright:
*     {routine_copyright}

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
      CHARACTER*20		HDUTYPE			!

      INTEGER			FSTAT			! FITSIO status
      INTEGER			LUN			! Logical unit
      INTEGER			NHDU			! HDU number

      LOGICAL			CREATED			! Did we create object?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the HDU
      CALL ADI2_LOCHDU1( FID, HDU, .TRUE., ID, CREATED, STATUS )

*  Get the HDU number
      CALL ADI_CGET0I( FID, '.NHDU', NHDU, STATUS )

*  Ensure previous HDU's data areas are defined
      IF ( NHDU .GT. 1 ) THEN
        CALL ADI2_CHKPRV( FID, NHDU-1, .FALSE., STATUS )
      END IF

*  Move the specified unit
      CALL ADI2_GETLUN( FID, LUN, STATUS )
      FSTAT = 0
      CALL FTMAHD( LUN, NHDU, HDUTYPE, FSTAT )
      IF ( (FSTAT.NE.0) .AND. (FSTAT.NE.107) ) THEN
        CALL ADI2_FITERP( FSTAT, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_MOVHDU', STATUS )

      END
