      SUBROUTINE ADI2_DEFHP( FID, HDU, NBYTES, STATUS )
*+
*  Name:
*     ADI2_DEFHP

*  Purpose:
*     Define the heap size for the current HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_DEFHP( FID, HDU, NBYTES, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of current FITS file
*     HDU = CHARACTER*(*)
*        Logical name of HDU
*     NBYTES = INTEGER (given)
*        Size of the heap
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
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     1 Mar 1995 (DJA):
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
      INTEGER			FID			! FITSfile object
      CHARACTER*(*)		HDU			! HDU name
      INTEGER			NBYTES			! Size of heap

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			FSTAT			! FITSIO status
      INTEGER			HID			! HDU identifier
      INTEGER			LUN			! Logical unit
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate HDU
      CALL ADI2_LOCHDU( FID, HDU, HID, STATUS )

*  Get logical unit
      CALL ADI2_GETLUN( FID, LUN, STATUS )

*  Set the heap size
      FSTAT = 0
      CALL FTPTHP( LUN, NBYTES, FSTAT )
      IF ( FSTAT .NE. 0 ) CALL ADI2_FITERP( FSTAT, STATUS )

*  Flag HDU as defined and release it
      CALL ADI_CPUT0L( HID, '.DEF_END', .TRUE., STATUS )
      CALL ADI_ERASE( HID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_DEFHP', STATUS )

      END
