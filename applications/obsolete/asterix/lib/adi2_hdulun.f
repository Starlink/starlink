      SUBROUTINE ADI2_HDULUN( HDUID, LUN, STATUS )
*+
*  Name:
*     ADI2_HDULUN

*  Purpose:
*     Get logical unit associated with an HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_HDULUN( HDUID, LUN, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of FITShdu object
*     LUN = INTEGER (returned)
*        Logical unit used by FITSIO
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
*     11 Sep 1995 (DJA):
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
      INTEGER			HDUID

*  Arguments Returned:
      INTEGER			LUN

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
c     INTEGER			FID			! File identifier
c     INTEGER			HDUTYP			! HDU type
c     INTEGER			IHDU			! HDU number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate file via reference (more like adi2_getlun? - rb)
c     CALL ADI_CGETREF( HDUID, '.File', FID, STATUS )
c     CALL ADI_GETFILE( HDUID, FID, STATUS )

*  Read logical unit
      CALL ADI_CGET0I( HDUID, 'Lun', LUN, STATUS )

*  Extract HDU number and move to it
c     CALL ADI_CGET0I( HDUID, 'Ihdu', IHDU, STATUS )
c     CALL ADI2_MVAHDU( FID, LUN, IHDU, HDUTYP, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_HDULUN', STATUS )

      END
