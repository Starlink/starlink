      SUBROUTINE ADI2_ADDHDU( ID, HDU, IHDU, HDUTYP, HDUID, STATUS )
*+
*  Name:
*     ADI2_ADDHDU

*  Purpose:
*     Add an HDU description to a FITSfile

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_ADDHDU( ID, HDU, IHDU, HDUTYP, HDUID, STATUS )

*  Description:
*     Add HDU description to file.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARACTER*(*) (given)
*        Name of the HDU we're adding
*     IHDU = INTEGER (given)
*        The consecutive HDU number
*     HDUTYP = INTEGER (given)
*        HDU type as returned by FITSIO
*     HDUID = INTEGER (returned)
*        ADI identifier of FITShdu object
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
      INTEGER			ID, IHDU, HDUTYP
      CHARACTER*(*)		HDU

*  Arguments Returned:
      INTEGER			HDUID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      INTEGER			HCID			! HDU container
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the HDU container in the file
      CALL ADI_FIND( ID, 'Hdus', HCID, STATUS )

*  Create description structure
      CALL ADI_NEW0( 'FITShdu', HDUID, STATUS )
      CALL ADI_CPUT0I( HDUID, 'Ihdu', IHDU, STATUS )
      CALL ADI_CPUT0I( HDUID, 'HduType', HDUTYP, STATUS )

*  Write HDU index entry
      CALL ADI2_PUTHDI( ID, HDU, HDUID, IHDU, STATUS )

*  Write to HDU container
      CALL ADI_CPUTID( HCID, HDU, HDUID, STATUS )

*  Write its name
      CALL ADI_CPUT0C( HDUID, 'Name', HDU, STATUS )

*  Write reference to parent file
      CALL ADI_CPUTREF( HDUID, '.File', ID, STATUS )

*  Clone the identifier for the caller
      CALL ADI_CLONE( HDUID, HDUID, STATUS )

*  Release the HDU container
      CALL ADI_ERASE( HCID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_ADDHDU', STATUS )

      END
