      SUBROUTINE ADI2_DEFBTB( FID, HDU, NROWS, NFLDS, NAMES, TYPES,
     :                        UNITS, VARIDAT, STATUS )
*+
*  Name:
*     ADI2_DEFBTB

*  Purpose:
*     Define the data area of a BINTABLE HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_DEFBTB( FID, HDU, NROWS, NFLDS, NAMES, TYPES,
*                       UNITS, VARIDAT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITS file object
*     HDU = CHARACTER*(*) (given)
*        Name of the HDU
*     NROWS = INTEGER (given)
*        Number of rows in the table
*     NFLDS = INTEGER (given)
*        Number of fields in the table
*     NAMES[] = CHARACTER*(*) (given)
*        Names of the table fields
*     TYPES[] = CHARACTER*(*) (given)
*        Types of the table fields
*     UNITS[] = CHARACTER*(*) (given)
*        Units of the table fields
*     VARIDAT = LOGICAL (given)
*        Table contains variable length items?
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
*     28 Feb 1995 (DJA):
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
      INTEGER			FID			! See above
      CHARACTER*(*)		HDU			!
      INTEGER			NROWS			!
      INTEGER			NFLDS			!
      CHARACTER*(*)		NAMES(*)		!
      CHARACTER*(*)		TYPES(*)		!
      CHARACTER*(*)		UNITS(*)		!
      LOGICAL			VARIDAT			!

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			FSTAT			! FITSIO status
      INTEGER			LUN			! Logical unit
      INTEGER			NKEY			! Number of keywords
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FSTAT = 0

*  Locate the HDU buffer
      CALL ADI2_LOCHDU( FID, HDU, HID, STATUS )

*  Ensure all previous HDU's data area's are now defined
      CALL ADI_CGET0I( HID, '.IHDU', CHDU, STATUS )
      IF ( CHDU .GT. 1 ) THEN
        CALL ADI2_CHKPRV( FID, CHDU-1, STATUS )
      END IF

*  Get logical unit
      CALL ADI2_GETLUN( FID, LUN, STATUS )

*  Reserve space for keywords
      CALL ADI_NCMP( HID, NKEY, STATUS )
      IF ( NKEY .GT. 0 ) THEN
        CALL FTHDEF( LUN, NKEY, FSTAT )
      END IF

*  Define BINTABLE extension
      FSTAT = 0
      CALL FTPHBN( LUN, NROWS, NFLDS, NAMES, TYPES, UNITS, HDU,
     :             VARIDAT, FSTAT )
      CALL FTRDEF( LUN, FSTAT )
      IF ( FSTAT .EQ. 0 ) THEN
        CALL ADI_CPUT0L( HID, '.DEF_START', .TRUE., STATUS )
        CALL ADI_CPUT0L( HID, '.DEF_END', (.NOT. VARIDAT), STATUS )
      ELSE
        CALL ADI2_FITERP( FSTAT, STATUS )

      END IF

*  Free the buffer
      CALL ADI2_ERASE( HID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_DEFBTB', STATUS )
      END IF

      END
