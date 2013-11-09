      SUBROUTINE ADI2_FCREAT( FILE, ID, FID, STATUS )
*+
*  Name:
*     ADI2_FCREAT

*  Purpose:
*     Create a new FITS file, or FITS file component

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADI2_FCREAT( FILE, ID, FID, STATUS )

*  Description:
*     Create a new FITS file.

*  Arguments:
*     FILE = INTEGER (given)
*        ADI identifier of string holding name of file to create
*     ID = INTEGER (given)
*        ADI identifier of object to link to the new file
*     FID = INTEGER (returned)
*        The identifier of the FITSfile object created
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
*     package:adi, usage:private, FITS

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     1 Feb 1995 (DJA):
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
      INTEGER			FILE, ID

*  Arguments Returned:
      INTEGER			FID

*  Status:
      INTEGER 			STATUS                  ! Global status

*  Local Variables:
      CHARACTER*132		FNAME			! File name
      CHARACTER*80 		FPATH			! Sub-file path stuff

      INTEGER			BSIZE			! Block size
      INTEGER			FPLEN			! FPATH length
      INTEGER			FSTAT			! FITSIO status
      INTEGER			HDU			! HDU number
      INTEGER			LFILEC			! Last filename char
      INTEGER			LUN			! Logical unit number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract filename
      CALL ADI_GET0C( FILE, FNAME, STATUS )

*  Parse the file specification into file, hdu and keyword names
      CALL ADI2_PARSE( FNAME, LFILEC, HDU, FPATH, FPLEN, STATUS )

*  Grab logical unit from system
      CALL FIO_GUNIT( LUN, STATUS )

*  Rename any old file if necessary
      CALL ADI2_EXIREN( FNAME(:LFILEC), STATUS )

*  Try to create file
      BSIZE = 1
      FSTAT = 0
      CALL FTINIT( LUN, FNAME(:LFILEC), BSIZE, FSTAT )

*  Created ok?
      IF ( FSTAT .EQ. 0 ) THEN

*    Create new instance of a FITSfile object
        CALL ADI_NEW0( 'FITSfile', FID, STATUS )

*    Write HDU number
        CALL ADI_CPUT0I( FID, 'UserHDU', HDU, STATUS )

*    Write sub-file structure
        IF ( FPLEN .GT. 0 ) THEN
          CALL ADI_CPUT0C( FID, 'Fpath', FPATH(:FPLEN), STATUS )
        END IF

*    Write extra info into the file handle object
        CALL ADI_CPUT0I( FID, 'Lun', LUN, STATUS )
        CALL ADI_CPUT0I( FID, 'BlockSize', BSIZE, STATUS )

      ELSE

*    Return unit to system
        CALL FIO_PUNIT( LUN, STATUS )

*    Report an error
        CALL ADI2_FITERP( FSTAT, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FCREAT', STATUS )

      END
