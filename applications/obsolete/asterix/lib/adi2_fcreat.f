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
*     {routine_description}

*  Arguments:
*     {argument_name}[dimensions] = {data_type} ({argument_access_mode})
*        {argument_description}
*     STATUS = INTEGER ({status_access_mode})
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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

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
      INTEGER			FILE			! File name to open
      INTEGER			ID			! Template object

*  Arguments Returned:
      INTEGER			FID			! New file object

*  Status:
      INTEGER 			STATUS                  ! Global status

*  External References:
C      [external_declaration]
C      {data_type} {external_name} ! [external_description]

*  Local Constants:
c      {data_type} {constant_name} ! [constant_description]
C      PARAMETER ( {constant_name} = {cons} )

*  Local Variables:
      CHARACTER*132		FNAME			! File name
      CHARACTER*8 		KEYWRD			! Keyword name

      INTEGER			BSIZE			! Block size
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
      CALL ADI2_PARSE( FNAME, LFILEC, HDU, KEYWRD, STATUS )

*  Grab logical unit from system
      CALL FIO_GUNIT( LUN, STATUS )

	CALL ADI_PRINT( ID, STATUS )

*  Try to create file
      BSIZE = 1
      FSTAT = 0
      CALL FTINIT( LUN, FNAME(:LFILEC), BSIZE, FSTAT )

*  Created ok?
      IF ( FSTAT .EQ. 0 ) THEN

*    Create new instance of a FITSfile object
        CALL ADI_NEW0( 'FITSfile', FID, STATUS )

*    Write HDU number
        CALL ADI_CPUT0I( FID, '.HDU', HDU, STATUS )

*    Write extra info into the file handle object
        CALL ADI_CPUT0I( FID, '.LUN', LUN, STATUS )
        CALL ADI_CPUT0I( FID, '.BLOCK_SIZE', BSIZE, STATUS )

      ELSE

*    Return unit to system
        CALL FIO_PUNIT( LUN, STATUS )

*    Report an error
        CALL ADI2_FITERP( FSTAT, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FCREAT', STATUS )

      END
