      SUBROUTINE ADI2_DEFIMG( FID, HDU, NDIM, DIMS, TYPE, STATUS )
*+
*  Name:
*     ADI2_DEFIMG

*  Purpose:
*     Define the data area of a IMAGE HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_DEFIMG( FID, HDU, NDIM, DIMS, TYPE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITS file object
*     HDU = CHARACTER*(*) (given)
*        Name of the HDU
*     NDIM = INTEGER (given)
*        Number of dimensions of image extension
*     DIMS[NDIM] = INTEGER (given)
*        Sizes of the dimensions
*     TYPE = CHARACTER*(*) (given)
*        Type of the image extension data
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
      INTEGER			NDIM			!
      INTEGER			DIMS(*)			!
      CHARACTER*(*)		TYPE			!

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*40		EXTNAME			! Name of extension

      INTEGER			BITPIX			! FITS type marker
      INTEGER			FSTAT			! FITSIO status
      INTEGER			HID			! HDU identifier
      INTEGER			KCID			! Keyword container
      INTEGER			LUN			! Logical unit
      INTEGER			NKEY			! Number of keywords
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FSTAT = 0

*  Locate the HDU buffer
      CALL ADI2_MOVHDU( FID, HDU, HID, STATUS )

*  Get logical unit
      CALL ADI2_GETLUN( FID, LUN, STATUS )

*  Reserve space for keywords
      CALL ADI_FIND( HID, 'Keys', KCID, STATUS )
      CALL ADI_NCMP( KCID, NKEY, STATUS )
      IF ( NKEY .GT. 0 ) THEN
        CALL FTHDEF( LUN, NKEY, FSTAT )
      END IF
      CALL ADI_ERASE( KCID, STATUS )

*  Decide on BITPIX
      IF ( TYPE .EQ. 'BYTE' ) THEN
        BITPIX = 8
      ELSE IF ( TYPE .EQ. 'WORD' ) THEN
        BITPIX = 16
      ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
        BITPIX = 32
      ELSE IF ( TYPE .EQ. 'REAL' ) THEN
        BITPIX = -32
      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
        BITPIX = -64
      END IF

*  Define IMAGE extension
      FSTAT = 0
      CALL ADI_CGET0C( HID, 'Name', EXTNAME, STATUS )
      CALL FTPHPR( LUN, .TRUE., BITPIX, NDIM, DIMS, 0, 1, .TRUE.,
     :             FSTAT )
      IF ( FSTAT .EQ. 0 ) THEN
        CALL ADI_CPUT0L( HID, 'DefStart', .TRUE., STATUS )
        CALL ADI_CPUT0L( HID, 'DefEnd', .TRUE., STATUS )
      ELSE
        CALL ADI2_FITERP( FSTAT, STATUS )
      END IF

*  Define its size
      CALL FTRDEF( LUN, FSTAT )
      IF ( FSTAT .NE. 0 ) THEN
        CALL ADI2_FITERP( FSTAT, STATUS )
      END IF

*  Free the buffer
      CALL ADI_ERASE( HID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_DEFBTB', STATUS )
      END IF

      END
