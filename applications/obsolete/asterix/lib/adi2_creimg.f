      SUBROUTINE ADI2_CREIMG( FID, HDU, NDIM, DIMS, TYPE, HDUID,
     :                        STATUS )
*+
*  Name:
*     ADI2_CREIMG

*  Purpose:
*     Define a new FITS extension called HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CREIMG( FID, HDU, NDIM, DIMS, TYPE, HDUID, STATUS )

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
*     HDUID = INTEGER (returned)
*        The ADI identifier of the new FITShdu
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
      INTEGER			FID, NDIM, DIMS(*)
      CHARACTER*(*)		HDU, TYPE

*  Arguments Returned:
      INTEGER			HDUID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			BITPIX			! FITS type marker
      INTEGER			FSTAT			! FITSIO status
      INTEGER			LUN			! Logical unit
      INTEGER			NHDU			! Number of HDUs
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FSTAT = 0

*  Have we found the end of this file? We found by testing the number of
*  HDU's. This is only non-zero if all the HDU's have been scanned
      CALL ADI_CGET0I( FID, 'Nhdu', NHDU, STATUS )
      IF ( NHDU .EQ. 0 ) THEN

*    Find a non-existant HDU which will force them to be counted
        CALL ADI2_FNDHDU( FID, '________', HDUID, STATUS )

        CALL ADI_CGET0I( FID, 'Nhdu', NHDU, STATUS )

      END IF
      NHDU = NHDU + 1

*  Get the logical unit
      CALL ADI_CGET0I( FID, 'Lun', LUN, STATUS )

*  Create the new HDU
      FSTAT = 0
      CALL FTCRHD( LUN, FSTAT )
      IF ( FSTAT .NE. 0 ) THEN
        CALL ADI2_FITERP( FSTAT, STATUS )
        GOTO 89
      END IF

*  Add an HDU description
      CALL ADI2_ADDHDU( FID, HDU, NHDU, 0, HDUID, STATUS )
      CALL ADI_CPUT0L( HDUID, 'Changed', .TRUE., STATUS )
      CALL ADI_CPUT0I( FID, '.CurHdu', NHDU, STATUS )
      CALL ADI_CPUT0I( FID, 'Nhdu', NHDU, STATUS )

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
      CALL FTPHPR( LUN, .TRUE., BITPIX, NDIM, DIMS, 0, 1, .TRUE.,
     :             FSTAT )
      IF ( FSTAT .EQ. 0 ) THEN

*    Mark as defined
        CALL ADI_CPUT0L( HDUID, 'DefStart', .TRUE., STATUS )
        CALL ADI_CPUT0L( HDUID, 'DefEnd', .TRUE., STATUS )

*    Write the extension name
        CALL FTPKYS( LUN, 'EXTNAME', HDU, 'Name of this extension',
     :               FSTAT )

*    Scan the keywords just written
        CALL ADI2_SCAN( HDUID, STATUS )

      ELSE
        CALL ADI2_FITERP( FSTAT, STATUS )
        GOTO 89
      END IF

*  Define its size
      CALL FTRDEF( LUN, FSTAT )
      IF ( FSTAT .NE. 0 ) THEN
        CALL ADI2_FITERP( FSTAT, STATUS )
        GOTO 89
      END IF

*  Report failure
 89   IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_SETC( 'HDU', HDU )
        CALL ERR_REP( ' ', 'Unable to create new image extensio'/
     :                /'n /^HDU/', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_CREIMG', STATUS )
      END IF

      END
