      SUBROUTINE ADI2_IMGTSHP( HDUID, WMRK, BITPIX, NDIM, DIMS, STATUS )
*+
*  Name:
*     ADI2_IMGTSHP

*  Purpose:
*     Derive type and shape from keywords

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_IMGTSHP( FID, WMRK, BITPIX, NDIM, DIMS, STATUS )

*  Description:
*     Commit any changes to keywords or data to the FITS file on disk. The
*     file is not closed.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier HDU cache object
*     WMRK = LOGICAL (given)
*        Mark keywords as written?
*     BITPIX = INTEGER (returned)
*        Bits per pixel
*     NDIM = INTEGER (returned)
*        Number of axes
*     DIMS[ADI__MXDIM] = INTEGER (returned)
*        Sizes of axes
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
*      2 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			HDUID
      LOGICAL			WMRK

*  Arguments Returned:
      INTEGER			BITPIX, NDIM, DIMS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*6		AXKEY			! Axis dimension key
      CHARACTER*72		CMNT			! Comment string

      INTEGER			IAX			! Loop over axes
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Gather keywords for image extension
      IF ( WMRK ) THEN
        CALL ADI2_HGKYI( HDUID, '>BITPIX', BITPIX, CMNT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          BITPIX = 8
          CALL ERR_ANNUL( STATUS )
        END IF
        CALL ADI2_HGKYI( HDUID, '>NAXIS', NDIM, CMNT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          NDIM = 0
          CALL ERR_ANNUL( STATUS )
        END IF
      ELSE
        CALL ADI2_HGKYI( HDUID, 'BITPIX', BITPIX, CMNT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          BITPIX = 8
          CALL ERR_ANNUL( STATUS )
        END IF
        CALL ADI2_HGKYI( HDUID, 'NAXIS', NDIM, CMNT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          NDIM = 0
          CALL ERR_ANNUL( STATUS )
        END IF
      END IF
      DO IAX = 1, NDIM
        WRITE( AXKEY, '(A,I1.1)' ) 'NAXIS', IAX
        IF ( WMRK ) THEN
          CALL ADI2_HGKYI( HDUID, '>'//AXKEY, DIMS(IAX), CMNT, STATUS )
        ELSE
          CALL ADI2_HGKYI( HDUID, AXKEY, DIMS(IAX), CMNT, STATUS )
        END IF
      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_IMGTSHP', STATUS )
      END IF

      END
