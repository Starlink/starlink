      SUBROUTINE ADI2_ISHAPE( FID, HDU, MAXDIM, DIMS, NDIM, STATUS )
*+
*  Name:
*     ADI2_ISHAPE

*  Purpose:
*     Return dimensions of named IMAGE extension

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_ISHAPE( FID, HDU, MAXDIM, DIMS, NDIM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARACTER*(*) (given)
*        Name of the HDU whose dimensions are required
*     MAXDIM = INTEGER (given)
*        Maximum number of dimensions acceptable
*     DIMS[] = INTEGER (returned)
*        The dimensions of the image extension
*     NDIM = INTEGER (returned)
*        Dimensionality of image extension
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
*     26 Jul 1995 (DJA):
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
      INTEGER			FID
      CHARACTER*(*)		HDU
      INTEGER			MAXDIM

*  Arguments Returned:
      INTEGER			DIMS(*)
      INTEGER			NDIM

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			BITPIX			! Type indicator
      INTEGER                   FSTAT                   ! FITSIO status
      INTEGER                   HID                     ! HDU identifier
      INTEGER			GCOUNT, PCOUNT		! Unused keyword values
      INTEGER                   LUN                     ! Logical unit

      LOGICAL			EXTEND			! Keyword value
      LOGICAL			SIMPLE			! Keyword value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the HDU buffer
      CALL ADI2_MOVHDU( FID, HDU, HID, STATUS )

*  Get logical unit
      CALL ADI2_GETLUN( FID, LUN, STATUS )

*  Get keyword values
      FSTAT = 0
      CALL FTGHPR( LUN, MAXDIM, SIMPLE, BITPIX, NDIM, DIMS,
     :             PCOUNT, GCOUNT, EXTEND, FSTAT )
      IF ( FSTAT .NE. 0 ) THEN
        CALL ADI2_FITERP( FSTAT, STATUS )
      END IF

*  Free the buffer
      CALL ADI_ERASE( HID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_ISHAPE', STATUS )

      END
