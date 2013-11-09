      SUBROUTINE ADI2_CHGIMG( HDUID, NDIF, NDIM, DIMS, STATUS )
*+
*  Name:
*     ADI2_CREIMG

*  Purpose:
*     Define a new FITS extension called HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CHGIMG( HDUID, NDIM, DIMS, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     HDUID = INTEGER (given)
*        The ADI identifier of the FITShdu
*     NDIF = INTEGER (given)
*        Amount by which this extension is larger than the old extension
*     NDIM = INTEGER (given)
*        Number of dimensions of image extension
*     DIMS[NDIM] = INTEGER (given)
*        Sizes of the dimensions
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
      INTEGER			HDUID, NDIF, NDIM, DIMS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*1		STR			! Character of IAX

      INTEGER			FSTAT			! FITSIO status
      INTEGER			IAX			! Axis loop
      INTEGER			LUN			! Logical unit
      INTEGER			NELM			! New # elements
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the logical unit
      CALL ADI2_HDULUN( HDUID, LUN, STATUS )

*  If extension is being shortened fill the remaining data area with zeroes
      IF ( NDIF .LT. 0 ) THEN
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )
        FSTAT = 0
        CALL FTPPRU( LUN, 1, NELM + NDIF + 1, -NDIF, FSTAT )
      END IF

*  Update the dimension keywords
      FSTAT = 0
      CALL ADI2_HPKYI( HDUID, '@NAXIS', NDIM, ' ', STATUS )
      CALL FTMKYJ( LUN, 'NAXIS', NDIM, '&', FSTAT )
      DO IAX = 1, NDIM
        WRITE( STR, '(I1)' ) IAX
        CALL ADI2_HPKYI( HDUID, '@NAXIS'//STR, DIMS(IAX), ' ', STATUS )
        CALL FTMKYJ( LUN, 'NAXIS'//STR, DIMS(IAX), '&', FSTAT )
      END DO

*  Redefine IMAGE extension
      IF ( FSTAT .EQ. 0 ) THEN
        CALL FTRDEF( LUN, FSTAT )
      END IF
      IF ( FSTAT .NE. 0 ) CALL ADI2_FITERP( FSTAT, STATUS )

*  Mark HDU as changed
      CALL ADI_CPUT0L( HDUID, '.Changed', .TRUE., STATUS )

*  Report failure
 89   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Unable to resize image extension', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_CHGIMG', STATUS )
      END IF

      END
