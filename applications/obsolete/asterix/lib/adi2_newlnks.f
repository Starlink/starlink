      SUBROUTINE ADI2_NEWLNK_ARR( AID, FID, STATUS )
*+
*  Name:
*     ADI2_NEWLNK_ARR

*  Purpose:
*     Link an Array object with a new FITSfile object

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADI2_NEWLNK_ARR( AID, FID, STATUS )

*  Description:
*     Provides the method to link an object derived from "Array" to an
*     object derived from "FITSfile".

*  Arguments:
*     AID = INTEGER (given)
*        ADI identifier of Array class object
*     FID = INTEGER (given)
*        ADI identifier of FITSfile class object
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
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			AID			! Array class object
      INTEGER			FID			! FITSfile class object

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*1		FKEY			! FITSIO type code
      CHARACTER*20		TYPE			! Type string

      INTEGER			BITPIX			! Bits per pixel
      INTEGER			DIMS(ADI__MXDIM)	! Array dimensions
      INTEGER			DPTR			! Ptr to data
      INTEGER			FSTAT			! FITSIO status
      INTEGER			I			! Loop over dimensions
      INTEGER			LUN			! Logical unit
      INTEGER			NDIM			! Array dimensionality
      INTEGER			NELM			! Total # elements

      LOGICAL			THERE			! Component exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get attributes from "Array" object which are needed for the mandatory
*  keywords in the FITS file
      CALL ADI_CGET0C( AID, 'TYPE', TYPE, STATUS )
      CALL ADI_CGET1I( AID, 'SHAPE', ADI__MXDIM, DIMS, NDIM, STATUS )

*  Select the value of BITPIX depending on type
      IF ( TYPE .EQ. 'INTEGER' ) THEN
        BITPIX = 32
        FKEY = 'J'
      ELSE IF ( TYPE .EQ. 'WORD' ) THEN
        BITPIX = 16
        FKEY = 'I'
      ELSE IF ( TYPE .EQ. 'BYTE' ) THEN
        BITPIX = 8
        FKEY = 'B'
      ELSE IF ( TYPE .EQ. 'REAL' ) THEN
        BITPIX = -32
        FKEY = 'E'
      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
        BITPIX = -64
        FKEY = 'D'
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'TYPE', TYPE )
        CALL ERR_REP( ' ', 'Cannot write arrays of type /^TYPE/',
     :                STATUS )

      END IF

*  Extract logical unit
      CALL ADI_CGET0I( FID, '.LUN', LUN, STATUS )

*  Write keywords
      FSTAT = 0
      CALL FTPHPR( LUN, .TRUE., BITPIX, NDIM, DIMS, 0, 1, .TRUE.,
     :             FSTAT )
      CALL FTRDEF( LUN, STATUS )
      IF ( FSTAT .NE. 0 ) THEN
        CALL ADI2_FITERP( FSTAT, STATUS )
      END IF

*  Is data defined?
      CALL ADI_THERE( AID, 'Values', THERE, STATUS )
      IF ( THERE ) THEN

*    Count total number of elements
        NELM = 1
        DO I = 1, NDIM
          NELM = NELM * DIMS(I)
        END DO

*    Map with supplied type
        CALL ADI_CMAP( AID, 'Values', TYPE, 'READ', DPTR, STATUS )

*    Write values
        IF ( FKEY .EQ. 'J' ) THEN
          CALL FTPPRJ( LUN, 1, 1, NELM, %VAL(DPTR), STATUS )
        ELSE IF ( FKEY .EQ. 'I' ) THEN
          CALL FTPPRI( LUN, 1, 1, NELM, %VAL(DPTR), STATUS )
        ELSE IF ( FKEY .EQ. 'B' ) THEN
          CALL FTPPRB( LUN, 1, 1, NELM, %VAL(DPTR), STATUS )
        ELSE IF ( FKEY .EQ. 'E' ) THEN
          CALL FTPPRE( LUN, 1, 1, NELM, %VAL(DPTR), STATUS )
        ELSE IF ( FKEY .EQ. 'D' ) THEN
          CALL FTPPRD( LUN, 1, 1, NELM, %VAL(DPTR), STATUS )
c        ELSE IF ( FKEY .EQ. 'L' ) THEN
c          CALL FTPPRL( LUN, 1, 1, NELM, %VAL(DPTR), STATUS )
        END IF

*    Unmap
        CALL ADI_CUNMAP( AID, 'Values', DPTR, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK )
     :     CALL AST_REXIT( 'ADI2_NEWLNK_ARR', STATUS )

      END
