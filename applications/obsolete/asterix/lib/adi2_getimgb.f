      SUBROUTINE ADI2_GETIMGB( FID, HDU, NDIM, DIMS, DATA, STATUS )
*+
*  Name:
*     ADI2_GETIMGB

*  Purpose:
*     Get BYTE data from image extension HDU of file FID

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_GETIMGB( FID, HDU, NDIM, DIMS, DATA, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARACTER*(*) (given)
*        Name of the HDU whose dimensions are required
*     NDIM = INTEGER (given)
*        Dimensionality of DATA
*     DIMS[] = INTEGER (given)
*        The dimensions of DATA
*     DATA[] = BYTE (returned)
*        The data from the extension
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
      INCLUDE '{global_constants_file}' ! [global_constants_description]

*  Arguments Given:
      INTEGER			FID
      CHARACTER*(*)		HDU

*  Arguments Returned:
      BYTE			DATA(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			ADIMS(ADI__MXDIM)	! Actual dimensions
      INTEGER			FPIX(ADI__MXDIM)	! 1st pix to extract
      INTEGER			FSTAT			! FITSIO status code
      INTEGER			I			! Loop over dims
      INTEGER			LPIX(ADI__MXDIM)	! Last pix to extract
      INTEGER			LUN			! Logical unit of file

      LOGICAL			ANYF			! Any diff values?
      LOGICAL			PAD			! DATA requires padding?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get dimensions of extension
      CALL ADI2_ISHAPE( FID, HDU, ADI__MXDIM, ADIMS, ANDIM, STATUS )

*  If ok, and dimensionalities match
      IF ( (STATUS .EQ. SAI__OK) .AND. (ANDIM.EQ.NDIM) ) THEN

*    Limit data to be extracted to size of user array
        PAD = .FALSE.
        DO I = 1, ANDIM
          FPIX(I) = 1
          LPIX(I) = MIN( DIMS(I), ADIMS(I) )
          PAD = (PAD .OR (DIMS(I).NE.ADIMS(I)))
        END DO

*    Extract the logical unit
        CALL ADI2_GETLUN( FID, LUN, STATUS )

*    Read the data
        FSTAT = 0
        CALL FTGSVB( LUN, 1, NDIM, ADIMS, FPIX, LPIX, FPIX, 0, DATA,
     :               ANYF, FSTAT )
        IF ( FSTAT .NE. 0 ) THEN
          CALL ADI2_FITERP( FSTAT, STATUS )

*    Pad if required
        IF ( PAD ) THEN
          CALL MSG_PRNT( 'No padding yet!' )
        END IF

*  Otherwise dims don't match
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Dimensions of user array and FITS data '/
     :                                      /'do not match', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_GETIMGB', STATUS )
      END IF

      END
