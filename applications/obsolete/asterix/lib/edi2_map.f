      SUBROUTINE EDI2_MAP( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     EDI2_MAP

*  Purpose:
*     Service ListMap requests from the EDI system for FITS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI2_MAP( NARG, ARGS, OARG, STATUS )

*  Description:
*     Services EDI map requests for FITS files.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
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
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*6		ETABLE			! Name of events HDU
      CHARACTER*20		LIST
      CHARACTER*6		MODE
      CHARACTER*7		TYPE

      INTEGER			BCOL			! Column number
      INTEGER			EVHDU			! HDU containing events
      INTEGER			LBND,UBND		! Mapping bounds
      INTEGER			LID			! List structure
      INTEGER			PTR			! Mapped data address
      INTEGER			PSID			! Private list store
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      OARG = ADI__NULLID

*  Locate the hdu containing the events
      CALL ADI_CGET0C( ARGS(2), '.Etable', ETABLE, STATUS )
      CALL ADI2_FNDHDU( ARGS(2), ETABLE, EVHDU, STATUS )

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), LIST, STATUS )
      CALL ADI_GET0C( ARGS(4), TYPE, STATUS )
      CALL ADI_GET0C( ARGS(5), MODE, STATUS )
      CALL ADI_GET0I( ARGS(6), LBND, STATUS )
      CALL ADI_GET0I( ARGS(7), UBND, STATUS )

*  Locate the list structure
      CALL EDI_IDXNAM( ARGS(1), LIST, LID, STATUS )

*  Locate the BINTABLE column
      CALL ADI2_FNDBTC( EVHDU, LIST, BCOL, STATUS )

*  Locate storage area for this column
      CALL BDI0_LOCPST( ARGS(2), LIST, .TRUE., PSID, STATUS )

*  Map the column
      IF ( LBND .EQ. 0 ) THEN
        LBND = 1
        CALL ADI2_HGKYI( EVHDU, 'NAXIS2', UBND, STATUS )
      END IF
      CALL ADI2_MAPCOL( EVHDU, BCOL, LBND, UBND-LBND+1, TYPE,
     :                  MODE, PSID, PTR, STATUS )

*  Free storage
      CALL ADI_ERASE( PSID, STATUS )

*  And the list structure
      CALL ADI_ERASE( LID, STATUS )

*  Release the HDU
      CALL ADI_ERASE( EVHDU, STATUS )

*  Store pointer in return argument
      CALL ADI_NEWV0I( PTR, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI2_MAP', STATUS )

      END
