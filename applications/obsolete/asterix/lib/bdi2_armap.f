      SUBROUTINE BDI2_ARMAP( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_ARMAP

*  Purpose:
*     Service FileItemMap(Array,FITSfile,item,type,mode) requests

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_ARMAP( NARG, ARGS, OARG, STATUS )

*  Description:
*     Service FileItemGet(Array,FITSfile,item) requests from the BDI system

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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		ITEM			! Item to map
      CHARACTER*20		COLNAM			! Column name
      CHARACTER*6		MODE			! Mapping mode
      CHARACTER*8		TYPE			! Mapping type
      CHARACTER*72		CMNT			! (rb)

      INTEGER			BCOL			! Column number
      INTEGER			CLEN			! Column length
      INTEGER			PTR			! Mapped data
      INTEGER			PSID			! Private storage
      INTEGER			PHDU			! Primary hdu id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )
      CALL ADI_GET0C( ARGS(4), TYPE, STATUS )
      CALL ADI_GET0C( ARGS(5), MODE, STATUS )

*  Locate the primary hdu
      CALL ADI2_FNDHDU( ARGS(2), ' ', .FALSE., PHDU, STATUS )

*  Locate the BDI private storage for the item, creating if required
      CALL BDI0_LOCPST( ARGS(1), ITEM, .TRUE., PSID, STATUS )

*  Switch on the various items
      IF ( ITEM .EQ. 'Data' ) THEN

*    Locate the column
        CALL ADI_CGET0C( ARGS(2), 'SubItem', COLNAM, STATUS )

*    Find its number
        CALL ADI2_FNDBTC( PHDU, COLNAM, BCOL, STATUS )

*    Get length of array
        CALL ADI2_HGKYI( PHDU, 'NAXIS2', CLEN, CMNT, STATUS )

*    Map it
        CALL ADI2_MAPCOL( PHDU, BCOL, 1, CLEN, TYPE, MODE, PSID,
     :                    PTR, STATUS )

      END IF

*  Release storage
      CALL ADI_ERASE( PSID, STATUS )

*  Release the primary hdu
      CALL ADI_ERASE( PHDU, STATUS )

*  Store the pointer
      CALL ADI_NEWV0I( PTR, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_ARMAP', STATUS )

      END
