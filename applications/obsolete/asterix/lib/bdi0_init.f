      SUBROUTINE BDI0_INIT( STATUS )
*+
*  Name:
*     BDI0_INIT

*  Purpose:
*     Load BDI ADI definitions

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI0_INIT( STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     STATUS = INTEGER (givend and returned)
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

*  Global Variables:
      INCLUDE 'BDI_CMN'
*       BDI_INIT = LOGICAL (returned)
*         BDI system is initialised?

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			BDI1_CHK
      EXTERNAL			BDI1_CREAT
      EXTERNAL			BDI1_MAP
      EXTERNAL			BDI1_MAPERR
      EXTERNAL			BDI1_MAPLQ
      EXTERNAL			BDI1_UNMAP
      EXTERNAL			BDI1_GET

*  Local Variables:
      INTEGER			DID			! Dummy identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If not already initialised
      IF ( .NOT. BDI_INIT ) THEN

*    Requires the data models package
        CALL ADI_REQPKG( 'dsmodels', STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemChk(_BinDS,_HDSfile,_CHAR)',
     :                   BDI1_CHK, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemCreat(_BinDS,_HDSfile,_CHAR)',
     :                   BDI1_CREAT, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemMap(_BinDS,_HDSfile,_CHAR,_CHAR,_CHAR)',
     :                   BDI1_MAP, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemUnmap(_BinDS,_HDSfile,_CHAR,_INTEGER)',
     :                   BDI1_UNMAP, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemGet(_BinDS,_HDSfile,_CHAR)',
     :                   BDI1_GET, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemMap(_BinDS,_HDSfile,"Error",_CHAR,_CHAR)',
     :                   BDI1_MAPERR, DID, STATUS )

        CALL ADI_DEFFUN(
     :    'FileItemMap(_BinDS,_HDSfile,"LogicalQuality",_CHAR,_CHAR)',
     :                   BDI1_MAPLQ, DID, STATUS )

*    Mark as initialised
        BDI_INIT = .TRUE.

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI0_INIT', STATUS )

      END
