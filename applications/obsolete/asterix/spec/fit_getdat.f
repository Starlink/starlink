      SUBROUTINE FIT_GETDAT( ID, GENUS, FSTAT, WORKSPACE, WEIGHTS,
     :                       NDS, OBDAT, NGOOD, SSCALE, PREDDAT,
     :                       INSTR, STATUS )
*+
*  Name:
*     FIT_GETDAT

*  Purpose:
*     Get data for fitting

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FIT_GETDAT( ID, GENUS, FSTAT, WORKSPACE, WEIGHTS, NDS, OBDAT,
*                      NGOOD, SSCALE, PREDDAT, INSTR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     {argument_name}[dimensions] = {data_type} ({argument_access_mode})
*        {argument_description}
*     STATUS = INTEGER ({status_access_mode})
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
*     FIT Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/fit.html

*  Keywords:
*     package:fit, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Mar 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'

*  Structure Declarations:
      INCLUDE 'FIT_STRUC'

*  Arguments Given:
      INTEGER			ID			! Input data (or ref)
      CHARACTER*(*)             GENUS           	! Model GENUS
      INTEGER                   FSTAT           	! Fit statistic flag
      LOGICAL                   WORKSPACE               ! Set up workspace?

*  Arguments Given and Returned:
      LOGICAL 			WEIGHTS                 ! Set up data weights?

*  Arguments Returned:
      INTEGER 			NDS                     ! No of datasets
      RECORD /DATASET/ 		OBDAT(NDSMAX)  		! Observed datasets
      INTEGER 			NGOOD                   ! No of good data elements
      INTEGER 			SSCALE                  ! Factor for scaling fitstat
      RECORD /PREDICTION/ 	PREDDAT(NDSMAX) 	! Data predicted by model
      RECORD /INSTR_RESP/ 	INSTR(NDSMAX) 		! Instrument responses

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! HDS locator

      INTEGER			I			! Loop over datasets
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get locator for dataset
      CALL ADI1_GETLOC( ID, LOC, STATUS )

*  Invoke HDS routine
      CALL FIT_DATINGET( LOC, GENUS, FSTAT, WORKSPACE, WEIGHTS,
     :                   NDS, OBDAT, NGOOD, SSCALE, PREDDAT,
     :                   INSTR, STATUS )

*  Set up ADI stuff
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Input is not a set?
        IF ( OBDAT(1).SETINDEX .EQ. 0 ) THEN
          CALL ADI_CLONE( ID, OBDAT(1).D_ID, STATUS )
        ELSE
          DO I = 1, NDS
            CALL ADI1_PUTLOC( OBDAT(I).DLOC, OBDAT(I).D_ID, STATUS )
            CALL ADI1_PUTLOC( OBDAT(I).BLOC, OBDAT(I).B_ID, STATUS )
          END DO
        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FIT_GETDAT', STATUS )

      END
