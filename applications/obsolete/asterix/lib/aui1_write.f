      SUBROUTINE AUI1_WRITE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     AUI1_WRITE

*  Purpose:
*     Write auxilliary info to an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL AUI1_WRITE( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

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
*     AUI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/aui.html

*  Keywords:
*     package:aui, usage:private, HDS, auxilliary data, write

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Apr 1995 (DJA):
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

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	MLOC			! MORE object
      CHARACTER*40		NAME			! Attribute name
      CHARACTER*(DAT__SZLOC)	SLOC			! Structure object

      INTEGER			DPOS			! Location of '.'

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the MORE box, creating if necessary
      CALL ADI1_LOCMORE( ARGS(1), .TRUE., MLOC, STATUS )

*  Get name. If the name is structured then create the named extension in MORE
      CALL ADI_GET0C( ARGS(2), NAME, STATUS )
      DPOS = INDEX( NAME, '.' )
      IF ( DPOS .GT. 1 ) THEN

*    Does structure already exists?
        CALL DAT_THERE( MLOC, NAME(:DPOS-1), THERE, STATUS )
        IF ( .NOT. THERE ) THEN
          CALL DAT_NEW( MLOC, NAME(:DPOS-1), 'EXT', 0, 0, STATUS )
        END IF

*    Locate the structure
        CALL DAT_FIND( MLOC, NAME(:DPOS-1), SLOC, STATUS )

      ELSE
        SLOC = MLOC

      END IF

*  Write the value
      CALL ADI1_CCA2HT( ARGS(3), ' ', SLOC, NAME(DPOS+1:), STATUS )

*  Annul structure if found
      IF ( DPOS .GT. 0 ) THEN
        CALL DAT_ANNUL( SLOC, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'AUI1_WRITE', STATUS )

      END
