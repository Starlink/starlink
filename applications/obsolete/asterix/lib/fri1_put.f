      SUBROUTINE FRI1_PUT( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     FRI1_PUT

*  Purpose:
*     Write file reference to an HDS object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FRI1_PUT( NARG, ARGS, OARG, STATUS )

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
*     FRI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fri.html

*  Keywords:
*     package:fri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Jul 1995 (DJA):
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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ALOC			! ASTERIX locator
      CHARACTER*(DAT__SZNAM)	ROBJ			! Reference obj name
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No return value
      OARG = ADI__NULLID

*  Locate ASTERIX box of file
      CALL ADI1_LOCAST( ARGS(1), .TRUE., ALOC, STATUS )

*  Translate the logical name into the HDS will use
      CALL FRI1_TRNSNM( ARGS(2), ROBJ, STATUS )

*  Write the reference
      CALL FRI1_PUT1( ALOC, ROBJ, ARGS(3), STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FRI1_PUT', STATUS )

      END



      SUBROUTINE FRI1_PUT1( LOC, CNAME, REFID, STATUS )
*+
*  Name:
*     FRI1_PUT1

*  Purpose:
*     Write file reference to an HDS object component

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FRI1_PUT1( LOC, CNAME, REFID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     LOC = CHARACTER*(DAT__SZLOC) (given)
*        HDS object in which component CNAME will be created
*     CNAME = CHARACTER*(*) (given)
*        Name of new component to create
*     REFID = INTEGER (given)
*        The referenced object
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
*     FRI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fri.html

*  Keywords:
*     package:fri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Jul 1995 (DJA):
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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      CHARACTER*(DAT__SZLOC)	LOC
      CHARACTER*(*)		CNAME
      INTEGER			REFID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ARLOC			! AST_REF object
      CHARACTER*(DAT__SZLOC)	RLOC			! Referenced HDS object
      CHARACTER*132		FILE,PATH		! ADI path info
      CHARACTER*20		TYPE			! ADI type of arg(3)

      INTEGER			FILID			! File identifier
      INTEGER			NLEV			! Path info

      LOGICAL			DER			! Object derivation ok?
      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reference exists?
      CALL DAT_THERE( LOC, CNAME, THERE, STATUS )
      IF ( THERE ) THEN
        CALL MSG_SETC( 'NAME', CNAME )
        IF ( REFID .EQ. ADI__NULLID ) THEN
          CALL MSG_PRNT( 'Deleting existing ^NAME reference...' )
        ELSE
          CALL MSG_PRNT( 'Replacing existing ^NAME reference...' )
        END IF
        CALL DAT_ERASE( LOC, CNAME, STATUS )
      END IF

*  If object is null, quit
      IF ( REFID .EQ. ADI__NULLID ) GOTO 99

*  Get type of object to write
      CALL ADI_TYPE( REFID, TYPE, STATUS )

*  Object is a character string?
      IF ( TYPE(1:4) .EQ. 'CHAR' ) THEN

*    Simply write the name to a character component
        CALL ADI1_CCA2HC( REFID, ' ', LOC, CNAME, STATUS )

*  Object is an HDS file?
      ELSE IF ( TYPE(1:7) .EQ. 'HDSfile' ) THEN

*    Extract referenced object locator
        CALL ADI1_GETLOC( REFID, RLOC, STATUS )

*    Write the object reference
        CALL REF_CRPUT( LOC, CNAME, RLOC, .FALSE., STATUS )

*  Otherwise, must be derived from FileObject
      ELSE

*    Check derived
        CALL ADI_GETFILE( REFID, FILID, STATUS )
        IF ( FILID .NE. ADI__NULLID ) THEN

*      Extract full path to object
          CALL ADI_FTRACE( REFID, NLEV, PATH, FILE, STATUS )

*      Create and locate an ADI_REF object
          CALL DAT_NEW( LOC, CNAME, 'ADI_REF', 0, 0, STATUS )
          CALL DAT_FIND( LOC, CNAME, ARLOC, STATUS )

*      Create and write the two components
          CALL DAT_NEW0C( ARLOC, 'FILE', 200, STATUS )
          CALL DAT_NEW0C( ARLOC, 'PATH', 200, STATUS )
          CALL CMP_PUT0C( ARLOC, 'FILE', FILE, STATUS )
          CALL CMP_PUT0C( ARLOC, 'PATH', PATH, STATUS )

*      Release referenced object
          CALL DAT_ANNUL( ARLOC, STATUS )

        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Cannot write object as a file '/
     :         /'reference - unrecognised object type', STATUS )

        END IF

      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FRI1_PUT1', STATUS )

      END
