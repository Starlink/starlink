      SUBROUTINE GCB1_SAVE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     GCB1_SAVE

*  Purpose:
*     Saves Grafix Control Block to HDS file object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB1_SAVE( NARG, ARGS, OARG, STATUS )

*  Description:
*     Saves the GCB to an HDS file. At the moment just invokes the HDS
*     specific routine GCB_SAVE which should eventually be incorporated
*     into this routine.

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
*     GCB Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gcb.html

*  Keywords:
*     package:gcb, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Jul 1995 (DJA):
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
      INCLUDE 'GCB_PAR'

*  Global Variables:
      INCLUDE 'GCB_CMN'
*        G_MEMPTR = INTEGER (given)
*           Active GCB data area
*        G_VERSION = REAL (given)
*           GCB version number

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	GCBLOC			! GRAFIX_CONTROL locator

      INTEGER 			NBYTE,NSCAL,NSTRUC
      INTEGER 			GCBPTR			! Mapped GCB data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find size required
      CALL GCB_CSIZE( NBYTE, NSCAL, NSTRUC, STATUS )

*  Locate the GCB object in the HDS file, make sure its at least NBYTES
*  long and map it
      CALL GCB1_SAVE_MAPGCB( ARGS(1), NBYTE, GCBLOC, GCBPTR, STATUS )

*  Copy semi-compressed notice board to output
      CALL GCB_SAVE_SUB( NSCAL, NSTRUC, %val(G_MEMPTR), %val(GCBPTR),
     :                                                       STATUS )

*  Release output
      CALL DAT_UNMAP( GCBLOC, STATUS )
      CALL DAT_ANNUL( GCBLOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB1_SAVE', STATUS )

      END



      SUBROUTINE GCB1_SAVE_MAPGCB( FID, NBYTE, GCBLOC, GCBPTR, STATUS )
*+
*  Name:
*     GCB1_SAVE_MAPGCB

*  Purpose:
*     Map the GCB object after making sure its at least NBYTES long

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB1_SAVE_MAPGCB( FID, NBYTE, GCBLOC, GCBPTR, STATUS )

*  Description:
*     Maps the GRAFIX_CONTROL object in an HDS file. Makes sure the GCB
*     in the file if present is NBYTE bytes long.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of HDSfile object
*     NBYTE = INTEGER (given)
*        Required length of the GCB
*     GCBLOC = CHARACTER*(DAT__SZLOC) (returned)
*        Locator to the GRAFIX_CONTROL object
*     GCBPTR = INTEGER (returned)
*        Pointer to the mapped GCB data
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
*     GCB Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gcb.html

*  Keywords:
*     package:gcb, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Jul 1995 (DJA):
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
      INCLUDE 'GCB_PAR'

*  Arguments Given:
      INTEGER			FID, NBYTE

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC)	GCBLOC
      INTEGER			GCBPTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	GLOC			! GRAFIX locator

      INTEGER			BYTES			! Existing GCB size

      LOGICAL 			OK
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate GRAFIX structure
      CALL ADI1_LOCGRAF( FID, .TRUE., GLOC, STATUS )

*  See if component already there
      CALL DAT_THERE( GLOC, 'GRAFIX_CONTROL', OK, STATUS )
      IF ( OK ) THEN
        CALL DAT_FIND( GLOC, 'GRAFIX_CONTROL', GCBLOC, STATUS )
        CALL DAT_SIZE( GCBLOC, BYTES, STATUS )

*    Enlarge if necessary
        IF ( BYTES .LT. NBYTE ) THEN
          BYTES = NBYTE
          CALL DAT_ALTER( GCBLOC, 1, NBYTE, STATUS )
        END IF
        CALL DAT_MAP( GCBLOC, '_BYTE', 'UPDATE', 1, BYTES, GCBPTR,
     :                STATUS )

*  if not then create it
      ELSE
        CALL DAT_NEW( GLOC, 'GRAFIX_CONTROL', '_BYTE', 1, NBYTE,
     :                STATUS )
        CALL DAT_FIND( GLOC, 'GRAFIX_CONTROL', GCBLOC, STATUS )
        CALL DAT_MAP( GCBLOC, '_BYTE', 'WRITE', 1, NBYTE, GCBPTR,
     :                STATUS )

      END IF

      END
