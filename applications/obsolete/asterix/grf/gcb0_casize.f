      SUBROUTINE GCB0_CASIZE( CACHE, NBYTE, STATUS )
*+
*  Name:
*     GCB0_CASIZE

*  Purpose:
*     Find size in bytes of a cached GCB

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB0_CASIZE( CACHE, NBYTE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     CACHE = INTEGER (given)
*        Cache address
*     NBYTE = INTEGER (returned)
*        Size in bytes of cached GCB
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
*     10 Oct 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GCB_PAR'

*  Arguments Given:
      INTEGER                   CACHE

*  Arguments Returned:
      INTEGER                   NBYTE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			DISP			!
      INTEGER			NSCAL, NSTRUC		! # cmps in GCB
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract cache address
      CALL ADI_GET0I( ARGS(1), CACHE, STATUS )

*  Find size required
      DISP = GCB__SZPTR + 1
      CALL GCB_GETI_SUB( %VAL(CACHE), DISP, GCB__SZPTR, GCB__PTRFMT,
     :                                               NSCAL, STATUS )
      DISP = DISP + GCB__SZPTR
      CALL GCB_GETI_SUB( %VAL(CACHE), DISP, GCB__SZPTR, GCB__PTRFMT,
     :                                              NSTRUC, STATUS )
      NBYTE = GCB__NHDBLK * GCB__SZPTR + NSCAL + NSTRUC

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'GCB0_CASIZE', STATUS )
      END IF

      END
