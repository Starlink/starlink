      SUBROUTINE BDI_PUTC( ID, ITEMS, NDIM, DIMS, DATA, STATUS )
*+
*  Name:
*     BDI_PUTC

*  Purpose:
*     Put the named items with CHARACTER type and dimensions

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_PUTC( ID, ITEMS, NDIM, DIMS, DATA, STATUS )

*  Description:
*     Writes the character items specified by the ITEMS string.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be mapped
*     NDIM = INTEGER (given)
*        The dimensionality of the users data buffer
*     DIMS[NDIM] = INTEGER (given)
*        The dimensions of the users buffer
*     DATA[] = CHARACTER*(*) (given)
*        The returned data
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
*     package:bdi, usage:public

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
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			ID, NDIM, DIMS(*)
      CHARACTER*(*)		ITEMS, DATA(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			C1, C2			! Character pointers
      INTEGER			DOBJ			! Temp data object
      INTEGER			CURELM			! Current o/p element
      INTEGER			IITEM			! Item counter
      INTEGER			LID			! Linked object
      INTEGER			NELM			! Max elements per item
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( BDI__PKG ) ) CALL BDI0_INIT( STATUS )

*  Second is the linked file object
      CALL ADI_GETLINK( ID, LID, STATUS )

*  Current element in output buffer
      CURELM = 1

*  Number of output data elements user has supplied per item
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*  Loop over items while more of them and status is ok
      CALL UDI0_CREITI( ITEMS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Construct ADI object holding value
        CALL ADI_NEWVC( NDIM, DIMS, DATA, DOBJ, STATUS )

*    Write the data
        CALL BDI_PUT1( ID, LID, ITEMS(C1:C2), DOBJ, STATUS )

*    Release our temporary data object
        CALL ERR_BEGIN( STATUS )
        CALL ADI_ERASE( DOBJ, STATUS )
        CALL ERR_END( STATUS )

*    Advance pointer into output buffer
        CURELM = CURELM + NELM

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_PUTC', STATUS )

      END
