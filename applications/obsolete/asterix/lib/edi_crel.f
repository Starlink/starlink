      SUBROUTINE EDI_CREL( ID, LIST, TYPE, NDIM, DIMS, DECR,
     :                     FMIN, FMAX, QUANTUM, UNITS, LID, STATUS )
*+
*  Name:
*     EDI_CREL

*  Purpose:
*     Create a list of any type

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_CREL( ID, LIST, TYPE, NDIM, DIMS, DECR, FMIN, FMAX,
*                    QUANTUM, UNITS, LID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     LIST = CHARACTER*(*) (given)
*        Name of the new list
*     TYPE = CHARACTER*(*) (given)
*        Data type of the list data
*     NDIM = INTEGER (given)
*        Dimensionality of each list item
*     DIMS = INTEGER (given)
*        Dimensions of each list item
*     DECR = LOGICAL (given)
*        Is the list naturally decreasing in value
*     FMIN = any type (given)
*        The minimum allowable data value
*     FMAX = any type (given)
*        The maximum allowable data value
*     QUANTUM = any type (given)
*        The minimum realistic change in list value
*     UNITS = CHARACTER*(*) (given)
*        Units of the list data
*     LID = INTEGER (returned)
*        The identifier of the new list
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
*     package:edi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     17 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			ID, NDIM, DIMS(*)
      CHARACTER*(*)		LIST, TYPE, UNITS
      LOGICAL	                DECR
      BYTE			FMIN(*), FMAX(*), QUANTUM(*)

*  Arguments Returned:
      INTEGER			LID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*20		LTYPE			! Validated type
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( EDI__PKG ) ) CALL EDI0_INIT( STATUS )

*  Create new list object
      CALL ADI_NEW0( 'EventList', LID, STATUS )

*  Write its name
      CALL ADI_CPUT0C( LID, 'Name', LIST, STATUS )

*  Validate and write the type
      CALL UDI0_CHKTYP( TYPE, LTYPE, STATUS )
      CALL ADI_CPUT0C( LID, 'TYPE', LTYPE(:CHR_LEN(LTYPE)), STATUS )

*  Write the list data dimensions
      IF ( NDIM .GT. 0 ) THEN
        CALL ADI_CPUT1I( LID, 'SHAPE', NDIM, DIMS, STATUS )
      END IF

*  Write decreasing flag
      CALL ADI_CPUT0L( LID, 'Decreasing', DECR, STATUS )

*  Unless character type, write type dependent quantities
      IF ( LTYPE(1:4) .NE. 'CHAR' ) THEN
        CALL ADI_CPUT0( LID, 'Min', LTYPE, FMIN, STATUS )
        CALL ADI_CPUT0( LID, 'Max', LTYPE, FMAX, STATUS )
        CALL ADI_CPUT0( LID, 'Quantum', LTYPE, QUANTUM, STATUS )
      END IF

*  Write units
      IF ( UNITS .GT. ' ' ) THEN
        CALL ADI_CPUT0C( LID, 'Units', UNITS, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_CREL', STATUS )

      END
