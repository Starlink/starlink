      SUBROUTINE ADI1_NATRL( ID, CLASS, STATUS )
*+
*  Name:
*     ADI1_NATRL

*  Purpose:
*     Attempt to define the natural class for an HDS object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_NATRL( ID, CLASS, STATUS )

*  Description:
*     Attempts to open the named file object as an HDS file. If successful
*     the routine stores the locator on the property list of the ID object.

*  Arguments:
*     ID = INTEGER (Given)
*        ADI identifier of FileHandle object
*     CLASS = CHAR (Returned)
*        Class if derived, otherwise blank
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (JET-X,University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Jul 1994 (DJA):
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
      INTEGER			ID

*  Arguments Returned:
      CHARACTER*(*)		CLASS

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	FLOC			! Object locator
      CHARACTER*(DAT__SZTYP)	TYPE			! Object type

      INTEGER			DIMS(DAT__MXDIM)	! Object dimensions
      INTEGER			NDIM			! Object dimensionality

      LOGICAL			PRIM			! Object is primitive?
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      CLASS = ' '

*    Retrieve the locator from the property list
      CALL ADI1_GETLOC( ID, FLOC, STATUS )

*    Is it primitive?
      CALL DAT_PRIM( FLOC, PRIM, STATUS )
      IF ( PRIM ) THEN

*      Get its dimensions
        CALL DAT_SHAPE( FLOC, DAT__MXDIM, DIMS, NDIM, STATUS )

*      Scalar?
        IF ( NDIM .EQ. 0 ) THEN
          CLASS = 'Scalar'
        ELSE
          CLASS = 'Array'
        END IF

      ELSE

*      Get object type
        CALL DAT_TYPE( FLOC, TYPE, STATUS )

*      Event dataset?
        IF ( TYPE(1:4) .EQ. 'EVDS' ) THEN
          CLASS = 'EventDS'
        ELSE IF ( TYPE(1:4) .EQ. 'BINDS' ) THEN
          CLASS = 'BinDS'
        END IF

      END IF

      END
