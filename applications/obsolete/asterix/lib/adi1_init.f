      SUBROUTINE ADI1_INIT( STATUS )
*+
*  Name:
*     ADI1_INIT

*  Purpose:
*     Load ADI definitions required for use of HDS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_INIT( STATUS )

*  Description:
*     {routine_description}

*  Arguments:
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			ADI1_OPEN
      EXTERNAL			ADI1_FCLONE
      EXTERNAL			ADI1_FCREAT
      EXTERNAL			ADI1_FTRACE

c      EXTERNAL			BDI1_SETLNK

      EXTERNAL			EDI1_SETLNK
      EXTERNAL			EDI1_UNLNK

*  Local Variables:
      INTEGER			DID			! Dummy id (ignored)
      INTEGER			RID			! Representation id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Load the HDS package
      CALL ADI_REQPKG( 'hds', STATUS )

*  Locate the HDS file representation object
      CALL ADI_LOCREP( 'HDS', RID, STATUS )

*  Define the file methods
      CALL ADI_DEFRCB( RID, 'OpenRtn', ADI1_OPEN, STATUS )
      CALL ADI_DEFRCB( RID, 'CreatRtn', ADI1_FCREAT, STATUS )
      CALL ADI_DEFMTH( 'FileClone(_HDSfile,_CHAR)', ADI1_FCLONE, DID,
     :                 STATUS )
      CALL ADI_DEFMTH( 'FileTrace(_HDSlocator)', ADI1_FTRACE, DID,
     :                 STATUS )

*  Define BDI interface
c      CALL ADI_DEFMTH( 'SetLink(_BinDS,_HDSfile)', BDI1_SETLNK,
c     :                 DID, STATUS )

*  Define EDI interface
      CALL ADI_DEFMTH( 'SetLink(_EventDS,_HDSfile)', EDI1_SETLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'UnLink(_EventDS,_HDSfile)', EDI1_UNLNK,
     :                 DID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_INIT', STATUS )

      END
