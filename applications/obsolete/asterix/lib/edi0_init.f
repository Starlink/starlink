      SUBROUTINE EDI0_INIT( STATUS )
*+
*  Name:
*     EDI0_INIT

*  Purpose:
*     Load EDI ADI definitions

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI0_INIT( STATUS )

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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:private

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
      INCLUDE 'AST_PKG'

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI
      EXTERNAL			EDI1_ALTLEN
      EXTERNAL			EDI1_CREAT
      EXTERNAL			EDI1_LUPDT
      EXTERNAL			EDI1_MAP
      EXTERNAL			EDI1_QMAP
      EXTERNAL			EDI1_QUNMAP
      EXTERNAL			EDI1_UNMAP

      EXTERNAL			EDI2_MAP
      EXTERNAL			EDI2_UNMAP

*  Local Variables:
      INTEGER			DID			! Dummy identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If not already initialised
      IF ( .NOT. AST_QPKGI( EDI__PKG ) ) THEN

*    Requires the data models package
        CALL ADI_REQPKG( 'dsmodels', STATUS )

*    HDS interface
        CALL ADI_DEFFUN( 'ListMap(_EventDS,_HDSfile,_CHAR,'/
     :      /'_CHAR,_CHAR,_INTEGER,_INTEGER)', EDI1_MAP, DID, STATUS )
        CALL ADI_DEFFUN( 'ListMapQuantum(_EventDS,_HDSfile,_CHAR,'/
     :      /'_CHAR,_CHAR,_INTEGER,_INTEGER)', EDI1_QMAP, DID, STATUS )
        CALL ADI_DEFFUN( 'ListUnmap(_EventDS,_HDSfile,_CHAR)',
     :                               EDI1_UNMAP, DID, STATUS )
        CALL ADI_DEFFUN( 'ListUnmapQuantum(_EventDS,_HDSfile,_CHAR)',
     :                               EDI1_QUNMAP, DID, STATUS )
        CALL ADI_DEFFUN( 'ListCreate(_EventDS,_HDSfile,_EventList)',
     :                               EDI1_CREAT, DID, STATUS )
        CALL ADI_DEFFUN( 'ListAlterLength(_EventDS,_HDSfile,_INTEGER)',
     :                               EDI1_ALTLEN, DID, STATUS )
        CALL ADI_DEFFUN( 'ListModify(_EventDS,_HDSfile,_CHAR,'/
     :                     /'_EventList)',
     :                               EDI1_LUPDT, DID, STATUS )

*    FITS interface
        CALL ADI_DEFFUN( 'ListMap(_EventDS,_FITSfile,_CHAR,'/
     :      /'_CHAR,_CHAR,_INTEGER,_INTEGER)', EDI2_MAP, DID, STATUS )
        CALL ADI_DEFFUN( 'ListUnmap(_EventDS,_FITSfile,_CHAR)',
     :                               EDI2_UNMAP, DID, STATUS )

*    Mark as initialised
        CALL AST_SPKGI( EDI__PKG )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI0_INIT', STATUS )

      END
