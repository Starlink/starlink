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
      EXTERNAL			ADI0_SETLNK

      EXTERNAL			ADI1_OPEN
      EXTERNAL			ADI1_FCLONE
      EXTERNAL			ADI1_FCLOSE
      EXTERNAL			ADI1_FCREAT
      EXTERNAL			ADI1_FOBNAM
      EXTERNAL			ADI1_FTRACE

      EXTERNAL			BDI1_SETLNK
      EXTERNAL			BDI1_UNLNK

      EXTERNAL			EDI1_SETLNK
      EXTERNAL			EDI1_UNLNK

      EXTERNAL			FSI1_NEWLNK
      EXTERNAL			FSI1_SETLNK
      EXTERNAL			FSI1_GETSEL
      EXTERNAL			FSI1_GETREF
      EXTERNAL			FSI1_PUTSEL
      EXTERNAL			FSI1_PUTREF

      EXTERNAL			GMI1_NEWLNK
      EXTERNAL			GMI1_SETLNK

      EXTERNAL			PRF1_GET
      EXTERNAL			PRF1_SET

      EXTERNAL			SSI1_NEWLNK

      EXTERNAL			UDI1_COPANC

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
      CALL ADI_DEFMTH( 'FileClose(_HDSfile)', ADI1_FCLOSE, DID,
     :                   STATUS )
      CALL ADI_DEFMTH( 'FileObjName(_HDSlocator)', ADI1_FOBNAM, DID,
     :                 STATUS )
      CALL ADI_DEFMTH( 'FileTrace(_HDSlocator)', ADI1_FTRACE, DID,
     :                 STATUS )

*  Define BDI interface
      CALL ADI_DEFMTH( 'SetLink(_BinDS,_HDSfile)', BDI1_SETLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'SetLink(_Array,_HDSfile)', BDI1_SETLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'UnLink(_BinDS,_HDSfile)', BDI1_UNLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'UnLink(_Array,_HDSfile)', BDI1_UNLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'UnLink(_Scalar,_HDSfile)', BDI1_UNLNK,
     :                 DID, STATUS )

*  Define EDI interface
      CALL ADI_DEFMTH( 'SetLink(_EventDS,_HDSfile)', EDI1_SETLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'UnLink(_EventDS,_HDSfile)', EDI1_UNLNK,
     :                 DID, STATUS )

*  Multi-graph dataset interface
      CALL ADI_DEFMTH( 'NewLink(_MultiGraph,_HDSfile)', GMI1_NEWLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'SetLink(_MultiGraph,_HDSfile)', GMI1_SETLNK,
     :                 DID, STATUS )

*  File set interface
      CALL ADI_DEFMTH( 'NewLink(_FileSet,_HDSfile)', FSI1_NEWLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'SetLink(_FileSet,_HDSfile)', FSI1_SETLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'ReadSel(_FileSet,_HDSfile,_INTEGER)',
     :                 FSI1_GETSEL, DID, STATUS )
      CALL ADI_DEFMTH( 'WriteSel(_FileSet,_HDSfile,_INTEGER,_)',
     :                 FSI1_PUTSEL, DID, STATUS )
      CALL ADI_DEFMTH( 'ReadRef(_FileSet,_HDSfile,_INTEGER,_CHAR)',
     :                 FSI1_GETREF, DID, STATUS )
      CALL ADI_DEFMTH( 'WriteRef(_FileSet,_HDSfile,_INTEGER,_)',
     :                 FSI1_PUTREF, DID, STATUS )

*  Source search results files
      CALL ADI_DEFMTH( 'NewLink(_SSDS,_HDSfile)', SSI1_NEWLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'NewLink(_SSDSset,_HDSfile)', SSI1_NEWLNK,
     :                 DID, STATUS )

*  Processing flag setting
      CALL ADI_DEFMTH( 'GetProFlag(_,_HDSfile,_CHAR)', PRF1_GET,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'SetProFlag(_,_HDSfile,_CHAR,_LOGICAL)',
     :                 PRF1_SET, DID, STATUS )

*  Ancillary copying
      CALL ADI_DEFMTH( 'CopyAncillary(_,_HDSfile,_,_HDSfile,_CHAR)',
     :                 UDI1_COPANC, DID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_INIT', STATUS )

      END
