      SUBROUTINE ADI2_INIT( STATUS )
*+
*  Name:
*     ADI2_INIT

*  Purpose:
*     Load ADI definitions required for use of FITS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_INIT( STATUS )

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
*      1 May 1996 (DJA):
*        Added PRF2_GET to service generic processing flag requests
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
      EXTERNAL        		ADI2_OPEN
      EXTERNAL        		ADI2_FCREAT
      EXTERNAL        		ADI2_FCLONE
      EXTERNAL        		ADI2_FCLOSE
      EXTERNAL        		ADI2_FCOMIT
      EXTERNAL        		ADI2_FOBNAM
      EXTERNAL        		ADI2_FTRACE
      EXTERNAL        		ADI2_NEWLNK_ARR

      EXTERNAL        		BDI2_SETLNK
      EXTERNAL        		BDI2_UNLNK

      EXTERNAL        		EDI2_SETLNK

      EXTERNAL			PRF2_GET

*  Local Variables:
      INTEGER			DID			! Dummy id (ignored)
      INTEGER			RID			! Representation id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Load the FITS package
      CALL ADI_REQPKG( 'fits', STATUS )

*  Locate the FITS file representation object
      CALL ADI_LOCREP( 'FITS', RID, STATUS )

      CALL ADI_DEFRCB( RID, 'CreatRtn', ADI2_FCREAT, STATUS )
      CALL ADI_DEFRCB( RID, 'OpenRtn', ADI2_OPEN, STATUS )

*  File system methods
      CALL ADI_DEFMTH( 'FileClose(_FITSfile)', ADI2_FCLOSE, DID,
     :                   STATUS )
      CALL ADI_DEFMTH( 'FileClone(_FITSfile,_CHAR)', ADI2_FCLONE, DID,
     :                 STATUS )
      CALL ADI_DEFMTH( 'FileCommit(_FITSfile)', ADI2_FCOMIT, DID,
     :                   STATUS )
      CALL ADI_DEFMTH( 'FileObjName(_FITSfile)', ADI2_FOBNAM, DID,
     :                   STATUS )
      CALL ADI_DEFMTH( 'FileTrace(_FITSfile)', ADI2_FTRACE, DID,
     :                   STATUS )

      CALL ADI_DEFMTH( 'NewLink(_Array,_FITSfile)', ADI2_NEWLNK_ARR,
     :                   DID, STATUS )

*  Define BDI interface
      CALL ADI_DEFMTH( 'SetLink(_BinDS,_FITSfile)', BDI2_SETLNK,
     :                 DID, STATUS )

      CALL ADI_DEFMTH( 'SetLink(_Array,_FITSfile)', BDI2_SETLNK,
     :                 DID, STATUS )

      CALL ADI_DEFMTH( 'SetLink(_Scalar,_FITSfile)', BDI2_SETLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'UnLink(_BinDS,_FITSfile)', BDI2_UNLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'UnLink(_Array,_FITSfile)', BDI2_UNLNK,
     :                 DID, STATUS )
      CALL ADI_DEFMTH( 'UnLink(_Scalar,_FITSfile)', BDI2_UNLNK,
     :                 DID, STATUS )

*  Define EDI interface
      CALL ADI_DEFMTH( 'SetLink(_EventDS,_FITSfile)', EDI2_SETLNK,
     :                 DID, STATUS )

*  Processing flag methods
      CALL ADI_DEFMTH( 'GetProFlag(_,_FITSfile,_CHAR)', PRF2_GET,
     :                 DID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_INIT', STATUS )

      END
