      SUBROUTINE BDI0_INIT( STATUS )
*+
*  Name:
*     BDI0_INIT

*  Purpose:
*     Load BDI ADI definitions

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI0_INIT( STATUS )

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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

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

      EXTERNAL			BDI0_SCARCHK

      EXTERNAL			BDI1_CHK
      EXTERNAL			BDI1_DELETE
      EXTERNAL			BDI1_GET
      EXTERNAL			BDI1_MAP
      EXTERNAL			BDI1_PUT

      EXTERNAL			BDI0_UNMAP		! Generic methods

      EXTERNAL			BDI2_CHK
      EXTERNAL			BDI2_MAP
      EXTERNAL			BDI2_PUT

      EXTERNAL			BDI2_SCMAP

      EXTERNAL			BDI2_ARMAP

      EXTERNAL			BDI2_SPCHK
      EXTERNAL			BDI2_SPGET
      EXTERNAL			BDI2_SPMAP
      EXTERNAL			BDI2_SPMAPLQ

      EXTERNAL			BDI2_TICHK
      EXTERNAL			BDI2_TIGET
      EXTERNAL			BDI2_TIMAP

      EXTERNAL			BDI2_IMCHK
      EXTERNAL			BDI2_IMGET
      EXTERNAL			BDI2_IMMAP

*  Local Variables:
      INTEGER			DID			! Dummy identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If not already initialised
      IF ( .NOT. AST_QPKGI( BDI__PKG ) ) THEN

*    Requires the data models package
        CALL ADI_REQPKG( 'dsmodels', STATUS )

*    General file format independent methods
        CALL ADI_DEFFUN(
     :       'FileItemChk(_Scalar,_,_CHAR)',
     :                   BDI0_SCARCHK, DID, STATUS )
        CALL ADI_DEFFUN(
     :       'FileItemChk(_Array,_,_CHAR)',
     :                   BDI0_SCARCHK, DID, STATUS )

*    HDS general binned dataset interface
        CALL ADI_DEFFUN(
     :       'FileItemChk(_,_HDSfile,_CHAR)',
     :                   BDI1_CHK, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemDel(_,_HDSfile,_CHAR)',
     :                   BDI1_DELETE, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemGet(_,_HDSfile,_CHAR)',
     :                   BDI1_GET, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemMap(_,_HDSfile,_CHAR,_CHAR,_CHAR)',
     :                   BDI1_MAP, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemUnmap(_,_HDSfile,_CHAR,_INTEGER)',
     :                   BDI0_UNMAP, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemPut(_,_HDSfile,_CHAR,_)',
     :                   BDI1_PUT, DID, STATUS )

*    FITS spectral interface
        CALL ADI_DEFFUN(
     :       'FileItemChk(_Spectrum,_FITSfile,_CHAR)',
     :                   BDI2_SPCHK, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemGet(_Spectrum,_FITSfile,_CHAR)',
     :                   BDI2_SPGET, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemMap(_Spectrum,_FITSfile,_CHAR,_CHAR,_CHAR)',
     :                   BDI2_SPMAP, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemMap(_Spectrum,_FITSfile,"LogicalQuality",'/
     :                   /'_CHAR,_CHAR)', BDI2_SPMAPLQ, DID, STATUS )

*    FITS times series interface
        CALL ADI_DEFFUN(
     :       'FileItemChk(_TimeSeries,_FITSfile,_CHAR)',
     :                   BDI2_TICHK, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemGet(_TimeSeries,_FITSfile,_CHAR)',
     :                   BDI2_TIGET, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemMap(_TimeSeries,_FITSfile,_CHAR,_CHAR,_CHAR)',
     :                   BDI2_TIMAP, DID, STATUS )

*    FITS image interface
c        CALL ADI_DEFFUN(
c     :       'FileItemChk(_XYimage,_FITSfile,_CHAR)',
c     :                   BDI2_IMCHK, DID, STATUS )
c
c        CALL ADI_DEFFUN(
c     :       'FileItemGet(_XYimage,_FITSfile,_CHAR)',
c     :                   BDI2_IMGET, DID, STATUS )
c
c        CALL ADI_DEFFUN(
c     :       'FileItemMap(_XYimage,_FITSfile,_CHAR,_CHAR,_CHAR)',
c     :                   BDI2_IMMAP, DID, STATUS )

*    FITS general binned interface
        CALL ADI_DEFFUN(
     :       'FileItemChk(_BinDS,_FITSfile,_CHAR)',
     :                   BDI2_CHK, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemMap(_BinDS,_FITSfile,_CHAR,_CHAR,_CHAR)',
     :                   BDI2_MAP, DID, STATUS )

        CALL ADI_DEFFUN(
     :       'FileItemPut(_BinDS,_FITSfile,_CHAR,_)',
     :                   BDI2_PUT, DID, STATUS )

*    FITS Scalar interface
        CALL ADI_DEFFUN(
     :       'FileItemMap(_Scalar,_FITSfile,"Data",_CHAR,_CHAR)',
     :                   BDI2_SCMAP, DID, STATUS )

*    FITS Array interface
        CALL ADI_DEFFUN(
     :       'FileItemMap(_Array,_FITSfile,"Data",_CHAR,_CHAR)',
     :                   BDI2_ARMAP, DID, STATUS )

*    All FITS unmapping for BDI goes through BDI0_UNMAP
        CALL ADI_DEFFUN(
     :       'FileItemUnmap(_,_FITSfile,_CHAR,_INTEGER)',
     :                   BDI0_UNMAP, DID, STATUS )

*    Mark as initialised
        CALL AST_SPKGI( BDI__PKG )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI0_INIT', STATUS )

      END
