      SUBROUTINE FOLDGRID( STATUS )
*+
*  Name:
*     FOLDGRID

*  Purpose:
*     Folds a spectral grid with an energy response

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL FOLDGRID( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Folds a spectral grid with an energy response to generate a folded
*     grid. Such grids offer large speed improvements over non-folded
*     grids at the expense of lack of flexibility at the high-energy end
*     of the grid. In the unfolded grid case high energy excursions have
*     a bremsstrahlung component bolted on. This is not possible in the
*     folded grid, and so the temperature is pegged at the top of the grid.
*
*     The spectral dimension of the grid must agree with the energy
*     dimension of the response for this program to work.

*  Usage:
*     foldgrid {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        The input grid
*     RESP = CHAR (read)
*        Name of the object containing the energy response.
*     OUT = CHAR (read)
*        The output grid

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

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     foldgrid, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Mar 1996 V2.0-0 (DJA):
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
      INCLUDE 'ADI_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'FOLDGRID Version V2.0-0' )

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ILOC			! Input grid locator
      CHARACTER*(DAT__SZLOC)	ICLOC			! Input grid component
      CHARACTER*(DAT__SZNAM)	CNAME			! Grid component name
      CHARACTER*(DAT__SZLOC)	OLOC			! Output grid locator

      INTEGER			ARFID			! Part of response
      INTEGER			I			! Loop variable
      INTEGER			IDPTR			! Input grid data
      INTEGER			IFID			! Input grid
      INTEGER			NCOMP			! # grid components
      INTEGER			NDIM, GDIMS(ADI__MXDIM)	! Grid dimensions
      INTEGER			NENER			! # response energies
      INTEGER			NSPEC			! # spectra in grid
      INTEGER			OAVPTR, OAWPTR		! Output channel axis
      INTEGER			ODIMS(ADI__MXDIM)	! Output dimensions
      INTEGER			ODPTR			! Output grid data
      INTEGER			OFID			! Output grid
      INTEGER			RCPTR			! Response channel axis
      INTEGER			RMFID			! Part of response
      INTEGER			RFID			! Response object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input grid
      CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )

*  Object containing energy response
      CALL USI_ASSOC( 'RESP', 'BinDS', 'READ', RFID, STATUS )
      CALL ERI_GETIDS( RFID, 1, RMFID, ARFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Grid dimensions, hence number of spectra
      CALL BDI_GETSHP( IFID, ADI__MXDIM, GDIMS, NDIM, STATUS )
      CALL ARR_SUMDIM( NDIM-1, GDIMS(2), NSPEC )

*  Response dimensions - check against grid
      CALL ADI_CGET0I( RMFID, 'NENERGY', NENER, STATUS )
      IF ( GDIMS(1) .NE. NENER ) THEN
        CALL MSG_SETI( 'NG', GDIMS(1) )
        CALL MSG_SETI( 'NR', NENER )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Number of bins in grid spectra (^NG) '/
     :            /'does not agree with number of energy bins in '/
     :            /'response ^NR). Rebin the grid before running '/
     :            /'this program', STATUS )
        GOTO 99
      END IF

*  Number of channels, hence output dimensions
      CALL ARR_COP1I( NDIM, GDIMS, ODIMS, STATUS )
      CALL ADI_CGET0I( RMFID, 'NCHAN', ODIMS(1), STATUS )

*  Make copy of input. The output is a lot smaller so do it piecemeal
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      CALL BDI_LINK( 'BinDS', NDIM, ODIMS, 'READ', OFID, STATUS )
      CALL ADI1_GETLOC( IFID, ILOC, STATUS )
      CALL ADI1_GETLOC( OFID, OLOC, STATUS )
      CALL DAT_NCOMP( ILOC, NCOMP, STATUS )
      DO I = 1, NCOMP
        CALL DAT_INDEX( ILOC, I, ICLOC, STATUS )
        CALL DAT_NAME( ICLOC, CNAME, STATUS )
        IF ( .NOT. CHR_INSET( 'DATA_ARRAY,ENERGY_BOUNDS', CNAME ) ) THEN
          CALL DAT_COPY( ICLOC, OLOC, CNAME, STATUS )
        END IF
        CALL DAT_ANNUL( ICLOC, STATUS )
      END DO

*  Map in the input grid
      CALL MSG_PRNT( 'Rebinning grid spectra...' )
      CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )
      CALL BDI_MAPR( OFID, 'Data', 'WRITE', ODPTR, STATUS )
      CALL ERI_FOLDN( NENER, NSPEC, %VAL(IDPTR), ODIMS(1), RMFID, ARFID,
     :                %VAL(ODPTR), STATUS )
      CALL BDI_UNMAP( IFID, 'Data', IDPTR, STATUS )
      CALL BDI_UNMAP( OFID, 'Data', ODPTR, STATUS )
      CALL MSG_PRNT( 'Done' )

*  Change energy axis to channels
      CALL BDI_AXPUT0C( OFID, 1, 'Label', 'Energy', STATUS )
      CALL BDI_AXPUT0C( OFID, 1, 'Units', 'Channels', STATUS )
      CALL ADI_CMAPR( RMFID, 'Channels', 'READ', RCPTR, STATUS )
      CALL BDI_AXMAPR( OFID, 1, 'Data', 'WRITE', OAVPTR, STATUS )
      CALL BDI_AXMAPR( OFID, 1, 'Width', 'WRITE', OAWPTR, STATUS )
      CALL FOLDGRID_B2VW( ODIMS(1), %VAL(RCPTR), %VAL(OAVPTR) %VAL(OAWPTR),
     :                    STATUS )

*  Write channel bounds component too
      CALL CMP_PUT1I( OLOC, 'CHANNEL_BOUNDS', NCHAN+1, %VAL(RCPTR),
     :                STATUS )

*  Release the pointers
      CALL BDI_AXUNMAP( OFID, 1, 'Data', OAVPTR, STATUS )
      CALL BDI_AXUNMAP( OFID, 1, 'Width', OAWPTR, STATUS )
      CALL ADI_UNMAP( RMFID, 'Channels', RCPTR, STATUS )

*  Write a bit of history
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE FOLDGRID_B2VW( NCHAN, BND, VAL, WID, STATUS )
*+
*  Name:
*     FOLDGRID_B2VW

*  Purpose:
*     Convert bounds array to axis centres and widths

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FOLDGRID_B2VW( NCHAN, BND, VAL, WID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     {argument_name}[dimensions] = {data_type} ({argument_access_mode})
*        {argument_description}
*     STATUS = INTEGER ({status_access_mode})
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
*     {task_references}...

*  Keywords:
*     FOLDGRID, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Mar 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			NCHAN
      REAL			BND(*)

*  Arguments Returned:
      REAL			VAL(*), WID(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over channels
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over channel bounds
      DO I = 1, NCHAN
        VAL(I) = (BND(I) + BND(I+1))/2.0
        WID(I) = (BND(I+1)-BND(I))
      END DO

      END
