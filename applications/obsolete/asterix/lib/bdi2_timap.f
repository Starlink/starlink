      SUBROUTINE BDI2_TIMAP( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_TIMAP

*  Purpose:
*     Service FileItemMap(TimeSeries,FITSfile,item,type,mode) requests

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_TIMAP( NARG, ARGS, OARG, STATUS )

*  Description:
*     Service FileItemGet(TimeSeries,FITSfile,item) requests from the BDI system

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
C      [external_declaration]
C      {data_type} {external_name} ! [external_description]

*  Local Variables:
      CHARACTER*20		ITEM
      CHARACTER*6		MODE			! Mapping mode
      CHARACTER*8		TYPE			! Mapping type
      CHARACTER*72		CMNT			! (rb)

      INTEGER			BCOL			! Binary table column
      INTEGER			DIM			! Bins in series
      INTEGER			PTR			! Mapped data
      INTEGER			PSID			! Private storage
      INTEGER			TIHDU			! RATE hdu id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )
      CALL ADI_GET0C( ARGS(4), TYPE, STATUS )
      CALL ADI_GET0C( ARGS(5), MODE, STATUS )

*  Locate the RATE hdu
      CALL ADI2_FNDHDU( ARGS(2), 'RATE', .FALSE., TIHDU, STATUS )

*  Switch on the various items
      IF ( ITEM .EQ. 'Data' ) THEN

*    Locate column holding data
        CALL ADI2_FNDBTC( TIHDU, 'RATE', BCOL, STATUS )

*  Axis data
      ELSE IF ( ITEM .EQ. 'Axis_1_Data' ) THEN

*    Map column data
        CALL ADI2_FNDBTC( TIHDU, 'TIME', BCOL, STATUS )

*  Data error
      ELSE IF ( ITEM .EQ. 'Error' ) THEN

*    Map column data
        CALL ADI2_FNDBTC( TIHDU, 'ERROR', BCOL, STATUS )

      END IF

*  Column defined?
      IF ( BCOL .GT. 0 ) THEN

*    Locate the BDI private storage for the item, creating if required
        CALL BDI0_LOCPST( ARGS(1), ITEM, .TRUE., PSID, STATUS )

*    Length of time series
        CALL ADI2_HGKYI( TIHDU, 'NAXIS2', DIM, CMNT, STATUS )

*    Map the column
        CALL ADI2_MAPCOL( ARGS(1), TIHDU, BCOL, 1, DIM, TYPE, MODE,
     :                    PSID, PTR, STATUS )

*    Release storage
        CALL ADI_ERASE( PSID, STATUS )

      END IF

*  Release the RATE hdu
      CALL ADI_ERASE( TIHDU, STATUS )

*  Store the pointer
      CALL ADI_NEWV0I( PTR, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_TIMAP', STATUS )

      END
