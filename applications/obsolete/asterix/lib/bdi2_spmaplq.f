      SUBROUTINE BDI2_SPMAPLQ( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_SPMAPLQ

*  Purpose:
*     Service FileItemMap requests for the LogicalQuality field of FITSfile

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_SPMAPLQ( NARG, ARGS, OARG, STATUS )

*  Description:
*     Provides mapping for the 'Error' class member of BinDS derived
*     objects in HDS files. This member is derived from the VARIANCE
*     file object.

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

*  Local Variables:
      CHARACTER*6		MODE

      INTEGER			BCOL			! Binary table column
      INTEGER			NELM			! Number of data items
      INTEGER			PSID			! Items private store
      INTEGER			PTR			! Mapped data address
      INTEGER			QPTR			! Mapped quality array
      INTEGER			SPHDU			! SPECTRUM hdu
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      OARG = ADI__NULLID

*  Extract the arguments
      CALL ADI_GET0C( ARGS(5), MODE, STATUS )

*  Ensure objects satisfy mapping requirement
      IF ( MODE .EQ. 'READ' ) THEN

*    Locate the SPECTRUM hdu
        CALL ADI2_FNDHDU( ARGS(2), 'SPECTRUM', .FALSE., SPHDU, STATUS )

*    Find quality column
        CALL ADI2_FNDBTC( SPHDU, 'QUALITY', BCOL, STATUS )

*    Get number of data elements
        CALL BDI_GETNEL( ARGS(1), NELM, STATUS )

*    Locate the BDI private storage for the item, creating if required
        CALL BDI0_LOCPST( ARGS(1), 'LogicalQuality', .TRUE., PSID,
     :                    STATUS )

*    Found?
        IF ( BCOL .GT. 0 ) THEN

*      Map the column
          CALL ADI2_MAPCOL( ARGS(1), SPHDU, BCOL, 1, NELM, 'INTEGER',
     :                      'READ', PSID, QPTR, STATUS )

*      Create dynamic array
          CALL DYN_MAPL( 1, NELM, PTR, STATUS )

*      Convert quality to logical value
          CALL BDI2_SPMAPLQ_COP( NELM, %VAL(QPTR), %VAL(PTR), STATUS )

*      Release the column
          CALL ADI2_UNMAP( ARGS(2), PSID, 0, STATUS )

        ELSE

*      Create dynamic array
          CALL DYN_MAPL( 1, NELM, PTR, STATUS )

*      Fill it with 'good' value
          CALL ARR_INIT1L( .TRUE., NELM, %VAL(PTR), STATUS )

        END IF

*    If mapping went ok, store the pointer
        CALL ADI2_STOMAP( PSID, SPHDU, 'X', PTR, 'LOGICAL',
     :                    MODE, STATUS )

*    Release private storage
        CALL ADI_ERASE( PSID, STATUS )

*    Release the SPECTRUM hdu
        CALL ADI_ERASE( SPHDU, STATUS )

*    If mapping went ok, store the pointer in the return argument
        CALL ADI_NEWV0I( PTR, OARG, STATUS )

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'M', MODE )
        CALL ERR_REP( 'BDI2_SPMAPLQ_1', 'Mapping LogicalQuality for '/
     :                 /'update/write access is forbidden', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'BDI2_SPMAPLQ', STATUS )
      END IF

      END



      SUBROUTINE BDI2_SPMAPLQ_COP( N, QVAL, LVAL, STATUS )
*+
*  Name:
*     BDI2_SPMAPLQ_COP

*  Purpose:
*     Convert OGIP spectral quality values to logical good or bad

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_SPMAPLQ_COP( N, QVAL, LVAL, STATUS )

*  Description:
*     Provides mapping for the 'Error' class member of BinDS derived
*     objects in HDS files. This member is derived from the VARIANCE
*     file object.

*  Arguments:
*     N = INTEGER (given)
*        Number of values to copy
*     QVAL[] = INTEGER (given)
*        OGIP spectral quality values
*     LVAL[] = LOGICAL (returned)
*        Logical values, true if BVAL is zero, false otherwise
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

*  Arguments Given:
      INTEGER                   N
      INTEGER			QVAL(*)

*  Arguments Returned:
      LOGICAL			LVAL(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the logical quality
      DO I = 1, N
        LVAL(I) = (QVAL(I).EQ.0)
      END DO

      END
