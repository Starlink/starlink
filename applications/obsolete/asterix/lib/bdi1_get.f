      SUBROUTINE BDI1_GET( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI1_GET

*  Purpose:
*     Service FileItemGet requests from the BDI system for HDS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_GET( NARG, ARGS, OARG, STATUS )

*  Description:
*     Services BDI get requests for HDS files. In get mode we copy the
*     data from the file into a dynamic ADI object. The BDI top-level
*     makes the decision about the extraction type and other matters.

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
*     8 Nov 1995 (DJA):
*        Handle GET operation for whole axes
*     6 Dec 1995 (DJA):
*       Unified invention scheme
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CLOC			! New component
      CHARACTER*20		ITEM			! Item to get

      INTEGER			ITID			! Invented item id
      INTEGER			NDIM, DIMS(DAT__MXDIM)	! Model object dims
      INTEGER			WBPTR			! Write back address

      LOGICAL			OK			! Data is valid?
      LOGICAL			STRUC			! Object is structure
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      OARG = ADI__NULLID

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )

*  Locate object to be got
      CALL BDI1_CFIND( ARGS(1), ARGS(2), ITEM, .FALSE., CLOC,
     :                 NDIM, DIMS, STATUS )

*  Everything ok?
      IF ( (STATUS .EQ. SAI__OK) .AND. (CLOC.NE.DAT__NOLOC) ) THEN

*    Whole axis?
        IF ( (ITEM(:5) .EQ. 'Axis_') .AND.
     :       (INDEX(ITEM(6:),'_').EQ.0) ) THEN

*      Create ADI axis description object
          CALL ADI_NEW0( 'BinDSAxis', OARG, STATUS )

*      Copy each component of the axis structure
          CALL ADI1_CCH2AC( CLOC, 'LABEL', OARG, 'Label', STATUS )
          CALL ADI1_CCH2AC( CLOC, 'UNITS', OARG, 'Units', STATUS )
          CALL ADI1_CCH2AL( CLOC, 'NORMALISED', OARG,
     :                      'Normalised', STATUS )
          CALL ADI1_CCH2AT( CLOC, 'DATA_ARRAY', OARG, 'Data', STATUS )
          CALL ADI1_CCH2AT( CLOC, 'WIDTH', OARG, 'Width', STATUS )
          CALL ADI1_CCH2AT( CLOC, 'LOWIDTH', OARG, 'LoWidth', STATUS )
          CALL ADI1_CCH2AT( CLOC, 'HIWIDTH', OARG, 'HiWidth', STATUS )

        ELSE

*      Is its data valid?
          CALL DAT_STRUC( CLOC, STRUC, STATUS )
          IF ( STRUC ) THEN
            OK = .TRUE.
          ELSE
            CALL DAT_STATE( CLOC, OK, STATUS )
          END IF
          IF ( OK ) THEN

*        Copy from HDS to ADI
            CALL ADI1_CCH2AT( CLOC, ' ', OARG, ' ', STATUS )

          ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'IT', ITEM )
            CALL ERR_REP( 'BDI1_GET_1', 'Item ^IT data is invalid',
     :                    STATUS )

          END IF

*      Free the HDS object
          CALL DAT_ANNUL( CLOC, STATUS )

        END IF

      ELSE

*    Try to invent data
        CALL ERR_BEGIN( STATUS )
        CALL BDI1_INVNT( ARGS(1), ARGS(2), ITEM, 'REAL', 'READ',
     :                   ITID, NDIM, DIMS, WBPTR, STATUS )
        CALL ERR_END( STATUS )

*    Invented ok?
        IF ( ITID .NE. ADI__NULLID ) THEN

*      We should store this and re-use it

*      Return to user
          OARG = ITID

        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'IT', ITEM )
          CALL ERR_REP( ' ', 'Unable to get item ^IT data from '/
     :                  /'HDS file', STATUS )
        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_GET', STATUS )

      END
