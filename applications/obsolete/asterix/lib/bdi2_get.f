      SUBROUTINE BDI2_GET( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_GET

*  Purpose:
*     Service FileItemGet requests from the BDI system for FITS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_GET( NARG, ARGS, OARG, STATUS )

*  Description:
*     Services BDI get requests for FITS files. In get mode we copy the
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
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
*        Original version.
*     8 Nov 1995 (DJA):
*        Handle GET operation for whole axes
*     6 Dec 1995 (DJA):
*       Unified invention scheme
*    14 Jul 1997 (RB):
*       Add BinDSAxis scheme for axis copying
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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		ITEM			! Item to get
      CHARACTER*10		AXITEM(7)
      CHARACTER*7		AXTYPE(7)
      CHARACTER*40		CVAL

      REAL			RVAL

      INTEGER			CACHEID			! File cache object
      INTEGER			ITID			! Invented item id
      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Model object dims
      INTEGER			WBPTR			! Write back address
      INTEGER			I

      LOGICAL			OK			! Data is valid?
      LOGICAL			LVAL

*  Local data:
      DATA AXITEM		/'Label', 'Units', 'Normalised', 'Data',
     :				 'Width', 'LoWidth', 'HiWidth'/
      DATA AXTYPE		/'CHAR',  'CHAR',  'LOGICAL',    'REAL',
     :                           'REAL',  'REAL',    'REAL'/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      OARG = ADI__NULLID

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )

*  Whole axis?
      IF ( (ITEM(:5) .EQ. 'Axis_') .AND.
     :     (INDEX(ITEM(6:),'_').EQ.0) ) THEN

*    Create ADI axis description object
        CALL ADI_NEW0( 'BinDSAxis', OARG, STATUS )

*      Copy each component of the axis structure
        DO I = 1, 7
          CALL BDI2_CFIND( ARGS(1), ARGS(2), ITEM(1:6)//'_'//AXITEM(I),
     :                     .FALSE., .FALSE., CACHEID, NDIM, DIMS,
     :                     STATUS )
          IF ( CACHEID .NE. ADI__NULLID ) THEN
            CALL ADI_CNEW0( OARG, AXITEM(I), AXTYPE(I), STATUS )
            IF ( AXTYPE(I) .EQ. 'REAL' ) THEN
              CALL ADI_CGET0R( CACHEID, 'Value', RVAL, STATUS )
              CALL ADI_CPUT0R( OARG, AXITEM(I), RVAL, STATUS )
            ELSE IF ( AXTYPE(I) .EQ. 'CHAR' )  THEN
              CALL ADI_CGET0C( CACHEID, 'Value', CVAL, STATUS )
              CALL ADI_CPUT0C( OARG, AXITEM(I), CVAL, STATUS )
            ELSE IF ( AXTYPE(I) .EQ. 'LOGICAL' )  THEN
              CALL ADI_CGET0L( CACHEID, 'Value', LVAL, STATUS )
              CALL ADI_CPUT0L( OARG, AXITEM(I), LVAL, STATUS )
            END IF
          ELSE
            CALL ERR_ANNUL( STATUS )
          END IF
        END DO
      ELSE

*    Locate object to be got
        CALL BDI2_CFIND( ARGS(1), ARGS(2), ITEM, .FALSE., .FALSE.,
     :                   CACHEID, NDIM, DIMS, STATUS )

*    Everything ok?
        IF ( (STATUS .EQ. SAI__OK) .AND. (CACHEID.NE.ADI__NULLID) ) THEN

*    Just locate the data in the cache object
*    Special fix for no AUNIT keyword in ROSAT files (RB)
          CALL ADI_FIND( CACHEID, 'Value', OARG, STATUS )
          OK = (STATUS .NE. SAI__OK)

*      Free the cache object
          CALL ADI_ERASE( CACHEID, STATUS )

        ELSE

*      Try to invent data
          CALL ERR_BEGIN( STATUS )
          CALL BDI2_INVNT( ARGS(1), ARGS(2), ITEM, 'REAL', 'READ',
     :                     ITID, NDIM, DIMS, WBPTR, STATUS )
          CALL ERR_END( STATUS )

*      Invented ok?
          IF ( ITID .NE. ADI__NULLID ) THEN

*        We should store this and re-use it
            CALL ERR_ANNUL( STATUS )

*        Return to user
            OARG = ITID

          ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'IT', ITEM )
            CALL ERR_REP( ' ', 'Unable to get item ^IT data from '/
     :                    /'HDS file', STATUS )
          END IF

        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_GET', STATUS )

      END
