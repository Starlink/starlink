      SUBROUTINE BDI1_CFIND( MID, HID, ITEM, CREATE, CLOC, STATUS )
*+
*  Name:
*     BDI1_CFIND

*  Purpose:
*     Locate HDS component for a given item, creating if required

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_CFIND( MID, HID, ITEM, CREATE, CLOC, STATUS )

*  Description:
*     Locate HDS component for a given item, creating if required. If the
*     object does not exist and creation is not allowed then CLOC is set
*     to a flag value.

*  Arguments:
*     MID = INTEGER (given)
*        Model data object
*     HID = INTEGER (given)
*        HDSfile data object
*     ITEM = CHARACTER*(*) (given)
*        BDI data item
*     CREATE = LOGICAL (given)
*        Create structures if they don't exist?
*     CLOC = CHARACTER*(DAT__SZLOC) (returned)
*        Locator to object matching item. If the item does not exist
*        the CLOC is set to the symbolic value DAT__NOLOC
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
*     10 Aug 1995 (DJA):
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

*  Arguments Given:
      INTEGER			MID, HID
      CHARACTER*(*)		ITEM
      LOGICAL			CREATE

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC)	CLOC

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ALOC			! AXIS container
      CHARACTER*(DAT__SZLOC)	ELOC			! ERROR container
      CHARACTER*(DAT__SZLOC)	LOC			! Top level locator
      CHARACTER*(DAT__SZLOC)	QLOC			! QUALITY container
      CHARACTER*(DAT__SZTYP)	TYPE			! Data type

      INTEGER			DIMS(DAT__MXDIM)	! Dimensions
      INTEGER			IAX			! An axis number
      INTEGER			NDIM			! Dimensionality

      LOGICAL			ISBIND			! Binned dataset
      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      CLOC = DAT__NOLOC

*  Extract file locator
      CALL ADI1_GETLOC( HID, LOC, STATUS )

*  Get dimensions and basic type in create mode. If no values replace
*  with nulls in the hope that we can get away with it! These nulls
*  must be trapped by BDI1_CFIND1
      IF ( CREATE ) THEN
        CALL BDI_GETSHP( MID, DAT__MXDIM, DIMS, NDIM, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          NDIM = -1
        END IF
        CALL BDI_GETTYP( MID, TYPE, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          TYPE = '*unknown*'
        END IF
      END IF

*  Object is derived from BinDS?
      CALL ADI_DERVD( MID, 'BinDS', ISBIND, STATUS )

*  Top-level data array
      IF ( ITEM .EQ. 'Data' ) THEN

*    Should create structure array object depending on presence
*    of magic flag
        IF ( ISBIND ) THEN
          CALL BDI1_CFIND1( LOC, 'DATA_ARRAY', CREATE, '_'//TYPE, NDIM,
     :                      DIMS, THERE, CLOC, STATUS )
        ELSE
          CALL DAT_CLONE( LOC, CLOC, STATUS )
        END IF

*  If not structured objects all subsequent requests will fail
      ELSE IF ( .NOT. ISBIND ) THEN

*    Do nothing unless we have been told to create something which
*    we aren't allowed to do
        IF ( CREATE ) THEN
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'IT', ITEM )
          CALL ERR_REP( ' ', 'Cannot create item ^IT in non-'/
     :                          /'structured output', STATUS )
        END IF

*  Top-level variance
      ELSE IF ( ITEM .EQ. 'Variance' ) THEN

        CALL BDI1_CFIND1( LOC, 'VARIANCE', CREATE, '_'//TYPE, NDIM,
     :                    DIMS, THERE, CLOC, STATUS )

*  Axis container
      ELSE IF ( ITEM .EQ. 'Axes' ) THEN

        CALL BDI1_CFIND1( LOC, 'AXIS', CREATE, 'AXIS', 1,
     :                    NDIM, THERE, CLOC, STATUS )

*  Top-level text components whose names are the same as their HDS counterparts
      ELSE IF ( (ITEM.EQ.'Units') .OR. (ITEM.EQ.'Label') .OR.
     :          (ITEM.EQ.'Title') ) THEN

*    Does it exist? If not, and we can create it, do so
        CALL DAT_THERE( LOC, ITEM, THERE, STATUS )
        IF ( CREATE .AND. .NOT. THERE ) THEN
          IF ( ITEM .EQ. 'Units' ) THEN
            CALL DAT_NEW0C( LOC, ITEM, 40, STATUS )
          ELSE
            CALL DAT_NEW0C( LOC, ITEM, 80, STATUS )
          END IF
          THERE = (STATUS.EQ.SAI__OK)
        END IF

*    If it now exists, locate it
        IF ( THERE ) THEN
          CALL DAT_FIND( LOC, ITEM, CLOC, STATUS )
        END IF

*  Asymmetric data errors
      ELSE IF ( (ITEM .EQ. 'LoError') .OR. (ITEM .EQ. 'HiError') ) THEN

*    Look for container
        CALL BDI1_CFIND1( LOC, 'ERROR', CREATE, 'ERROR', 0, 0,
     :                    THERE, ELOC, STATUS )

*    If present, look for lower or upper component
        IF ( THERE ) THEN
          IF ( ITEM .EQ. 'LoError' ) THEN
            CALL BDI1_CFIND1( ELOC, 'LOWER', CREATE, '_'//TYPE, NDIM,
     :                        DIMS, THERE, CLOC, STATUS )
          ELSE
            CALL BDI1_CFIND1( ELOC, 'UPPER', CREATE, '_'//TYPE, NDIM,
     :                        DIMS, THERE, CLOC, STATUS )
          END IF
          CALL DAT_ANNUL( ELOC, STATUS )

        END IF

*  Axis items
      ELSE IF ( ITEM(1:2) .EQ. 'Ax' ) THEN

*    Look for top-level axis array
        CALL BDI1_CFIND1( LOC, 'AXIS', CREATE, 'AXIS', 1, NDIM,
     :                    THERE, CLOC, STATUS )

*    User is interested in a particular axis
        IF ( THERE .AND. (ITEM(1:4) .EQ. 'Axis') ) THEN

*      Get the axis number
          CALL CHR_CTOI( ITEM(6:6), IAX, STATUS )

*      Locate our particular axis
          CALL DAT_CELL( CLOC, 1, IAX, ALOC, STATUS )

*      Free the axis structure array
          CALL DAT_ANNUL( CLOC, STATUS )

*      Switch depending on the axis item required
          IF ( ITEM(8:) .EQ. 'Units' ) THEN

            CALL BDI1_CFIND1( ALOC, 'UNITS', CREATE, '_CHAR*40', 0, 0,
     :                        THERE, CLOC, STATUS )

          ELSE IF ( ITEM(8:) .EQ. 'Label' ) THEN

            CALL BDI1_CFIND1( ALOC, 'LABEL', CREATE, '_CHAR*80', 0, 0,
     :                        THERE, CLOC, STATUS )

          ELSE IF ( ITEM(8:) .EQ. 'Normalised' ) THEN

            CALL BDI1_CFIND1( ALOC, 'NORMALISED', CREATE, '_LOGICAL',
     :                        0, 0, THERE, CLOC, STATUS )

          ELSE IF ( ITEM(8:) .EQ. 'Data' ) THEN

            CALL BDI1_CFIND1( ALOC, 'DATA_ARRAY', CREATE, '_'//TYPE,
     :                        1, DIMS(IAX), THERE, CLOC, STATUS )

          ELSE IF ( ITEM(8:) .EQ. 'Width' ) THEN

            CALL BDI1_CFIND1( ALOC, 'WIDTH', CREATE, '_'//TYPE,
     :                        1, DIMS(IAX), THERE, CLOC, STATUS )

          ELSE IF ( ITEM(8:) .EQ. 'LoWidth' ) THEN

            CALL BDI1_CFIND1( ALOC, 'LWIDTH', CREATE, '_'//TYPE,
     :                        1, DIMS(IAX), THERE, CLOC, STATUS )

          ELSE IF ( ITEM(8:) .EQ. 'HiWidth' ) THEN

            CALL BDI1_CFIND1( ALOC, 'HWIDTH', CREATE, '_'//TYPE,
     :                        1, DIMS(IAX), THERE, CLOC, STATUS )

          END IF

*      Release the AXIS cell
          CALL DAT_ANNUL( ALOC, STATUS )

        END IF

*  Any of the quality associated items
      ELSE IF ( (ITEM.EQ.'Quality') .OR. (ITEM.EQ.'QualityMask') ) THEN

*    Does top level QUALITY structure exist?
        CALL BDI1_CFIND1( LOC, 'QUALITY', CREATE, 'QUALITY', 0, 0,
     :                    THERE, QLOC, STATUS )
        IF ( THERE ) THEN

*      Switch on item
*      The quality mask
          IF ( ITEM .EQ. 'QualityMask' ) THEN

*        Does bad bits mask exist?
            CALL BDI1_CFIND1( QLOC, 'BADBITS', CREATE, '_UBYTE', 0, 0,
     :                        THERE, CLOC, STATUS )

*      The quality array
          ELSE IF ( ITEM .EQ. 'Quality' ) THEN

*        Does quality array exist?
            CALL BDI1_CFIND1( QLOC, 'QUALITY', CREATE, '_UBYTE', NDIM,
     :                        DIMS, THERE, CLOC, STATUS )

          END IF

*      Release QUALITY structure
          CALL ERR_BEGIN( STATUS )
          CALL DAT_ANNUL( QLOC, STATUS )
          CALL ERR_END( STATUS )

        END IF

*  Otherwise an error
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( 'BDI1_CFIND_1', 'Unrecognised BDI data item '/
     :                /ITEM, STATUS )

      END IF

*  If bad status nullify locator
      IF ( STATUS .NE. SAI__OK ) CLOC = DAT__NOLOC

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_CFIND', STATUS )

      END


      SUBROUTINE BDI1_CFIND1( LOC, NAME, CREATE, TYPE, NDIM, DIMS,
     :                           THERE, CLOC, STATUS )
*+
*  Name:
*     BDI1_CFIND

*  Purpose:
*     Locate HDS component for a given item, creating if required

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_CFIND1( LOC, NAME, CREATE, TYPE, NDIM, DIMS, THERE,
*                       CLOC, STATUS )

*  Description:
*     Locate HDS component for a given item, creating if required. If the
*     object does not exist and creation is not allowed then CLOC is set
*     to a flag value.

*  Arguments:
*     CLOC = CHARACTER*(DAT__SZLOC) (given)
*        The HDS object containing the component we're interested in
*     NAME = CHARACTER*(*) (given)
*        The name of the component
*     CREATE = LOGICAL (given)
*        Create component if it doesn't exist?
*     TYPE = CHARACTER*(*) (given)
*        The type of the component if we have to create it
*     NDIM = INTEGER (given)
*        The dimensionality of the component if we have to create it
*     DIMS[] = INTEGER (given)
*        The dimensions of the component if we have to create it
*     THERE = LOGICAL (returned)
*        Object exists?
*     CLOC = CHARACTER*(DAT__SZLOC) (returned)
*        Locator to component, if THERE is true
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
*     10 Aug 1995 (DJA):
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

*  Arguments Given:
      CHARACTER*(DAT__SZLOC)	LOC
      CHARACTER*(*)		NAME, TYPE
      INTEGER			NDIM, DIMS(*)
      LOGICAL			CREATE

*  Arguments Returned:
      LOGICAL			THERE
      CHARACTER*(DAT__SZLOC)	CLOC

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Does the named object exist?
      CALL DAT_THERE( LOC, NAME, THERE, STATUS )
      IF ( CREATE .AND. .NOT. THERE ) THEN

*    Make sure type and dimensionality are specified
        IF ( (TYPE.EQ.'*unknown*') .OR. (NDIM.EQ.-1) ) THEN
          CALL ERR_REP( ' ', 'Insufficient type or dimensions '/
     :          /'information to create object '//NAME, STATUS )
        ELSE
          CALL DAT_NEW( LOC, NAME, TYPE, NDIM, DIMS, STATUS )
        END IF
        THERE = (STATUS.EQ.SAI__OK)
      END IF
      IF ( THERE ) THEN
        CALL DAT_FIND( LOC, NAME, CLOC, STATUS )
      END IF

      END
