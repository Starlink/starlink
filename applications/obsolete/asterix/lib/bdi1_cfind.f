      SUBROUTINE BDI1_CFIND( MID, HID, ITEM, CREATE, CLOC,
     :                       CNDIM, CDIMS, STATUS )
*+
*  Name:
*     BDI1_CFIND

*  Purpose:
*     Locate HDS component for a given item, creating if required

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_CFIND( MID, HID, ITEM, CREATE, CLOC, CNDIM, CDIMS, STATUS )

*  Description:
*     Locate HDS component for a given item, creating if required. If the
*     object does not exist and creation is not allowed then CLOC is set
*     to a flag value. The routine returns the shape of the object, whether
*     or not it is created, which is defined by the NDF data model.

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
*     CNDIM = INTEGER (returned)
*        The dimensionality of the object according to the data model. Note
*        that this not necessarily the dimensionality of the actual HDS
*        component
*     CDIMS[] = INTEGER (returned)
*        The dimensions of the object according to the data model. Note
*        that these are not necessarily the dimensions of the actual HDS
*        component
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
*     18 Jan 1996 (DJA):
*        Make sure axis values/widths and error quantities are expressed
*        in max( REAL, dataset preferred type )
*     22 Feb 1996 (DJA):
*        Changes in string concatenation for Linux port
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
      INTEGER			CNDIM, CDIMS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ALOC			! AXIS container
      CHARACTER*(DAT__SZLOC)	ELOC			! ERROR container
      CHARACTER*(DAT__SZLOC)	LOC			! Top level locator
      CHARACTER*(DAT__SZLOC)	QLOC			! QUALITY container
      CHARACTER*(DAT__SZTYP)	RTYPE			! Data type for axes
							! and errors
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

*  Get model dimensions
      CALL ADI_THERE( MID, 'SHAPE', THERE, STATUS )
      IF ( THERE ) THEN
        CALL BDI_GETSHP( MID, DAT__MXDIM, DIMS, NDIM, STATUS )
      ELSE
        NDIM = -1
      END IF

*  Get dimensions and basic type in create mode. If no values replace
*  with nulls in the hope that we can get away with it! These nulls
*  must be trapped by BDI1_CFIND1
      IF ( CREATE ) THEN
        TYPE(1:1) = '_'
        CALL BDI_GETTYP( MID, TYPE(2:), STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          TYPE = '*unknown*'
        END IF

*    Derive the type for axes and errors. This must be floating point.
*    Use single precision normalling, but double if that's the maain
*    dataset type
        IF ( TYPE .EQ. 'DOUBLE' ) THEN
          RTYPE = TYPE
        ELSE
          RTYPE = '_REAL'
        END IF

      END IF

*  Object is derived from BinDS?
      CALL ADI_DERVD( MID, 'BinDS', ISBIND, STATUS )

*  Top-level data array
      IF ( ITEM .EQ. 'Data' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )

*    Should create structure array object depending on presence
*    of magic flag
        IF ( ISBIND ) THEN
          CALL BDI1_CFIND1( LOC, 'DATA_ARRAY', CREATE, TYPE,
     :                      NDIM, DIMS, THERE, CLOC, STATUS )
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

*    Define dimensions
        CALL BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )

        CALL BDI1_CFIND1( LOC, 'VARIANCE', CREATE, RTYPE, NDIM,
     :                    DIMS, THERE, CLOC, STATUS )

*  Axis container
      ELSE IF ( ITEM .EQ. 'Axes' ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( 1, NDIM, CNDIM, CDIMS )

        CALL BDI1_CFIND1( LOC, 'AXIS', CREATE, 'AXIS', 1,
     :                    NDIM, THERE, CLOC, STATUS )

*  Top-level text components whose names are the same as their HDS counterparts
      ELSE IF ( (ITEM.EQ.'Units') .OR. (ITEM.EQ.'Label') .OR.
     :          (ITEM.EQ.'Title') ) THEN

*    Define dimensions
        CALL BDI1_CFIND0( 0, 0, CNDIM, CDIMS )

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

*    Define dimensions
        CALL BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )

*    Look for container
        CALL BDI1_CFIND1( LOC, 'ERROR', CREATE, 'ERROR', 0, 0,
     :                    THERE, ELOC, STATUS )

*    If present, look for lower or upper component
        IF ( THERE ) THEN
          IF ( ITEM .EQ. 'LoError' ) THEN
            CALL BDI1_CFIND1( ELOC, 'LOWER', CREATE, RTYPE, NDIM,
     :                        DIMS, THERE, CLOC, STATUS )
          ELSE
            CALL BDI1_CFIND1( ELOC, 'UPPER', CREATE, RTYPE, NDIM,
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

*      Define default dimensions for scalars
          CALL BDI1_CFIND0( 0, 0, CNDIM, CDIMS )

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

*        Define dimensions
            CALL BDI1_CFIND0( 1, DIMS(IAX), CNDIM, CDIMS )

            CALL BDI1_CFIND1( ALOC, 'DATA_ARRAY', CREATE, RTYPE,
     :                        1, DIMS(IAX), THERE, CLOC, STATUS )

          ELSE IF ( ITEM(8:) .EQ. 'Width' ) THEN

*        Define dimensions
            CALL BDI1_CFIND0( 1, DIMS(IAX), CNDIM, CDIMS )

            CALL BDI1_CFIND1( ALOC, 'WIDTH', CREATE, RTYPE,
     :                        1, DIMS(IAX), THERE, CLOC, STATUS )

          ELSE IF ( ITEM(8:) .EQ. 'LoWidth' ) THEN

*        Define dimensions
            CALL BDI1_CFIND0( 1, DIMS(IAX), CNDIM, CDIMS )

            CALL BDI1_CFIND1( ALOC, 'LWIDTH', CREATE, RTYPE,
     :                        1, DIMS(IAX), THERE, CLOC, STATUS )

          ELSE IF ( ITEM(8:) .EQ. 'HiWidth' ) THEN

*        Define dimensions
            CALL BDI1_CFIND0( 1, DIMS(IAX), CNDIM, CDIMS )

            CALL BDI1_CFIND1( ALOC, 'HWIDTH', CREATE, RTYPE,
     :                        1, DIMS(IAX), THERE, CLOC, STATUS )

          ELSE IF ( ITEM(8:) .LE. ' ' ) THEN
            CALL DAT_CLONE( ALOC, CLOC, STATUS )

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

*        Define dimensions
            CALL BDI1_CFIND0( 0, 0, CNDIM, CDIMS )

*        Does bad bits mask exist?
            CALL BDI1_CFIND1( QLOC, 'BADBITS', CREATE, '_UBYTE', 0, 0,
     :                        THERE, CLOC, STATUS )

*      The quality array
          ELSE IF ( ITEM .EQ. 'Quality' ) THEN

*        Define dimensions
            CALL BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )

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
        CALL MSG_SETC( 'IT', ITEM )
        CALL ERR_REP( 'BDI1_CFIND_1', 'Unrecognised BDI data item '/
     :                /'^IT', STATUS )

      END IF

*  If bad status nullify locator
      IF ( STATUS .NE. SAI__OK ) CLOC = DAT__NOLOC

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_CFIND', STATUS )

      END


      SUBROUTINE BDI1_CFIND0( NDIM, DIMS, CNDIM, CDIMS )
*+
*  Name:
*     BDI1_CFIND0

*  Purpose:
*     Copy dimensions array and dimensionality

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_CFIND0(  NDIM, DIMS, CNDIM, CDIMS )

*  Description:
*     Locate HDS component for a given item, creating if required. If the
*     object does not exist and creation is not allowed then CLOC is set
*     to a flag value.

*  Arguments:
*     NDIM = INTEGER (given)
*        The dimensionality
*     DIMS[] = INTEGER (given)
*        The dimensions
*     CNDIM = INTEGER (returned)
*        Exported copy of NDIM
*     CDIMS[] = INTEGER (returned)
*        Exported copy of DIMS

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

*  Arguments Given:
      INTEGER			NDIM, DIMS(*)

*  Arguments Returned:
      INTEGER			CNDIM, CDIMS(*)

*  Local Variables:
      INTEGER			I			! Loop over dimensions
*.

*  Export dimensions
      CNDIM = NDIM
      DO I = 1, NDIM
        CDIMS(I) = DIMS(I)
      END DO

      END



      SUBROUTINE BDI1_CFIND1( LOC, NAME, CREATE, TYPE, NDIM, DIMS,
     :                        THERE, CLOC, STATUS )
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

*  Local Variables:
      INTEGER			ENDIM, EDIMS(DAT__MXDIM)! Existing dimensions
      INTEGER			I			! Loop over dimensions

      LOGICAL			DOCRE			! Create object?
      LOGICAL			SAME			! Dims the same?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      DOCRE = .FALSE.

*  Does the named object exist?
      CALL DAT_THERE( LOC, NAME, THERE, STATUS )
      IF ( CREATE .AND. .NOT. THERE ) THEN

*    Make sure type and dimensionality are specified
        IF ( (TYPE.EQ.'*unknown*') .OR. (NDIM.EQ.-1) ) THEN
          CALL MSG_SETC( 'N', NAME )
          CALL ERR_REP( ' ', 'Insufficient type or dimensions '/
     :              /'information to create object ^N', STATUS )
        ELSE
          DOCRE = .TRUE.
        END IF

*  If object exists, make sure dimensions are ok. If they are not, coerce
*  them to the correct ones
      ELSE IF ( CREATE .AND. THERE ) THEN

*    Get existing dimensions
        CALL CMP_SHAPE( LOC, NAME, DAT__MXDIM, EDIMS, ENDIM, STATUS )

*    Are they the same as those required?
        SAME = (NDIM.EQ.ENDIM)
        I = 1
        DO WHILE ( (I.LE.NDIM) .AND. SAME )
          SAME = (DIMS(I).EQ.EDIMS(I))
          I = I + 1
        END DO

*    If not the same, recreate the object
        IF ( .NOT. SAME ) THEN
          CALL DAT_ERASE( LOC, NAME, STATUS )
          DOCRE = .TRUE.
        END IF

      END IF

*  Create it?
      IF ( DOCRE ) THEN
        CALL DAT_NEW( LOC, NAME, TYPE, NDIM, DIMS, STATUS )
        THERE = (STATUS.EQ.SAI__OK)
      END IF

*  Locate if object exists
      IF ( THERE ) THEN
        CALL DAT_FIND( LOC, NAME, CLOC, STATUS )
      END IF

      END
