      SUBROUTINE EDI1_SETLNK( LHS, RHS, STATUS )
*+
*  Name:
*     EDI1_SETLNK

*  Purpose:
*     Service SetLink method for EventDS to HDSfile links

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI1_SETLNK( LHS, RHS, STATUS )

*  Description:
*     Establishes ADI file link between high level objects Scalar, Array
*     and BinDS and the HDSfile.

*  Arguments:
*     LHS = INTEGER (given)
*        ADI identifier of high level object
*     RHS = INTEGER (given)
*        ADI identifier of low level object
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:private

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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   LHS, RHS

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CLOC			! LOC component
      CHARACTER*(DAT__SZLOC)	DLOC			! List data array
      CHARACTER*(DAT__SZLOC)	LOC			! Locator to file
      CHARACTER*(DAT__SZNAM)	NAME			! Component name
      CHARACTER*(DAT__SZTYP)	TYP			! Top level type

      INTEGER			DIMS(DAT__MXDIM)	! List dimensions
      INTEGER			ICOMP			! Loop over components
      INTEGER			L			! Use length of NAME
      INTEGER			LID			! Lists object id
      INTEGER			NCOMP			! Number of components
      INTEGER			NDIM			! List dimensionality
      INTEGER			NEVENT			! Number of records
      INTEGER			NLIST			! Number of lists

      LOGICAL			PRIM			! Is object primitive?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locator from HDSfile
      CALL ADI1_GETLOC( RHS, LOC, STATUS )

*  Object is primitive?
      CALL DAT_PRIM( LOC, PRIM, STATUS )
      IF ( PRIM ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( 'EDI1_SETLNK_1', 'Object is primitive, not '/
     :                /'an event dataset', STATUS )
        GOTO 99
      END IF

*  Get object type
      CALL DAT_TYPE( LOC, TYP, STATUS )

*  Type should be EVENT_DATASET, EVENTDS or EVDS. Issue warning and continue
      IF ( .NOT. CHR_INSET( 'EVENTDS,EVDS,EVENT_DATASET', TYPE ) ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', TYP )
        CALL ERR_REP( 'EDI1_SETLNK_2', 'Object is of type ^T, '/
     :      /'expecting an event dataset - will continue', STATUS )
        CALL ERR_FLUSH( STATUS )
      END IF

*  Initialise counters
      NLIST = 0
      NEVENT = 0

*  Get number of top level components
      CALL DAT_NCOMP( LOC, NCOMP, STATUS )
      DO ICOMP = 1, NCOMP

*    Index the ICOMP'th sub-component
        CALL DAT_INDEX( LOC, ICOMP, CLOC, STATUS )

*    Get its type, which must be LIST
        CALL DAT_TYPE( CLOC, TYP, STATUS )
        IF ( TYP .EQ. 'LIST' ) THEN

*      Increment list counter
          NLIST = NLIST + 1

*      Extract the list name
          CALL DAT_NAME( CLOC, NAME, STATUS )
          L = CHR_LEN(NAME)

*      Locate data array for the list
          CALL DAT_FIND( CLOC, 'DATA_ARRAY', DLOC, STATUS )

*      Get its shape
          CALL DAT_SHAPE( DLOC, DAT__MXDIM, DIMS, NDIM, STATUS )
          IF ( NLIST .EQ. 1 ) THEN
            NEVENT = DIMS(NDIM)
          ELSE IF ( DIMS(NDIM) .NE. NEVENT ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'L', NAME )
            CALL ERR_REP( ' ', 'List ^L length is inconsistent '/
     :                    /'with first list in dataset - probable'/
     :                         /' corruption of dataset', STATUS )
            GOTO 99
          END IF

*      Create new list descriptor
          CALL ADI_NEW0( 'EventList', LID, STATUS )

*      Write list name
          CALL ADI_CPUT0C( LID, 'Name', NAME(:L), STATUS )

*      Write shape data
          IF ( NDIM .GT. 1 ) THEN
            CALL ADI_CPUT1I( LID, 'SHAPE', NDIM-1, DIMS, STATUS )
          END IF

*      Get type and write
          CALL DAT_TYPE( DLOC, TYP, STATUS )
          CALL ADI_CPUT0C( LID, 'TYPE', TYP(2:CHR_LEN(TYP)), STATUS )

*      Release array locator
          CALL DAT_ANNUL( DLOC, STATUS )

*      Conditional copy of other stuff
          CALL ADI1_CCH2AL( CLOC, 'DECREASING',
     :                      LID, 'Decreasing', STATUS )
          CALL ADI1_CCH2AT( CLOC, 'FIELD_MIN', LID, 'Min', STATUS )
          CALL ADI1_CCH2AT( CLOC, 'FIELD_MAX', LID, 'Max', STATUS )
          CALL ADI1_CCH2AT( CLOC, 'QUANTUM', LID, 'Quantum', STATUS )
          CALL ADI1_CCH2AC( CLOC, 'LABEL', LID, 'Label', STATUS )
          CALL ADI1_CCH2AC( CLOC, 'UNITS', LID, 'Units', STATUS )

*      Update list description
          CALL EDI0_UPDLD( LHS, LID, STATUS )

        END IF

*    Free the component
        CALL DAT_ANNUL( CLOC, STATUS )

*  Next component
      END DO

*  Dataset title
      CALL ADI1_CCH2AC( LOC, 'TITLE', LHS, 'Title', STATUS )

*  Write class members
      CALL ADI_CPUT0I( LHS, 'NEVENT', NEVENT, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI1_SETLNK', STATUS )

*  Invoke base method to perform linkage
      CALL ADI_CALNXT( STATUS )

      END
