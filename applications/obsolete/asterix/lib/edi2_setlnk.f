      SUBROUTINE EDI2_SETLNK( LHS, RHS, STATUS )
*+
*  Name:
*     EDI2_SETLNK

*  Purpose:
*     Service SetLink method for EventDS to FITSfile links

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI2_SETLNK( LHS, RHS, STATUS )

*  Description:
*     Establishes ADI file link between high level objects EventDS and
*     its derivatives, and FITSfile.

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

*  Local Variables:
      CHARACTER*80		LABEL			! Column label
      CHARACTER*40		NAME			! Column name
      CHARACTER*3		STR			! Keyword trailer string
      CHARACTER*20		TYPE			! Column type

      INTEGER			DIMS(DAT__MXDIM)	! List dimensions
      INTEGER			ILIST			! Loop over lists
      INTEGER			L			! Use length of NAME
      INTEGER			LID			! Lists object id
      INTEGER			NCOMP			! Number of components
      INTEGER			NDIG			! Digits used in STR
      INTEGER			NDIM			! List dimensionality
      INTEGER			NEVENT			! Number of records
      INTEGER			NLIST			! Number of lists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract logical unit from FITSfile
      CALL ADI1_GETLOC( RHS, LUN, STATUS )

*  Try to locate the EVENTS extension
      CALL ADI2_FNDEXT( RHS, 'EVENTS', 'BINTABLE', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Read the keywords defining the number of events and columns
      CALL ADI2_KGET0I( RHS, ' ', 'TFIELDS', .FALSE., .FALSE., NLIST,
     :                 ' ', STATUS )
      CALL ADI2_KGET0I( RHS, ' ', 'NAXIS2', .FALSE., .FALSE., NEVENT,
     :                 ' ', STATUS )

*  Get number of top level components
      DO ILIST = 1, NLIST

*    Construct numeric trailer for keywords
        CALL CHR_ITOC( ILIST, STR, NDIG )

*    Read keyword containing name and description of field
        CALL ADI2_KGET0C( RHS, ' ', 'TTYPE'//STR(:NDIG), .FALSE.,
     :                    .TRUE., NAME, LABEL, STATUS )
        L = CHR_LEN(NAME)

*    Read table item describing the type of the data stored
        CALL ADI2_KGET0C( RHS, ' ', 'TFORM'//STR(:NDIG), .FALSE.,
     :                    .FALSE., TYPE, ' ', STATUS )

*    Now the optional items

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
          IF ( LABEL .GT. ' ' ) THEN
            CALL ADI_CPUT0C( LID, 'Label', LABEL(:CHR_LEN(LABEL)),
     :                       STATUS )
          END IF
          IF ( UNITS .GT. ' ' ) THEN
            CALL ADI_CPUT0C( LID, 'Units', UNITS(:CHR_LEN(UNITS)),
     :                       STATUS )
          END IF

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
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI2_SETLNK', STATUS )

*  Invoke base method to perform linkage
      CALL ADI_CALNXT( STATUS )

      END
