      SUBROUTINE BDI1_PUT( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI1_PUT

*  Purpose:
*     Service FileItemPut requests from the BDI system for HDS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_PUT( NARG, ARGS, OARG, STATUS )

*  Description:
*     Services BDI put requests for HDS files.

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
*      9 Aug 1995 (DJA):
*        Original version.
*     23 Feb 1996 (DJA):
*        Write array variant when moving data array for bad pixel flag
*     14 Mar 1996 (DJA):
*        SpacedData and ScalarWidth moved to top level
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
      CHARACTER*(DAT__SZLOC)	DLOC			! New DATA_ARRAY cmp
      CHARACTER*20		ITEM
      CHARACTER*(DAT__SZLOC)	TLOC			! Top level object

      INTEGER			IPTR			! Input data pointer
      INTEGER			ITID			! Invented data
      INTEGER			NELM			! # mapped elements
      INTEGER			NDIM, DIMS(DAT__MXDIM)	! Model dimensions
      INTEGER			PTR			! Output data pointer
      INTEGER			PSID			! Private storage
      INTEGER			WBPTR			! Write back function

      LOGICAL			STRUC			! Object is a structure
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      OARG = ADI__NULLID

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )

*  Trap the Axis_<n> item
      IF ( (ITEM(1:5) .EQ. 'Axis_') .AND. (ITEM(7:).LE.' ') ) THEN

*    Locate object to be got
        CALL BDI1_CREAT( ARGS(1), ARGS(2), ITEM, CLOC,
     :                   NDIM, DIMS, STATUS )

*    Copy components
        CALL ADI1_CCA2HC( ARGS(4), 'Label', CLOC, 'LABEL', STATUS )
        CALL ADI1_CCA2HC( ARGS(4), 'Units', CLOC, 'UNITS', STATUS )
        CALL ADI1_CCA2HL( ARGS(4), 'Normalised', CLOC,
     :                    'NORMALISED', STATUS )
        CALL ADI1_CCA2HT( ARGS(4), 'Data', CLOC, 'DATA_ARRAY', STATUS )
        CALL ADI1_CCA2HT( ARGS(4), 'Width', CLOC, 'WIDTH', STATUS )
        CALL ADI1_CCA2HT( ARGS(4), 'LoWidth', CLOC, 'LOWIDTH', STATUS )
        CALL ADI1_CCA2HT( ARGS(4), 'HiWidth', CLOC, 'HIWIDTH', STATUS )

*  Magic values flag?
      ELSE IF ( ITEM .EQ. 'MagicFlag' ) THEN

*    Locate primary data array
        CALL BDI1_CREAT( ARGS(1), ARGS(2), 'Data',
     :                   CLOC, NDIM, DIMS, STATUS )

*    Is the array a structure? If not, rename it, create a structure and
*    move the data into that new structure
        CALL DAT_STRUC( CLOC, STRUC, STATUS )
        IF ( .NOT. STRUC ) THEN

*      Create the new structure
          CALL ADI1_GETLOC( ARGS(2), TLOC, STATUS )
          CALL DAT_NEW( TLOC, '_DATA_ARRAY', 'ARRAY', 0, 0, STATUS )
          CALL DAT_FIND( TLOC, '_DATA_ARRAY', DLOC, STATUS )
          CALL DAT_MOVE( CLOC, DLOC, 'DATA', STATUS )

*      Rename
          CALL DAT_RENAM( DLOC, 'DATA_ARRAY', STATUS )
          CLOC = DLOC

        END IF

*    Write the array variant
        CALL DAT_NEW0C( DLOC, 'VARIANT', 6, STATUS )
        CALL CMP_PUT0C( DLOC, 'VARIANT', 'SIMPLE', STATUS )

*    Write the flag
        CALL ADI1_CCA2HL( ARGS(4), ' ', DLOC, 'BAD_PIXEL', STATUS )

*    Release HDS item
        CALL DAT_ANNUL( CLOC, STATUS )

*  Logical quality?
      ELSE IF ( ITEM .EQ. 'LogicalQuality' ) THEN

*    Try to invent the object
        CALL BDI1_INVNT( ARGS(1), ARGS(2), ITEM, 'LOGICAL', 'WRITE',
     :                   ITID, NDIM, DIMS, WBPTR, STATUS )

*    Successful?
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Store the object as a component of the BinDS object
          CALL BDI0_STOINV( ARGS(1), ITEM, ITID, STATUS )

*      Locate the BDI private storage for the item, creating if required
          CALL BDI0_LOCPST( ARGS(1), ITEM, .TRUE., PSID, STATUS )

*      Map the invented object
          CALL ADI_MAPL( ITID, 'WRITE', PTR, STATUS )

*      Store mapping details
          CALL BDI1_STOMAP( PSID, 'inv', DAT__NOLOC, ITID, PTR,
     :                      NDIM, DIMS, WBPTR, 'LOGICAL',
     :                      'WRITE', STATUS )

*      Copy data
          CALL ADI_MAPL( ARGS(4), 'READ', IPTR, STATUS )
          CALL ARR_SUMDIM( NDIM, DIMS, NELM )
          CALL ARR_COP1L( NELM, %VAL(IPTR), %VAL(PTR), STATUS )
          CALL ADI_UNMAP( ARGS(4), IPTR, STATUS )

*      Release storage
          CALL BDI1_UNMAP_INT( ARGS(1), ARGS(2), PSID, STATUS )

        END IF

*  All other items
      ELSE

*    Locate object to be got
        CALL BDI1_CREAT( ARGS(1), ARGS(2), ITEM, CLOC,
     :                   NDIM, DIMS, STATUS )

*    Everything ok?
        IF ( (STATUS .EQ. SAI__OK) .AND. (CLOC.NE.DAT__NOLOC) ) THEN

*      Copy from ADI to HDS
          CALL ADI1_CCA2HT( ARGS(4), ' ', CLOC, ' ', STATUS )

*      Free the HDS object
          CALL DAT_ANNUL( CLOC, STATUS )

        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_PUT', STATUS )

      END
