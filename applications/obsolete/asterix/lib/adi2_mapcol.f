      SUBROUTINE ADI2_MAPCOL( HDUID, BCOL, FROW, NELEM, TYPE, MODE,
     :                        PSID, PTR, STATUS )
*+
*  Name:
*     ADI2_MAPCOL

*  Purpose:
*     Map the BINTABLE column with specified type and mode

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_MAPCOL( HDUID, BCOL, FROW, NELM, TYPE, MODE, PSID,
*                       PTR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of FITShdu object
*     BCOL = INTEGER (given)
*        Binary column number holding data
*     FROW = INTEGER (given)
*        First element to access
*     NELEM = INTEGER (given)
*        Number of elements to access
*     TYPE = CHARACTER*(*) (given)
*        Type for mapping
*     MODE = CHARACTER*(*) (given)
*        Mapping access mode
*     PSID = INTEGER (given)
*        ADI identifier to private storage area for this object
*     PTR = INTEGER (returned)
*        The mapped memory address
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Sep 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER			HDUID, BCOL, FROW, NELEM, PSID
      CHARACTER*(*)		TYPE, MODE

*  Arguments Returned:
      INTEGER			PTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			FSTAT			! FITSIO status code
      INTEGER			LUN			! Logical i/o unit
      INTEGER			NVAL			! # accessed values

      LOGICAL			ANYF			! Any null values?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Number of values should be number of elements per field element times
*  number of elements
      NVAL = NELEM

*  Map some memory workspace
      CALL DYN_MAPT( 1, NVAL, TYPE, PTR, STATUS )

*  Locate logical unit
      CALL ADI2_HDULUN( HDUID, LUN, STATUS )

*  Read column data into workspace
      FSTAT = 0
      IF ( (TYPE .EQ. 'BYTE') .OR. (TYPE .EQ. 'UBYTE') ) THEN
        CALL FTGCVB( LUN, BCOL, FROW, 1, NVAL, VAL__BADB, %VAL(PTR),
     :               ANYF, FSTAT )

      ELSE IF ( (TYPE .EQ. 'WORD') .OR. (TYPE .EQ. 'UWORD')  ) THEN
        CALL FTGCVI( LUN, BCOL, FROW, 1, NVAL, VAL__BADW, %VAL(PTR),
     :               ANYF, FSTAT )

      ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
        CALL FTGCVJ( LUN, BCOL, FROW, 1, NVAL, VAL__BADI, %VAL(PTR),
     :               ANYF, FSTAT )

      ELSE IF ( TYPE .EQ. 'REAL' ) THEN
        CALL FTGCVE( LUN, BCOL, FROW, 1, NVAL, VAL__BADR, %VAL(PTR),
     :               ANYF, FSTAT )

      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
        CALL FTGCVD( LUN, BCOL, FROW, 1, NVAL, VAL__BADD, %VAL(PTR),
     :               ANYF, FSTAT )

      ELSE IF ( TYPE .EQ. 'LOGICAL' ) THEN
        CALL FTGCL( LUN, BCOL, FROW, 1, NVAL, %VAL(PTR), FSTAT )

      END IF

*  Store the pointer and the column number in the private storage
      CALL ADI2_STOMAP( PSID, HDUID, 'BC', PTR, TYPE, MODE, STATUS )

*  Store additional stuff needed by column mapping
      CALL ADI_CPUT0I( PSID, 'Column', BCOL, STATUS )
      CALL ADI_CPUT0I( PSID, 'Frow', FROW, STATUS )
      CALL ADI_CPUT0I( PSID, 'Nelm', NVAL, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_MAPCOL', STATUS )

      END
