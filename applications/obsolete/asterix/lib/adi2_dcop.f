      SUBROUTINE ADI2_DCOP( VALUE, CACHEID, STATUS )
*+
*  Name:
*     ADI2_DCOP

*  Purpose:
*     Copy data into cache object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_DCOP( VALUE, CACHEID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     HDUID = INTEGER (given)
*        HDU to add key to
*     KEY = CHARACTER*(*) (given)
*        Name of keyword. Prefix with '@' to inhibit HDU modification flag
*        being set true
*     VALUE = INTEGER (given)
*        ADI identifier of value
*     CMNT = CHARACTER*(*) (given)
*        Comment. Use '~' for standard comment
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
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			VALUE, CACHEID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		CTYPE			! Cache type
      CHARACTER*8		TYPE			! Data type

      INTEGER			ENDIM, EDIMS(ADI__MXDIM)! Expected object shape
      INTEGER			IDIM			! Loop over dimensions
      INTEGER			INPTR, OUTPTR		! Data pointers
      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Object shape
      INTEGER			NELM			! # elements
      INTEGER			VID			! Cache value id

      LOGICAL			MISMATCH		! Shape mismatch?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Array value
      CALL ADI_SHAPE( VALUE, ADI__MXDIM, DIMS, NDIM, STATUS )

*  Cache type
      CALL ADI_TYPE( CACHEID, CTYPE, STATUS )

*  Array?
      IF ( (CTYPE .EQ. 'FITSimgCache') .AND. (NDIM.GT.0) ) THEN

*    Get expected shape
        CALL ADI_CGET1I( CACHEID, 'SHAPE', ADI__MXDIM, EDIMS,
     :                   ENDIM, STATUS )
        IF ( ENDIM .EQ. NDIM ) THEN
          MISMATCH = .FALSE.
          DO IDIM = 1, NDIM
            MISMATCH = (MISMATCH .OR. (DIMS(IDIM).NE.EDIMS(IDIM)))
          END DO
        ELSE
          MISMATCH = .TRUE.
        END IF
        IF ( MISMATCH ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Dimensions of supplied data object '/
     :                 /'do not match those expected for this file'/
     :                 /' component', STATUS )
          GOTO 99
        END IF

*    Get type
        CALL ADI_TYPE( VALUE, TYPE, STATUS )

*    Map in that type
        CALL ADI_MAP( VALUE, TYPE, 'READ', INPTR, STATUS )

*    Map output object
        CALL ADI_CMAP( CACHEID, 'Value', TYPE, 'READ', OUTPTR, STATUS )

*    Number of elements
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*    Copy data
        IF ( TYPE .EQ. 'REAL' ) THEN
          CALL ARR_COP1R( NELM, %VAL(INPTR), %VAL(OUTPTR), STATUS )
        ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
          CALL ARR_COP1D( NELM, %VAL(INPTR), %VAL(OUTPTR), STATUS )
        ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
          CALL ARR_COP1I( NELM, %VAL(INPTR), %VAL(OUTPTR), STATUS )
        ELSE IF ( TYPE .EQ. 'LOGICAL' ) THEN
          CALL ARR_COP1L( NELM, %VAL(INPTR), %VAL(OUTPTR), STATUS )
        ELSE IF ( TYPE .EQ. 'WORD' ) THEN
          CALL ARR_COP1W( NELM, %VAL(INPTR), %VAL(OUTPTR), STATUS )
        ELSE IF ( TYPE .EQ. 'BYTE' ) THEN
          CALL ARR_COP1B( NELM, %VAL(INPTR), %VAL(OUTPTR), STATUS )
        END IF

*    Unmap
        CALL ADI_UNMAP( VALUE, INPTR, STATUS )
        CALL ADI_CUNMAP( CACHEID, 'Value', OUTPTR, STATUS )

*  Keyword cache
      ELSE IF ( CTYPE .EQ. 'FITSkeyCache' ) THEN
        CALL ADI_COPY( VALUE, VID, STATUS )
        CALL ADI_CPUTID( CACHEID, 'Value', VID, STATUS )

*  Otherwise primitive
      ELSE
        CALL ADI_COPY( VALUE, CACHEID, STATUS )

      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_DCOP', STATUS )
      END IF

      END
