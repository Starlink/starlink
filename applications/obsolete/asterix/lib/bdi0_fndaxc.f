      SUBROUTINE BDI0_FNDAXC( ID, QCODE, IAX, STATUS )
*+
*  Name:
*     BDI0_FNDAXC

*  Purpose:
*     Returns the axis number corresponding to the quantity code

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI0_FNDAXC( ID, QCODE, IAX, STATUS )

*  Description:
*     BDI users can refer to axes using a quantity code which has some
*     absolute physical meaning. Supported codes are described in the
*     BDI programmer guide.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     QCODE = CHARACTER*1 (given)
*        The quantity code
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
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			ID
      CHARACTER*1		QCODE

*  Arguments Returned:
      INTEGER			IAX

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Variables:
      CHARACTER*10		AXORD			! Model axis order
      CHARACTER*40		LABEL			! Axis label

      INTEGER			DIMS(ADI__MXDIM)	! Dimensions
      INTEGER			I			! Loop over axes
      INTEGER			NDIM			! Dimensionality

      LOGICAL			FOUND			! Found axis?
      LOGICAL			THERE			! Axis order string ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      IAX = 0
      FOUND = .FALSE.

*  Check known code
      IF ( INDEX( 'ETPXY', QCODE ) .GT. 0 ) THEN

*    Look for axis order string in data model
        CALL ADI_THERE( ID, 'AxisOrder', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CGET0C( ID, 'AxisOrder', AXORD, STATUS )
          IAX = INDEX( AXORD, QCODE )
          FOUND = (IAX.GT.0)

        ELSE

*      Loop over axes checking labels
          CALL BDI_GETSHP( ID, ADI__MXDIM, DIMS, NDIM, STATUS )
          I = 1
          DO WHILE ( (I.LE.NDIM) .AND. (STATUS.EQ.SAI__OK) .AND.
     :               .NOT. FOUND )
            CALL BDI_AXGET0C( ID, I, 'Label', LABEL, STATUS )
            IF ( (STATUS .EQ. SAI__OK) .AND. (LABEL.GT.' ') ) THEN
              CALL CHR_UCASE( LABEL )
               IF ( QCODE .EQ. 'X' ) THEN
                 IF ( CHR_INSET( 'X_RAW,X_CORR', LABEL ) .OR.
     :                (LABEL(1:1) .EQ. 'X') .OR.
     :                (INDEX( LABEL, 'LONGITUDE' ) .GT. 0) .OR.
     :                (INDEX( LABEL, 'ASCENSION' ) .GT. 0) .OR.
     :                (INDEX( LABEL, 'X AXIS' ) .GT. 0) ) THEN
                   FOUND = .TRUE.
                 END IF

               ELSE IF ( QCODE .EQ. 'Y' ) THEN
                 IF ( CHR_INSET( 'Y_RAW,Y_CORR', LABEL ) .OR.
     :                (LABEL(1:1) .EQ. 'Y') .OR.
     :                (INDEX( LABEL, 'LATITUDE' ) .GT. 0) .OR.
     :                (INDEX( LABEL, 'DECLINATION' ) .GT. 0) .OR.
     :                (INDEX( LABEL, 'Y AXIS' ) .GT. 0) ) THEN
                   FOUND = .TRUE.
                 END IF

               ELSE IF ( QCODE .EQ. 'T' ) THEN
                 IF ( CHR_INSET( 'RAW_TIMETAG,TIMETAG', LABEL ) .OR.
     :                (INDEX( LABEL, 'TIME' ) .GT. 0) ) THEN
                   FOUND = .TRUE.
                 END IF

               ELSE IF ( QCODE .EQ. 'E' ) THEN
                 IF ( CHR_INSET( 'CORR_PHA,PI,PHA,RAW_PHA', LABEL ) .OR.
     :                (INDEX( LABEL, 'CORRECTED PHA' ) .GT. 0) .OR.
     :                (INDEX( LABEL, 'PULSE' ) .GT. 0) .OR.
     :                (INDEX( LABEL, 'ENERGY' ) .GT. 0) ) THEN
                   FOUND = .TRUE.
                 END IF

               ELSE IF ( QCODE .EQ. 'P' ) THEN
                 FOUND = (INDEX( LABEL, 'PHASE' ) .GT. 0)

               END IF
            END IF

*        Next axis if not found
            IF ( .NOT. FOUND ) I = I + 1

          END DO
          IF ( FOUND ) IAX = I

        END IF

*    Not found?
        IF ( .NOT. FOUND ) THEN
          CALL MSG_SETC( 'C', QCODE )
          CALL BDI0_DESCID( ID, 'ID', STATUS )
          STATUS = SAI__ERROR
          CALL ERR_REP( 'BDI0_FNDAXC_1', 'Unable to locate axis '/
     :        /'specified by quantity code ^C in dataset ^ID', STATUS )
        END IF

*  Report as unknown
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'C', QCODE )
        CALL ERR_REP( 'BDI0_FNDAXC_1', 'Unrecognised axis quantity '/
     :                /'code (^C)', STATUS )

*  End of know code test
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI0_FNDAXC', STATUS )

      END
