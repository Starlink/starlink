      SUBROUTINE TCI_DT2MJD( DSTR, TSTR, MJD, STATUS )
*+
*  Name:
*     TCI_DT2MJD

*  Purpose:
*     Convert date and time strings to MJD

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TCI_DT2MJD( DSTR, TSTR, MJD, STATUS )

*  Description:
*     Converts strings of the form dd/mm/yy and hh:mm:ss into Modified
*     Julian Dates. The time string may be blank which case an integral
*     MJD will be returned.

*  Arguments:
*     DSTR = CHARACTER*(*) (given)
*        Date string in form dd/mm/yy
*     TSTR = CHARACTER*(*) (given)
*        Time string in form hh:mm:ss
*     MJD = DOUBLE PRECISION (returned)
*        Modified Julian Date
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
*     TCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/tci.html

*  Keywords:
*     package:tci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     8 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*)		DSTR, TSTR

*  Arguments Returned:
      DOUBLE PRECISION		MJD

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      DOUBLE PRECISION		FMJD

      INTEGER			FSTAT, JSTAT
      INTEGER			IY, IM, ID, IS, IH
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract day, month and year
      READ( DSTR, '(I2.2,1X,I2.2,1X,I2.2)', IOSTAT=FSTAT ) ID, IM, IY

*  If ok, convert to MJD
      IF ( FSTAT .EQ. 0 ) THEN

        CALL SLA_CALDJ( IY, IM, ID, MJD, JSTAT )

*    Significant time string?
        IF ( TSTR .GT. ' ' ) THEN

*      Extract day, month and year
          READ( TSTR, '(I2.2,1X,I2.2,1X,I2.2)', IOSTAT=FSTAT ) IH,
     :           IM, IS

*      If ok, convert to MJD fraction
          IF ( FSTAT .EQ. 0 ) THEN
            CALL SLA_DTF2D( IH, IM, DBLE(IS), FMJD, JSTAT )
            MJD = MJD + FMJD

          ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'TIME', TSTR )
            CALL ERR_REP( ' ', 'Error reading hour, minute or second'/
     :                          /' from time string /^TIME/', STATUS )

          END IF

        END IF

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'DATE', DSTR )
        CALL ERR_REP( ' ', 'Error reading day, month or year from'/
     :                            /' date string /^DATE/', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'TCI_DT2MJD', STATUS )

      END
