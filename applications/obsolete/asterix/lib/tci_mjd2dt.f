      SUBROUTINE TCI_MJD2DT( MJD, DSTR, TSTR, STATUS )
*+
*  Name:
*     TCI_MJD2DT

*  Purpose:
*     Write MJD to strings in forms dd/mm/yy and hh:mm:ss

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TCI_MJD2DT( MJD, DSTR, TSTR, STATUS )

*  Description:
*     Convert MJD to date and time string

*  Arguments:
*     MJD = DOUBLE PRECISION (given)
*        The date in MJD
*     DSTR = CHARACTER*(*) (returned)
*        Date in form dd/mm/yy
*     TSTR = CHARACTER*(*) (returned)
*        Time in form hh:mm:ss
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
      DOUBLE PRECISION		MJD

*  Arguments Returned:
      CHARACTER*(*)		DSTR, TSTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*1               SIGN                    ! Time sign

      DOUBLE PRECISION          FD                      ! Day fraction

      INTEGER                   IHMSF(4)                ! Bits of time
      INTEGER                   IY, IM, ID              ! Bits of date
      INTEGER                   JSTAT                   ! SLA status code
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Formats
 10   FORMAT( I2.2, '/', I2.2, '/', I2.2 )
 20   FORMAT( I2.2, ':', I2.2, ':', I2.2 )

*  Write strings
      CALL SLA_DJCAL( MJD, IY, IM, ID, FD, JSTAT )
      WRITE( DSTR, 10, IOSTAT=JSTAT ) ID, IM, IY
      CALL SLA_DD2RF( 0, FD, SIGN, IHMSF )
      WRITE( TSTR, 20, IOSTAT=JSTAT ) IHMSF(1), IHMSF(2), IHMSF(3)

      END
