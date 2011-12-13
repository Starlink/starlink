      REAL FUNCTION KPG1_SEED( STATUS )
*+
*  Name:
*     KPG1_SEED

*  Purpose:
*     Obtains a semi-random seed for random-number generation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_SEED( STATUS )

*  Description:
*     This function uses the computer time from an arbitrary date, plus
*     the current process id., to generate a non-repeatable seed.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     KPG1_SEED = REAL
*        The non-repeatable seed for use in SLA_RANDOM.  Note that
*        it is not necessarily in the range 0 to 1.

*  [optional_function_items]...
*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 March 17 (MJC):
*        Original version.
*     8-JUL-1999 (DSB):
*        Include process ID in initial seed, as well as the time.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NORM               ! Normalisation factor to modulate
                                 ! the time ticks
      PARAMETER ( NORM = 86400 )

*  External References:
      REAL SLA_RANDOM

*  Local Variables:
      INTEGER NTICKS             ! Number of computer clock ticks
      INTEGER PID                ! Process id.
      LOGICAL DONE               ! Has seed been set before?
      REAL SEED                  ! The returned seed

*  Ensure DONE is set false on the first call to this routine for
*  each process. Ensure the flag and seed values are retained for
*  future invocations.
      DATA DONE /.FALSE./
      SAVE DONE, SEED
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the seed has already been set, just update the previous seed by a
*  random amount.
      IF( DONE ) THEN
         SEED = 2*SEED*SLA_RANDOM( SEED )

*  If this is the first time the seed has been set in this process...
      ELSE

*  Obtain the computer time since an arbitrary epoch, in seconds.
         CALL PSX_TIME( NTICKS, STATUS )

*  Obtain the process id as an integer.
         CALL PSX_GETPID( PID, STATUS )

*  Generate the seed as an odd number.  The process id is included
*  because the ticks are in seconds.  On modern machines the
*  application may be called several times each second, and so the same
*  seed would be used if it depended on NTICKS alone.  Including the PID
*  causes different seeds to be used each time because the PIDs will be
*  different.  If the application is run repeatedly in the same process
*  (e.g. ICL), then the seed will be set in the above (DONE=.TRUE.)
*  block.
         SEED = REAL( ( MOD( NTICKS, NORM ) + 100*PID )* 2 + 1 )

*  Indicate the seed has been set.
         DONE = .TRUE.

      END IF

*  Return the seed value
      KPG1_SEED = SEED

      END
