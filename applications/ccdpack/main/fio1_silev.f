      SUBROUTINE CCD1_SILEV( IMIN, IMAX, LOGINT, STATUS )
*+
*  Name:
*     CCD1_SILEV

*  Purpose:
*     Sets the CCDPACK log system interaction level to fall within a
*     given range.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_SILEV( IMIN, IMAX, LOGINT, STATUS )

*  Description:
*     The routine extracts the value of the CCDPACK log system
*     interaction level from the log system common block. If the value
*     is outside the range IMIN to IMAX then the value is modified to
*     the nearest value. NOTE this routine should be used with extreme
*     caution, if a log file has been opened and the interaction level
*     is modified such that no output occurs to the log file, the log
*     file will NOT be closed unless the current value of CCD1_ILEV is
*     restored before calling CCD1_END. The current value for CCD1_ILEV
*     is returned for this purpose.

*  Arguments:
*     IMIN = INTEGER (Given)
*        Minimum value for interaction level. Must be greater than -1.
*     IMAX = INTEGER (Given)
*        Maximum value for interaction level. Must be less than 4.
*     LOGINT = INTEGER (Returned)
*        The current value for the log system interaction level (before
*        modification).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-NOV-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'FIO1_CLOG'        ! The CCDPACK log file system common
                                 ! block.
*        CCD1_LOGFD = INTEGER (Write)
*           FIO system file descriptor for log file.
*        CCD1_ILEV = INTEGER (Write)
*           Log system interaction level.
*           0 - no output from the CCDPACK logging system
*           1 - output to terminal only
*           2 - output to logfile only
*           3 - output to logfile and terminal
*        CCD1_BUFF = CHARACTER * ( MSG__SZMSG ) (Write)
*           Character buffer for output strings.

*  Arguments Given:
      INTEGER IMIN
      INTEGER IMAX

*  Arguments Returned:
      INTEGER LOGINT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IIMIN
      INTEGER IIMAX

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set LOGINT to the current value.
      LOGINT = CCD1_ILEV

*  Set the bounds to those permissable.
      IIMIN = MAX( IMIN, 0 )
      IIMAX = MIN( IMAX, 3 )

*  Change CCD1_ILEV to new value in this range.
      IF ( CCD1_ILEV .GT. IIMAX ) THEN
         CCD1_ILEV = IIMAX
      ELSE IF ( CCD1_ILEV .LT. IIMIN ) THEN
         CCD1_ILEV = IIMIN
      ELSE

*  Unmodified.
      END IF

      END
* $Id$
