      SUBROUTINE HSTDSP( HIST, HRMIN, HRMAX, NUMBIN, STATUS )
*+
*  Name:
*     HSTDSP

*  Purpose:
*     Displays an histogram

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*     CALL HSTDSP( HIST, HRMIN, HRMAX, NUMBIN, STATUS )

*  Description:
*     This routine reports an histogram to the user.

*  Arguments:
*     HIST( NUMBIN ) = INTEGER( READ )
*           The array holding the histogram.
*     HRMIN = REAL( READ )
*           The minimum value of the histogram.
*     HRMAX = REAL( READ )
*           The maximum value of the histogram.
*     NUMBIN = INTEGER( READ )
*           The number of bins in the histogram.
*     STATUS = INTEGER( READ, WRITE )
*           The status value on entry to this subroutine.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     If there are insufficient bins then
*        Report error and set bad status
*     Else
*        Calculate binsize
*        Report title
*        For each histogram bin
*           Report range and number in bin
*        End do
*     Endif
*     Return

*  Type Definitions:
*     IMPLICIT NONE

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council. All
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
*     Malcolm Currie RAL ( UK.AC.RL.STAR::CUR )
*     {enter_new_authors_here}

*  History:
*     1988 Jul 7  : Original (RL.STAR::CUR).
*     {enter_further_changes_here}

*-

*  Global Constants:

      INCLUDE 'SAE_PAR'

*  Arguments Given:

      INTEGER
     :    NUMBIN,
     :    HIST( NUMBIN )

      REAL
     :    HRMIN,
     :    HRMAX

*  Status:

      INTEGER STATUS

*  Local Variables:

      REAL
     :    NBINWD,               ! Width of the report bins
     :    RANGE                 ! The difference between the maximum
                                ! and the minimum values

      INTEGER
     :    K                     ! loop counter

      CHARACTER*50 TEXT         ! summary of the histogram

*.

*    If the status is bad, then return

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check that there can be a compression

      IF ( NUMBIN .LT. 1 ) THEN

*       Report error and set a bad status

         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NUMBIN', NUMBIN )
         CALL ERR_REP( 'ERR_HSTDSP_ISFBIN',
     :     'HSTDSP: Insufficient bins (^NUMBIN) in the histogram.',
     :     STATUS )

      ELSE

*       Calculate the size of the bins for the report.

         RANGE = HRMAX - HRMIN
         NBINWD = RANGE / REAL( NUMBIN )

*       Report title

         CALL MSG_OUT( 'LINE', ' ', STATUS )
         CALL MSG_OUT( 'TITLE', '                Histogram',
     :                 STATUS )
         CALL MSG_OUT( 'LINE', ' ', STATUS )

*       Report histogram

         DO  K = 1, NUMBIN, 1

            WRITE ( TEXT, 10 ) HRMIN + ( K-1 )*NBINWD,
     :              HRMIN + K*NBINWD, HIST( K )
  10        FORMAT( ' ', 1PG15.7, ' to', 1PG15.7, ' ', I8, ' pixels' )
            CALL MSG_OUT( 'OUTLINE', TEXT, STATUS )

         END DO

         CALL MSG_OUT( 'LINE', ' ', STATUS )
      END IF

      END
