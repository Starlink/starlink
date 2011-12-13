      PROGRAM TEST_STAT
*+
*  Name:
*     TEST_STAT

*  Purpose:
*     Test the subroutine PSX_STAT

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (JAC, UCLan)

*  History:
*     8-NOV-2007 (DSB):
*         Original version.

*-

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS

* Local Variables:
      CHARACTER*128 HOME
      CHARACTER*128 TIME
      INTEGER VALUE
      INTEGER SECS
      INTEGER MINS
      INTEGER HOURS
      INTEGER DAY
      INTEGER MONTH
      INTEGER YEAR
      INTEGER WDAY
      INTEGER YDAY
      INTEGER ISDST
      INTEGER TSTRCT
      INTEGER TLEN

* Initialize STATUS.
      STATUS = SAI__OK

* Test PSX_STAT
      PRINT *,' '
      PRINT *,'--  Program TEST_STAT, function PSX_STAT  --'
      PRINT *,' '

      CALL PSX_GETENV( 'HOME', HOME, STATUS )
      CALL PSX_STAT( HOME, 'ATIME', VALUE, STATUS )

      CALL PSX_LOCALTIME( VALUE, SECS, MINS, HOURS, DAY, MONTH, YEAR,
     :                    WDAY, YDAY, ISDST, TSTRCT, STATUS )
      CALL PSX_ASCTIME( TSTRCT, TIME, STATUS )

      TLEN = LEN( TIME )
      DO WHILE( TIME( TLEN : TLEN ) .EQ. ' ' .AND. TLEN .LT. 1 )
         TLEN = TLEN - 1
      END DO

      WRITE(*,*) 'HOME last accessed at ',TIME( : TLEN )

      END
