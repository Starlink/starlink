      PROGRAM TEST_TIME
*+
*  Name:
*     TEST_TIME

*  Purpose:
*     Test the function PSX time functions

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     17-APR-2006 (TIMJ):
*         Add prologue.

*-

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS

* Local Variables:
      INTEGER NTICKS
      INTEGER SEC, MIN, HOUR, DAY, MONTH, YEAR, WDAY, YDAY, ISDST,
     : TSTRCT
      CHARACTER * ( 27 ) STRING, STRING2

* Initialize STATUS
      STATUS = SAI__OK

      STRING = ' '
      STRING2 = ' '

      CALL EMS_BEGIN( STATUS )

* Test PSX_TIME
      PRINT *,' '
      PRINT *,'--  Program PSX_TIME, function PSX_TIME  --'
      PRINT *,' '

      CALL PSX_TIME( NTICKS, STATUS )
      PRINT *,'The value returned by PSX_TIME is ',NTICKS

* Test PSX_LOCALTIME
      PRINT *,' '
      PRINT *,'--  Program PSX_TIME, function PSX_LOCALTIME  --'
      PRINT *,' '

      CALL PSX_LOCALTIME( NTICKS, SEC, MIN, HOUR, DAY, MONTH, YEAR,
     : WDAY, YDAY, ISDST, TSTRCT, STATUS )
      PRINT *,'The values returned by PSX_LOCALTIME are:'
      PRINT *,'SEC   = ',SEC
      PRINT *,'MIN   = ',MIN
      PRINT *,'HOUR  = ',HOUR
      PRINT *,'DAY   = ',DAY
      PRINT *,'MONTH = ',MONTH
      PRINT *,'YEAR  = ',YEAR
      PRINT *,'WDAY  = ',WDAY
      PRINT *,'YDAY  = ',YDAY
      PRINT *,'ISDST = ',ISDST

* Test PSX_ASCTIME
      PRINT *,' '
      PRINT *,'--  Program PSX_TIME, function PSX_ASCTIME  --'
      PRINT *,' '

      CALL PSX_ASCTIME( TSTRCT, STRING, STATUS )
      PRINT *,'The value returned by PSX_ASCTIME is :'
      PRINT *,STRING

* Test PSX_TIME
      PRINT *,' '
      PRINT *,'--  Program PSX_TIME, function PSX_CTIME  --'
      PRINT *,' '

      CALL PSX_CTIME( NTICKS, STRING2, STATUS )
      PRINT *,'The value returned by PSX_CTIME is :'
      PRINT *,STRING2

* Test PSX_GMTIME
      PRINT *,' '
      PRINT *,'--  Program PSX_TIME, function PSX_GMTIME  --'
      PRINT *,' '

      CALL PSX_GMTIME( NTICKS, SEC, MIN, HOUR, DAY, MONTH, YEAR,
     : WDAY, YDAY, TSTRCT, STATUS )
      PRINT *,'The values returned by PSX_GMTIME are:'
      PRINT *,'SEC   = ',SEC
      PRINT *,'MIN   = ',MIN
      PRINT *,'HOUR  = ',HOUR
      PRINT *,'DAY   = ',DAY
      PRINT *,'MONTH = ',MONTH
      PRINT *,'YEAR  = ',YEAR
      PRINT *,'WDAY  = ',WDAY
      PRINT *,'YDAY  = ',YDAY

* Test PSX_ASCTIME
      PRINT *,' '
      PRINT *,'--  Program PSX_TIME, function PSX_ASCTIME  --'
      PRINT *,' '

      CALL PSX_ASCTIME( TSTRCT, STRING, STATUS )
      PRINT *,'The value returned by PSX_ASCTIME is :'
      PRINT *,STRING

      CALL EMS_END( STATUS )

      END
