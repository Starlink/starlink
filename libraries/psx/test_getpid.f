      PROGRAM TEST_GETPID
*+
*  Name:
*     TEST_GETPID

*  Purpose:
*     Test the subroutines PSX_PID and PSX_PPID.

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
      INTEGER PID, PPID

*Initialize STATUS
      STATUS = SAI__OK

* Test PSX_GETPID
      PRINT *,' '
      PRINT *,'--  Program PSX_GETPID, function PSX_GETPID  --'
      PRINT *,' '

      CALL PSX_GETPID( PID, STATUS )
      PRINT *,'Process ID =        ',PID

* Test PSX_GETPID
      PRINT *,' '
      PRINT *,'--  Program PSX_GETPID, function PSX_GETPPID  --'
      PRINT *,' '

      CALL PSX_GETPPID( PPID, STATUS )
      PRINT *,'Parent process ID = ',PPID

      END
