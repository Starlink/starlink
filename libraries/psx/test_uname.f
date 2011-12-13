      PROGRAM TEST_UNAME
*+
*  Name:
*     TEST_UNAME

*  Purpose:
*     Test the function PSX_UNAME

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
      CHARACTER*32 SYSNAME
      CHARACTER*32 NODENAME
      CHARACTER*32 RELEASE
      CHARACTER*32 VERSION
      CHARACTER*32 MACHINE

* Initialize STATUS
      STATUS = SAI__OK

* Test PSX_UNAME
      PRINT *,' '
      PRINT *,'--  Program PSX_UNAME, function PSX_UNAME  --'
      PRINT *,' '

      CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE,
     :   STATUS )
      PRINT *,'Sysname = ',SYSNAME
      PRINT *,'Nodename= ',NODENAME
      PRINT *,'Release = ',RELEASE
      PRINT *,'Version = ',VERSION
      PRINT *,'Machine = ',MACHINE
      PRINT *,'Return status = ',STATUS

      END
