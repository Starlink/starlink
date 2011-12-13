      PROGRAM TEST_TTYNAME
*+
*  Name:
*     TEST_TTYNAME

*  Purpose:
*     Test the functions PSX_ISATTY and PSX_TTYNAME.

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

* Local variables:
      INTEGER I                  ! Loop counter
      LOGICAL YES		 ! Is this a terminal?
      CHARACTER * ( 32 ) TNAME   ! Terminal name

* Initialize STATUS.
      STATUS = SAI__OK

* Do the test of PSX_ISATTY:
      PRINT *,' '
      PRINT *,'--  Program TEST_TERM. Testing PSX_ISATTY  --'
      PRINT *,' '
      DO I = -5,9
         CALL PSX_ISATTY( I, YES, STATUS )
         PRINT *,'File descriptor = ',I,', PSX_ISATTY returns ',YES
      END DO

* Do the test of PSX_TTYNAME:
      PRINT *,' '
      PRINT *,'--  Program TEST_TERM. Testing PSX_TTYNAME  --'
      PRINT *,' '
      DO I = -5,9
         CALL PSX_TTYNAME( I ,TNAME, STATUS )
         PRINT *,'File descriptor = ',I,', terminal name = ',TNAME
      END DO

      END
