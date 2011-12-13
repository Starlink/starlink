      PROGRAM TEST_RAND
*+
*  Name:
*     TEST_RAND

*  Purpose:
*     Test subroutines PSX_RAND and PSX_SRAND.

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
      INTEGER I, J, INUM, MAXNUM
      REAL FNUM

* Initialize STATUS.
      STATUS = SAI__OK

* Test PSX_RAND
      PRINT *,' '
      PRINT *,'--  Program PSX_RAND, function PSX_RAND  --'
      PRINT *,' '

      DO I = 1,10
         CALL PSX_RAND( INUM, MAXNUM, FNUM, STATUS )
         PRINT *,'INUM = ',INUM,'  MAXNUM = ',MAXNUM,'  FNUM = ',FNUM
      END DO

* Test PSX_SRAND
      PRINT *,' '
      PRINT *,'--  Program PSX_RAND, function PSX_SRAND  --'
      PRINT *,' '

      DO I = 1,5
         DO J = 1,3
            CALL PSX_SRAND( I, STATUS )
            CALL PSX_RAND( INUM, MAXNUM, FNUM, STATUS )
            PRINT *,'Seed = ',I,' Random number = ',INUM
         END DO
      END DO

      END
