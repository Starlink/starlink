      PROGRAM TEST_CHDIR
*+
*  Name:
*     TEST_CHDIR

*  Purpose:
*     Test the function PSX_CHDIR

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
      INCLUDE 'PSX_ERR'
      INTEGER STATUS

* Local Variables:
      CHARACTER *(256) CWD

*.
      PRINT *, ' '
      PRINT *, ' --  Program PSX_CHDIR, function PSX_CHDIR  -- '
      PRINT *, ' '

*     Look for this source code
      STATUS = SAI__OK

      CALL PSX_GETCWD( CWD, STATUS )

      IF (STATUS .EQ. SAI__OK) THEN
         PRINT *, 'Seemed to find CWD okay'
         CALL PSX_CHDIR( CWD, STATUS )
         IF (STATUS .EQ. SAI__OK) THEN
            PRINT *, 'CHDIR to CWD worked okay'
         ELSE
            PRINT *, 'Error changing to CWD'
         END IF
      ELSE
         PRINT *, 'Error obtaining CWD'
      END IF


      END
