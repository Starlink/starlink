      PROGRAM TEST_ACCESS
*+
*  Name:
*     TEST_ACCESS

*  Purpose:
*     Test the function PSX_ACCESS

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
      CHARACTER *(32) FILE
      LOGICAL ACCESS
      INTEGER REASON

*.
      FILE = 'test_access.f'

      PRINT *, ' '
      PRINT *, ' --  Program TEST_ACCESS, function PSX_ACCESS  -- '
      PRINT *, ' '

*     Look for this source code
      STATUS = SAI__OK
      CALL PSX_ACCESS( FILE, ' ', ACCESS, REASON, STATUS )
      IF ( ACCESS ) THEN
         PRINT *, 'Correctly found source code ',FILE
      ELSE
         PRINT *, 'Error finding source file: ', FILE
         PRINT *, 'Reason = ', REASON
      END IF


      CALL PSX_ACCESS( FILE, 'R', ACCESS, REASON, STATUS )
      IF ( ACCESS ) THEN
         PRINT *, 'Correctly found readable source code ',FILE
      ELSE
         PRINT *, 'Error finding readable source file: ', FILE
         PRINT *, 'Reason = ', REASON
      END IF


      CALL PSX_ACCESS( FILE, 'X', ACCESS, REASON, STATUS )
      IF ( .NOT. ACCESS ) THEN
         PRINT *, 'Correctly did not find executable source code ',FILE
      ELSE
         PRINT *, 'Error! Found executable source file: ', FILE
      END IF

      END
