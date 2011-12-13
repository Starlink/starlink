      PROGRAM TEST_REMOVE
*+
*  Name:
*     TEST_REMOVE

*  Purpose:
*     Test the function PSX_REMOVE

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
      CHARACTER *(12) TEMPFILE

*.

      PRINT *, ' '
      PRINT *, ' --  Program PSX_REMOVE, function PSX_REMOVE  -- '
      PRINT *, ' '


* Initialize STATUS
      STATUS = SAI__OK
      CALL EMS_BEGIN( STATUS )

* Test PSX_REMOVE on a file that does not exist
      CALL PSX_REMOVE( 'xxx--xxx--xxxx-ttt', STATUS )

      IF (STATUS .NE. SAI__OK) THEN
         PRINT *,'Attempt to remove non-existant file failed correctly'
         CALL EMS_ANNUL( STATUS )
      ELSE
         PRINT *,'Attempt to remove non-existant file succeeded!!'
      ENDIF

* Create a new file
      TEMPFILE = 'xxx-xxx.dat'
      OPEN ( UNIT=20, FILE=TEMPFILE )
      CALL PSX_REMOVE( TEMPFILE, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         PRINT *, 'Successfully removed temp file'
      END IF


      CALL EMS_END( STATUS )

      END
