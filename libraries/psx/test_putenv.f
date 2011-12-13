      PROGRAM TEST_PUTENV
*+
*  Name:
*     TEST_PUTENV

*  Purpose:
*     Test the subroutine PSX_PUTENV


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
      CHARACTER*32 RESULT1
      CHARACTER*32 RESULT2
      CHARACTER*32 RESULT3

* Initialize STATUS
      STATUS = SAI__OK

* Test PSX_PUTENV
      PRINT *,' '
      PRINT *,'--  Program TEST_PUTENV, function PSX_PUTENV  --'
      PRINT *,' '

* First set three environment variables
      CALL PSX_PUTENV( 'NAME1', 'VALUE1', STATUS )
      CALL PSX_PUTENV( 'NAME2', 'VALUE2', STATUS )
      CALL PSX_PUTENV( 'NAME3', 'VALUE3', STATUS )

      IF( STATUS .EQ.SAI__OK ) THEN
* Now get the values in reverse order
         CALL PSX_GETENV( 'NAME3', RESULT3, STATUS )
         CALL PSX_GETENV( 'NAME2', RESULT2, STATUS )
         CALL PSX_GETENV( 'NAME1', RESULT1, STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN
            IF( RESULT1 .NE. 'VALUE1' ) THEN
               PRINT *, 'ERROR: Variable NAME1 is ',RESULT1
            ELSE
               PRINT *, 'Variable NAME1 put and get OK'
            ENDIF
            IF( RESULT2 .NE. 'VALUE2' ) THEN
               PRINT *, 'ERROR: Variable NAME2 is ',RESULT2
            ELSE
               PRINT *, 'Variable NAME2 put and get OK'
            ENDIF
            IF( RESULT3 .NE. 'VALUE3' ) THEN
               PRINT *, 'ERROR: Variable NAME3 is ',RESULT3
            ELSE
               PRINT *, 'Variable NAME3 put and get OK'
            ENDIF

         ELSE
            PRINT *, 'Failed getting environment variables'
            PRINT *, 'Return status = ',status

         ENDIF

      ELSE
         PRINT *, 'Failed setting environment variables'
         PRINT *, 'Return status = ',status
      ENDIF
      END
